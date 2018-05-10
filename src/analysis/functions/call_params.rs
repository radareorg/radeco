//! Analyzes which registers are read by each function

use frontend::imports::ImportInfo;
use frontend::radeco_containers::RadecoModule;
use middle::dce;
use middle::ir;
use middle::regfile::SubRegisterFile;
use middle::ssa::cfg_traits::*;
use middle::ssa::ssa_traits::*;
use middle::ssa::ssastorage::SSAStorage;

use petgraph::prelude::*;
use petgraph::visit::{DfsPostOrder, VisitMap, Walker};

use std::collections::{hash_map, HashMap, HashSet};
use std::iter::{Extend, FromIterator};

/// For every function, record which registers it reads and which registers it
/// preserves.
///
/// This analysis is super conservative; for example, if a function preserves a
/// register by pushing it onto the stack and popping it back right before
/// returning, it is considered to be read and not preserved because we can't
/// guarantee that that stack location is never subsequently read or modified.
pub fn go(rmod: &mut RadecoModule) -> () {
    let mut a = Analyzer::new();
    a.go(rmod);
    for (addr, cc) in &a.call_convs {
        let rfn = &rmod.functions[addr];
        eprintln!("{:?} (@ {:#X}):", rfn.name, rfn.offset);
        eprintln!("  reads: {:?}", cc.reads);
        eprintln!("  preserves: {:?}", cc.preserves);
    }
}

// TODO: index in SubRegisterFile?
type Reg = String;

#[derive(Debug, Clone)]
struct CallingConvention {
    /// (potential) parameters
    reads: HashSet<Reg>,
    /// Callee-saved registers
    preserves: HashSet<Reg>,
}

impl CallingConvention {
    fn is_compatible_with(&self, other: &CallingConvention) -> bool {
        self.reads.is_subset(&other.reads) && self.preserves.is_superset(&other.preserves)
    }
}

struct Analyzer {
    /// Map from function addresses to their calling convention
    call_convs: HashMap<u64, CallingConvention>,
}

impl Analyzer {
    fn new() -> Analyzer {
        Analyzer {
            call_convs: HashMap::new(),
        }
    }

    /// Calls `patch_fn`, `dce::collect`, and `analyze_fn` on every function,
    /// callees first
    fn go(&mut self, rmod: &mut RadecoModule) -> () {
        // for imports, *ASSUME* that the callconv that r2 says is correct
        self.call_convs
            .extend(rmod.imports.iter().filter_map(|(&imp_addr, imp_info)| {
                let imp_rfn = &*imp_info.rfn.borrow();
                // ignore imports without callconvs
                let imp_args = imp_rfn.callconv.as_ref()?.args.as_ref()?;
                let reads: HashSet<_> = imp_args.iter().cloned().collect();
                // r2 doesn't tell us what registers are preserved
                let preserves = callconv_name_to_preserved_set(&imp_rfn.callconv_name);
                Some((imp_addr, CallingConvention { reads, preserves }))
            }));

        let mut dfs_wi = DfsPostOrder::empty(&rmod.callgraph).iter(&rmod.callgraph);
        // pick a function ...
        for fn_ni in rmod.callgraph.node_indices() {
            // ... and start a dfs on it
            dfs_wi.inner_mut().move_to(fn_ni);
            while let Some(fn_to_anal) = dfs_wi.next() {
                let fn_addr = rmod.callgraph[fn_to_anal];

                // ignore functions already in `call_convs` (probably because its an import)
                if !self.call_convs.contains_key(&fn_addr) {
                    let rfn = &mut rmod.functions.get_mut(&fn_addr).unwrap();

                    radeco_trace!("patching calls in fn: {}", rfn.name);
                    self.patch_fn(rfn.ssa_mut());

                    dce::collect(rfn.ssa_mut());
                    // TODO: inst_combine

                    radeco_trace!("analyzing fn: {}", rfn.name);
                    let cc = self.analyze_fn(rfn.ssa()).unwrap_or_else(|| {
                        radeco_err!("Failed to analyze fn: {:?} (@ {:#X})", rfn.name, fn_addr);
                        // if analysis failed, default to "reads and clobbers everything"
                        CallingConvention {
                            reads: HashSet::from_iter(rfn.ssa().regnames.iter().cloned()),
                            preserves: HashSet::new(),
                        }
                    });

                    self.call_convs.insert(fn_addr, cc);
                }
            }
        }
    }

    /// Using the callconv info we've gathered so far, patch-up call sites to
    /// to remove arguments that the callee doesn't read and make values in
    /// callee-saved registers be preserved across the call.
    fn patch_fn(&self, ssa: &mut SSAStorage) -> () {
        for node in ssa.inorder_walk() {
            if let Ok(NodeType::Op(ir::MOpcode::OpCall)) = ssa.node_data(node).map(|nd| nd.nt) {
                self.patch_call_node(ssa, node).unwrap_or_else(|| {
                    radeco_trace!(
                        "failed to remove unused args for call at {:#X}",
                        ssa.address(node).unwrap()
                    );
                });
            }
        }
    }

    fn patch_call_node(
        &self,
        ssa: &mut SSAStorage,
        call_node: <SSAStorage as SSA>::ValueRef,
    ) -> Option<()> {
        let (call_tgt_addr, call_reg_map) = direct_call_info(ssa, call_node)?; // bail on indirect or weird call
        let cc = self.call_convs.get(&call_tgt_addr)?; // bail if we have no info about the callee

        // remove unread args
        for (reg_idx, &op_node) in (0..).zip(&call_reg_map) {
            if !cc.reads.contains(reg_idx_to_name(ssa, reg_idx)) {
                ssa.op_unuse(call_node, op_node);
            }
        }

        // bridge preserved registers
        for use_node in ssa.uses_of(call_node) {
            // TODO: `ssa.registers(use_node)` might be the right thing to use,
            //       but it sometimes returns two registers which makes no sense
            //       :shrug:
            let reg_comment = ssa.comment(use_node).expect("use of call wasn't comment");
            let reg_name = reg_comment
                .split('@')
                .next()
                .expect("invalid reg comment format");
            let reg_idx = ssa.regnames
                .iter()
                .position(|x| x == reg_name)
                .expect("invalid reg name");
            if cc.preserves.contains(reg_name) {
                ssa.replace_value(use_node, call_reg_map[reg_idx]);
            }
        }

        Some(())
    }

    fn analyze_fn(&self, ssa: &SSAStorage) -> Option<CallingConvention> {
        let entry_regstate_node = ssa.registers_in(ssa.entry_node()?)?;
        let exit_regstate_node = ssa.registers_in(ssa.exit_node()?)?;
        // some registers may not be present in the entry node;
        // this means that the function neither reads nor preserves that register
        let entry_regstate: Vec<Option<_>> = register_state_info(ssa, entry_regstate_node);
        // every register must be present in the exit node though
        let exit_regstate: Vec<_> = register_state_info(ssa, exit_regstate_node)
            .into_iter()
            .collect::<Option<_>>()?;

        let mut reads = HashSet::new();
        let mut preserves = HashSet::new();

        // ignore registers not in entry regstate
        let mut regstate_iter = (0..).zip(exit_regstate).zip(entry_regstate)
            .filter_map(|((i,x),on)| on.map(|n| (i, n, x)));

        for (i, reg_val_entry, reg_val_exit) in regstate_iter {
            if reg_val_exit == reg_val_entry {
                preserves.insert(reg_idx_to_name(ssa, i as u8).to_owned());
            }

            // find all uses, ignoring entry/exit register state
            let mut uses_iter = ssa.uses_of(reg_val_entry)
                .into_iter()
                .filter(|&n| n != entry_regstate_node && n != exit_regstate_node);
            if uses_iter.next().is_some() {
                reads.insert(reg_idx_to_name(ssa, i as u8).to_owned());
            }
        }

        Some(CallingConvention { reads, preserves })
    }
}

/// Extracts the value of all registers at a `RegisterState` SSA node.
/// The length of the returned `Vec` is exactly `ssa.regnames.len() + 1`.
fn register_state_info(
    ssa: &SSAStorage,
    regstate_node: <SSAStorage as SSA>::ValueRef,
) -> Vec<Option<<SSAStorage as SSA>::ValueRef>> {
    let mut reg_opt_map: Vec<Option<_>> = vec![None; ssa.regnames.len() + 1];
    for (op_idx, op_node) in ssa.sparse_operands_of(regstate_node) {
        if let Some(p) = reg_opt_map.get_mut(op_idx as usize) {
            *p = Some(op_node);
        }
    }
    reg_opt_map
}

/// Extracts the call target address and the value of all registers
/// The length of the returned `Vec` is exactly `ssa.regnames.len()`.
/// Returns `None` if the call is indirect or if not all registers have a value.
fn direct_call_info(
    ssa: &SSAStorage,
    call_node: <SSAStorage as SSA>::ValueRef,
) -> Option<(u64, Vec<<SSAStorage as SSA>::ValueRef>)> {
    let mut tgt_opt: Option<u64> = None;
    let mut reg_opt_map: Vec<Option<_>> = vec![None; ssa.regnames.len()];

    for (op_idx, op_node) in ssa.sparse_operands_of(call_node) {
        if op_idx == 0 {
            tgt_opt = tgt_opt.or_else(|| ssa.constant(op_node));
        } else {
            if let Some(p) = reg_opt_map.get_mut((op_idx - 1) as usize) {
                *p = Some(op_node);
            }
        }
    }

    let reg_map_opt: Option<Vec<_>> = reg_opt_map.into_iter().collect();
    Some((tgt_opt?, reg_map_opt?))
}

fn reg_idx_to_name(ssa: &SSAStorage, reg_idx: u8) -> &str {
    ssa.regnames
        .get(reg_idx as usize)
        .map_or("mem", String::as_str)
}

// TODO: if r2 ever starts keeping track of preserved registers, use that instead of this
/// For a given named calling convention, return the set of registers it
/// preserves across calls (are callee-saved).
///
/// This should only be used for imported functions that we can't analyze to
/// find a more specific calling convention.
#[cfg_attr(rustfmt, rustfmt_skip)]
fn callconv_name_to_preserved_set(cc_name: &str) -> HashSet<String> {
    // see https://github.com/radare/radare2/tree/master/libr/anal/d
    // for what `cc_name` can be
    let reg_slice: &[_] = match cc_name {
        // --- x86[_64] ---
        // standard for SysV-compatible systems (most modern Unixes)
        // https://github.com/hjl-tools/x86-psABI/wiki/X86-psABI
        "amd64" => &["rbx", "rsp", "rbp", "r12", "r13", "r14", "r15"],
        "cdecl" => &["ebx", "esp", "ebp", "esi", "edi"],

        // standard for Windows
        // https://en.wikipedia.org/wiki/X86_calling_conventions and https://llvm.org/viewvc/llvm-project/llvm/trunk/lib/Target/X86/X86CallingConv.td?view=markup#l1047
        "ms" => &["rbx", "rsp", "rbp", "rsi", "rdi", "r12", "r13", "r14", "r15"],
        "stdcall" => &["ebx", "ebp", "esi", "edi"],

        // --- ARM ---
        // https://developer.arm.com/docs/ihi0042/latest
        "arm32" => &["r4", "r5", "r6", "r7", "r8", "r10", "r11", "sp"],
        // https://developer.arm.com/docs/ihi0055/latest
        "arm64" => &["x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "fp", "sp"],

        // if we don't recognize `cc_name`, assume all registers are clobbered
        _ => &[],
    };
    reg_slice.into_iter().map(|&x| x.to_owned()).collect()
}
