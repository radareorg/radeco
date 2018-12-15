//! Fixes the call target for all call sites in the SSA

use frontend::radeco_containers::*;
use middle::ir;
use middle::ssa::ssa_traits::*;
use middle::ssa::ssastorage::SSAStorage;

use std::collections::HashMap;

/// For every [`OpCall`] SSA node in every function, try to find that call
/// site's corresponding edge in [the callgraph] and replace the "target"
/// operand of the SSA node with a constant value for the address of the actual
/// call target.
///
/// [`OpCall`]: ir::MOpcode::OpCall
/// [the callgraph]: RadecoModule::callgraph
pub fn go(rmod: &mut RadecoModule) -> () {
    for rfun in rmod.functions.values_mut() {
        go_fn(rfun, &rmod.callgraph);
    }
}

fn go_fn(rfun: &mut RadecoFunction, callgraph: &CallGraph) -> () {
    let fn_addr = rfun.offset;
    let call_site_addr_to_target_addr: HashMap<u64, u64> = callgraph
        .callees(rfun.cgid())
        .map(|(cs_a, tgt_idx)| (cs_a, callgraph[tgt_idx]))
        .collect();
    let ssa = rfun.ssa_mut();
    for node in ssa.inorder_walk() {
        if let Ok(NodeType::Op(ir::MOpcode::OpCall)) = ssa.node_data(node).map(|x| x.nt) {
            fix_call_site(ssa, node, &call_site_addr_to_target_addr).unwrap_or_else(|| {
                radeco_err!(
                    "failed to fix call site {:?} in function at {:#X}",
                    node,
                    fn_addr
                )
            });
        }
    }
}

fn fix_call_site(
    ssa: &mut SSAStorage,
    call_node: <SSAStorage as SSA>::ValueRef,
    fn_call_map: &HashMap<u64, u64>,
) -> Option<()> {
    let call_site_addr = ssa.address(call_node)?.address;
    if let Some(&call_target_addr) = fn_call_map.get(&call_site_addr) {
        let old_opcall_tgt_node = ssa
            .sparse_operands_of(call_node)
            .iter()
            .find(|x| x.0 == 0)?
            .1;
        let new_opcall_tgt_node = ssa.insert_const(call_target_addr, None)?;
        ssa.op_unuse(call_node, old_opcall_tgt_node);
        ssa.op_use(call_node, 0, new_opcall_tgt_node);
    } else {
        radeco_trace!(
            "call site at {:#X} isn't in call graph; perhaps the call is indirect?",
            call_site_addr
        );
    }
    Some(())
}
