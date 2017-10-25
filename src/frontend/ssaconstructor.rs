//! This module uses the SSA Methods defined to contstruct the SSA form
//! straight from raw esil


// NB:
// There are some limitations to the current ESIL parser and these may/must be
// improved in the
// further commits.
// 1. We only parse ESIL that is "well formed", that is, it cannot have any
// statements after an if.
// For example: "zf,?{,0x80,rip,=,}" is a valid esil statement as it does not
// have any
// instructions after "}" in the same instruction.

use esil::lexer::{Token, Tokenizer};

use esil::parser::{Parse, Parser};
use frontend::instruction_analyzer::{InstructionAnalyzer, X86_CS_IA, IOperand};
use frontend::radeco_containers::RadecoFunction;

use middle::ir::{self, MAddress, MOpcode};
use middle::phiplacement::PhiPlacer;
use middle::regfile::SubRegisterFile;
use middle::ssa::graph_traits::Graph;
use middle::ssa::ssa_traits::{SSAExtra, SSAMod, ValueInfo};
use petgraph::graph::NodeIndex;

use r2api::structs::{LOpInfo, LRegInfo};

use regex::Regex;
use std::{fmt, cmp, u64};
use std::collections::HashMap;

pub type VarId = usize;

const FALSE_EDGE: u8 = 0;
const TRUE_EDGE: u8 = 1;
const UNCOND_EDGE: u8 = 2;

pub struct SSAConstruct<'a, T>
    where T: 'a + Clone + fmt::Debug + SSAExtra + SSAMod<BBInfo = MAddress,
                    ActionRef = <T as Graph>::GraphNodeRef,
                    CFEdgeRef = <T as Graph>::GraphEdgeRef>
{
    phiplacer: PhiPlacer<'a, T>,
    regfile: SubRegisterFile,
    intermediates: Vec<T::ValueRef>,
    // TODO: Combine this information with the registers field.
    // even better if this is done at the r2pipe level and we expose API to get
    // alias information.
    alias_info: HashMap<String, String>,
    ident_map: HashMap<String, u64>,
    // Used to keep track of esil if-else. The reference to the ITE node and the address of this
    // instruction.
    nesting: Vec<(T::ValueRef, MAddress)>,
    // Used to keep track of the offset within an instruction.
    instruction_offset: u64,
    needs_new_block: bool,
    mem_id: u64,
}

impl<'a, T> SSAConstruct<'a, T>
    where T: 'a + Clone + fmt::Debug + SSAExtra + SSAMod<BBInfo = MAddress,
                    ActionRef = <T as Graph>::GraphNodeRef,
                    CFEdgeRef = <T as Graph>::GraphEdgeRef>
{
    pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruct<'a, T> {
        let regfile = SubRegisterFile::new(reg_info);
        let mut sc = SSAConstruct {
            phiplacer: PhiPlacer::new(ssa, regfile.clone()),
            regfile: regfile,
            intermediates: Vec::new(),
            alias_info: HashMap::new(),
            ident_map: HashMap::new(),
            nesting: Vec::new(),
            instruction_offset: 0,
            needs_new_block: true,
            mem_id: 0,
        };

        {
            // Populate the alias information from reg_info
            let alias_info = &mut sc.alias_info;
            for register in &reg_info.alias_info {
                alias_info.insert(register.reg.clone(), register.role_str.clone());
            }
        }

        {
            let identmap = &mut sc.ident_map;
            for register in &reg_info.reg_info {
                identmap.insert(register.name.clone(), register.size as u64);
            }
        }

        // Add all the registers to the variable list.
        sc.phiplacer.add_variables(sc.regfile.whole_registers.clone());
        // Add a new variable for "memory".
        sc.phiplacer.add_variables(vec![ValueInfo::new_scalar(ir::WidthSpec::Known(0))]);
        sc
    }

    // Helper wrapper.
    pub fn construct(rfn: &mut RadecoFunction, ri: &LRegInfo) {
        let instructions = rfn.instructions().to_vec();
        let mut constr = SSAConstruct::new(rfn.ssa_mut(), &ri);
        constr.run(instructions.as_slice());
    }

    fn set_mem_id(&mut self, id: u64) {
        assert_eq!(self.mem_id, 0);
        self.mem_id = id;
    }

    fn mem_id(&self) -> u64 {
        assert_ne!(self.mem_id, 0);
        self.mem_id
    }

    // If the operand is a Token::Identifier, it has to be a register.
    // This is because we never push in a temporary that we create as a
    // Token::Identifier and all ESIL identifiers must be a valid register.
    // It will remain as a Token::Identifier only the first time the parser
    // encounters
    // it, we always push in a Token::Register or a Token::Intermediate.
    fn process_in(&mut self, var: &Option<Token>, address: &mut MAddress) -> Option<T::ValueRef> {
        if var.is_none() {
            return None;
        }
        let ret = match *var.as_ref().expect("This cannot be `None`") {
            // Since ESIL has no concept of intermediates, the identifier spotted by parser
            // has to be a register.
            Token::ERegister(ref name) |
            Token::EIdentifier(ref name) => self.phiplacer.read_register(address, name),
            // We arrive at this case only when we have popped an operand that we have pushed
            // into the parser. id refers to the id of the var in our intermediates table.
            Token::EEntry(ref id, _) => {
                *self.intermediates.get(*id).expect("This cannot return `None`")
            }
            Token::EConstant(value) => {
                // Add or retrieve a constant with the value from the table.
                self.phiplacer.add_const(address, value, None)
            }
            Token::EAddress => {
                // Treat this as retrieving a constant.
                let value = address.address;
                self.phiplacer.add_const(address, value, None)
            }
            _ => {
                panic!("SSAConstruct Error: Found something other than a Var as an operand to an \
                        instruction!")
            }
        };
        Some(ret)
    }

    fn process_out(&mut self, result: Option<T::ValueRef>, _: MAddress) -> Option<Token> {
        // NB 1: Process out is defined for any operation, not only equal as before.
        // Hence, here we should give result a new entry in the "intermediates" table
        // and return a
        // Token::EEntry. This will subsequently be pushed onto the parser stack.
        // If in some case (as in case of '=') we should not push any token onto the
        // parser stack,
        // this function should return `None`.
        // NB 2: We never write to an intermediate twice as this is an SSA form!
        if let Some(ref res) = result {
            self.intermediates.push(*res);
            let result_id = self.intermediates.len() - 1;
            let out_size = self.phiplacer.operand_width(res);
            Some(Token::EEntry(result_id, Some(out_size as u64)))
        } else {
            None
        }
    }


    fn process_op(&mut self,
                  token: &Token,
                  address: &mut MAddress,
                  operands: &[Option<Token>; 2])
                  -> Option<T::ValueRef> {
        // This is where the real transformation from ESIL to radeco IL happens. This
        // method choose the opcodes to translate from ESIL to radecoIL and also
        // handles assignments
        // and jumps as these are cases that need to be handled a bit differently from
        // the rest of the opcodes.
        let mut lhs = self.process_in(&operands[0], address);
        let mut rhs = self.process_in(&operands[1], address);

        self.phiplacer.narrow_const_operand(address, &mut lhs, &mut rhs);

        // Check if the two operands are of compatible sizes for compare
        let lhs_size = lhs.map_or(0, |i| self.phiplacer.operand_width(&i));
        let rhs_size = rhs.map_or(0, |i| self.phiplacer.operand_width(&i));

        let result_size = cmp::max(lhs_size, rhs_size);

        // Get the radeco Opcode and the output width.
        let (op, vt) = match *token {
            Token::ECmp => {
                let op = MOpcode::OpSub;
                let vt = ValueInfo::new_scalar(ir::WidthSpec::Known(result_size));
                (op, vt)
            }
            Token::ELt => (MOpcode::OpLt, ValueInfo::new_scalar(ir::WidthSpec::Known(1))),
            Token::EGt => (MOpcode::OpGt, ValueInfo::new_scalar(ir::WidthSpec::Known(1))),
            Token::EEq => {
                // This case is the only one that performs a write_register call. Since all
                // assignements in ESIL are only possible to registers, it is reasonable to
                // panic
                // if the register is invalid or not found.
                // If the register being written into is "PC" then we emit a jump (jmp) instead
                // of an assignment.
                if let Some(Token::EIdentifier(ref name)) = operands[0] {
                    if Some("PC".to_owned()) == self.alias_info.get(name).cloned() {
                        // There is a possibility that the jump target is not a constant and we
                        // don't have enough information right now to resolve this target. In this
                        // case, we add a new block and label it unresolved. This maybe resolved as
                        // a part of some other analysis. Right now, the only targets we can
                        // determine are the ones where the rhs is a constant.
                        if let Some(Token::EConstant(target)) = operands[1] {
                            // Direct/known CF tranfer
                            let target_addr = MAddress::new(target, 0);
                            self.phiplacer
                                .add_block(target_addr, Some(*address), Some(UNCOND_EDGE));
                            self.needs_new_block = true;
                        } else {
                            // Indirect CF transfer
                            if let Some(ref jump_idx) = rhs {
                                self.phiplacer.add_indirect_cf(jump_idx, address, UNCOND_EDGE);
                                // Next instruction should begin in a new block
                                self.needs_new_block = true;
                            } else {
                                radeco_warn!("Found a indirect jump without a corresponding \
                                              target expression");
                            }
                        }
                    } else {
                        // We are writing into a register.
                        self.phiplacer.write_register(address,
                                                      name,
                                                      rhs.expect("rhs for EEq cannot be `None`"));
                    }
                } else {
                    // This means that we're performing a memory write. So we need to emit an
                    // OpStore operation.
                    radeco_trace!("Memory Write");
                    let op_node = self.phiplacer
                        .add_op(&MOpcode::OpStore,
                                address,
                                ValueInfo::new_scalar(ir::WidthSpec::Known(0)));
                    self.phiplacer.op_use(&op_node,
                                          0,
                                          lhs.as_ref()
                                              .expect("`addr` of `MemoryWrite` cannot be `None`"));
                    self.phiplacer.op_use(&op_node,
                                          1,
                                          rhs.as_ref()
                                              .expect("`value` of `MemoryWrite` cannot be `None`"));
                }
                return None;
            }
            // Returns None.
            Token::EIf => {
                // Create a new block for true.
                // The next instruction must be a part of the true block, unless we see an "{"
                let op_node = self.phiplacer.add_op(&MOpcode::OpITE,
                                                    address,
                                                    ValueInfo::new_scalar(ir::WidthSpec::Known(1)));
                self.nesting.push((op_node, *address));
                // Though this is a ternary operator, the other two operands are actually
                // represented throught the control flow edges of the block to which ITE belongs
                // to. For clarity, we will add comments to show the same.
                // Hence: 0 -> compare statement. 1 -> T. 2 -> F.
                let true_address = MAddress::new(address.address, address.offset + 1);
                let _true_block = self.phiplacer
                    .add_block(true_address, Some(*address), Some(TRUE_EDGE));
                let true_comment = self.phiplacer
                    .add_comment(*address, scalar!(0), format!("T: {}", true_address));
                self.phiplacer.op_use(&op_node, 0, lhs.as_ref().expect("lhs cannot be `None`"));
                self.phiplacer.op_use(&op_node, 1, &true_comment);
                return None;
            }
            Token::ELsl => {
                (MOpcode::OpLsl, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::ELsr => {
                (MOpcode::OpLsr, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::ERor => {
                (MOpcode::OpRor, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::ERol => {
                (MOpcode::OpRol, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::EAnd => {
                (MOpcode::OpAnd, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::EOr => {
                (MOpcode::OpOr, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::ENeg => {
                (MOpcode::OpNot, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::EMul => {
                (MOpcode::OpMul, ValueInfo::new_scalar(ir::WidthSpec::from(result_size)))
            }
            Token::EXor => {
                (MOpcode::OpXor, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::EAdd => {
                (MOpcode::OpAdd, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::ESub => {
                (MOpcode::OpSub, ValueInfo::new_unresolved(ir::WidthSpec::from(result_size)))
            }
            Token::EDiv => {
                (MOpcode::OpDiv, ValueInfo::new_scalar(ir::WidthSpec::from(result_size)))
            }
            Token::EMod => {
                (MOpcode::OpMod, ValueInfo::new_scalar(ir::WidthSpec::from(result_size)))
            }
            Token::EPoke(_) => {
                // TODO: rhs has to be cast to size 'n' if it's size is not already n.
                let mem_id = self.mem_id();
                let mem = self.phiplacer.read_variable(address, mem_id);
                let op_node = self.phiplacer.add_op(&MOpcode::OpStore, address, scalar!(0));

                self.phiplacer.op_use(&op_node, 0, &mem);
                self.phiplacer.op_use(&op_node, 1, lhs.as_ref().expect("lhs cannot be `None`"));
                self.phiplacer.op_use(&op_node, 2, rhs.as_ref().expect("rhs cannot be `None`"));

                self.phiplacer.write_variable(*address, self.mem_id, op_node);
                return None;
            }
            Token::EPeek(n) => {
                let mem_id = self.mem_id();
                let mem = self.phiplacer.read_variable(address, self.mem_id);
                let op_node = self.phiplacer
                    .add_op(&MOpcode::OpLoad,
                            address,
                            ValueInfo::new_unresolved(ir::WidthSpec::from(n as u16)));

                self.phiplacer.op_use(&op_node, 0, &mem);
                self.phiplacer.op_use(&op_node, 1, lhs.as_ref().expect("lhs cannot be `None`"));
                return Some(op_node);
            }
            Token::EPop => unreachable!(),
            Token::EGoto => unimplemented!(),
            Token::EBreak => unimplemented!(),
            Token::EEndIf | Token::ENop => {
                return None;
            }
            // Anything else is considered invalid. Log this as a warning and move on.
            // We may not
            // want to panic here as we can still achieve a reasonable decompilation
            // missing just
            // one or two instructions.
            _ => {
                unimplemented!();
            }
        };

        // Insert `widen` cast of the two are not of same size and rhs is_some.
        if rhs.is_some() {
            let (lhs, rhs) = match lhs_size.cmp(&rhs_size) {
                cmp::Ordering::Greater => {
                    let vt = ValueInfo::new_unresolved(ir::WidthSpec::from(lhs_size));
                    let casted_rhs = self.phiplacer
                        .add_op(&MOpcode::OpZeroExt(lhs_size), address, vt);
                    self.phiplacer.op_use(&casted_rhs, 0, rhs.as_ref().expect(""));
                    self.phiplacer.propagate_reginfo(&casted_rhs);
                    (lhs.expect("lhs cannot be `None`"), casted_rhs)
                }
                cmp::Ordering::Less => {
                    let vt = ValueInfo::new_unresolved(ir::WidthSpec::from(rhs_size));
                    let casted_lhs = self.phiplacer
                        .add_op(&MOpcode::OpZeroExt(rhs_size), address, vt);
                    self.phiplacer
                        .op_use(&casted_lhs, 0, lhs.as_ref().expect("lhs cannot be `None`"));
                    self.phiplacer.propagate_reginfo(&casted_lhs);
                    (casted_lhs, rhs.expect(""))
                }
                cmp::Ordering::Equal => (lhs.expect(""), rhs.expect("")),
            };
            let op_node_ = self.phiplacer.add_op(&op, address, vt);
            self.phiplacer.op_use(&op_node_, 0, &lhs);
            self.phiplacer.op_use(&op_node_, 1, &rhs);
            Some(op_node_)
        } else {
            // There is only one operand, that is lhs. No need for cast.
            let op_node_ = self.phiplacer.add_op(&op, address, vt);
            self.phiplacer.op_use(&op_node_, 0, lhs.as_ref().expect(""));
            Some(op_node_)
        }
    }

    fn init_blocks(&mut self) {
        // Create a start block with all registers as variables defined in this block.
        // Seal this block as the start block cannot have any more successors.
        // Create another block and return as "current_block" that we are processing.
        let start_address = MAddress::new(0, 0);
        let start_block = self.phiplacer.add_block(start_address, None, None);

        self.phiplacer.mark_entry_node(&start_block);

        for (i, name) in self.regfile.whole_names.iter().enumerate() {
            let reg = self.regfile.whole_registers.get(i).expect("This cannot be `None`");
            // Name the newly created nodes with register names.
            let argnode = self.phiplacer.add_comment(start_address, *reg, name.clone());
            self.phiplacer.write_variable(start_address, i as u64, argnode);
        }

        {
            // Insert "mem" pseudo variable
            let reglen = self.regfile.whole_names.len();
            self.set_mem_id(reglen as u64);
            let mem_comment = self.phiplacer
                .add_comment(start_address, scalar!(0), "mem".to_owned());
            let mem_id = self.mem_id();
            self.phiplacer.write_variable(start_address, mem_id, mem_comment);
        }

        self.phiplacer.sync_register_state(start_block);
        // Add the exit block
        let exit_block = self.phiplacer.add_dynamic();
        self.phiplacer.mark_exit_node(&exit_block);
    }

    // For now, some other component provides SSAConstruct with the instructions
    // that it is supposed to convert into SSA. SSAConstruct does not care from
    // where this
    // ESIL is received, it merely takes this vector of ESIL strings and transforms
    // it into its SSA
    // form.
    pub fn run(&mut self, op_info: &[LOpInfo]) {
        let mut p = Parser::init(Some(self.ident_map.clone()), Some(64));
        let mut current_address = MAddress::new(0, 0);
        self.init_blocks();
        for op in op_info {
            if op.esil.is_none() {
                continue;
            }

            let offset = op.offset.unwrap_or(0);

            // Get ESIL string
            let esil_str = if let Some(ref esil_str_) = op.esil {
                esil_str_
            } else {
                radeco_warn!("No ESIL string found at: {}", offset);
                continue;
            };

            // Reset the instruction offset and remake the current_address.
            // TODO: Improve this mechanism.
            self.instruction_offset = 0;
            let next_address = MAddress::new(offset, self.instruction_offset);
            if self.needs_new_block {
                self.needs_new_block = false;
                self.phiplacer.add_block(next_address, None, None);
            }

            current_address.offset = 0;
            self.phiplacer.maybe_add_edge(current_address, next_address);
            current_address = next_address;

            // If the nesting vector has a non zero length, then we need to make another
            // block and add connecting false edges, note that this is in accordance to the
            // assumption stated at the top of this file.
            while let Some(ref node) = self.nesting.pop() {
                let src_address = node.1;
                let src_node = &node.0;
                let false_comment = self.phiplacer
                    .add_comment(src_address, scalar!(0), format!("F: {}", current_address));
                self.phiplacer.add_block(current_address, Some(src_address), Some(FALSE_EDGE));
                self.phiplacer.op_use(src_node, 2, &false_comment);
            }

            radeco_trace!("ssa_construct_esil|{}|{:?}", current_address, esil_str);

            // Handle call separately.
            // NOTE: This is a hack.
            if let Some(ref ty) = op.optype {
                if ty == "call" {
                    let unknown_str = "unknown".to_owned();
                    let call_operand =
                        self.phiplacer.add_comment(current_address,
                                                   scalar!(0),
                                                   op.opcode.clone().unwrap_or(unknown_str));
                    let op_call = self.phiplacer
                        .add_op(&MOpcode::OpCall, &mut current_address, scalar!(0));
                    // Since we cannot reason about call, a safe assumption is that it reads and
                    // writes every register.
                    for (i, reg) in self.regfile.whole_names.iter().enumerate() {
                        let rnode = self.phiplacer.read_register(&mut current_address, reg);
                        self.phiplacer.op_use(&op_call, (i + 1) as u8, &rnode);
                        let new_register_comment = format!("{}@{}", reg, current_address);
                        let width = self.regfile
                            .whole_registers
                            .get(i)
                            .expect("Unable to find register with index");
                        let comment_node = self.phiplacer
                            .add_comment(current_address, *width, new_register_comment);
                        self.phiplacer.write_register(&mut current_address, reg, comment_node);
                        self.phiplacer.op_use(&comment_node, i as u8, &op_call);
                    }
                    current_address.offset += 1;
                    self.phiplacer.set_address(&op_call, current_address);
                    self.phiplacer.op_use(&op_call, 0, &call_operand);
                    continue;
                }
            }

            // Some overrides as we do not support all esil and don't want to panic.
            let overrides = &["GOTO", "TRAP", "$", "TODO", "REPEAT"];
            if esil_str.split(",").any(|x| overrides.contains(&x)) {
                // Do something else other than asking the parserS
                lazy_static! {
                    static ref OPRE: Regex = Regex::new(r"[[:xdigit:]][[:xdigit:]]")
                                                   .expect("Unable to compile regex");
                }

                let byte_str = op.bytes.as_ref().expect("Invalid bytes for instruction");
                let bytes = OPRE.captures_iter(byte_str.as_str())
                    .map(|cap| u8::from_str_radix(&cap[0], 16).expect("Cannot Fail"))
                    .collect::<Vec<u8>>();

                // TODO/XXX:
                // This needs to be selected based on the current arch
                let ia = X86_CS_IA::new(bytes).expect("Unable to instantiate IA");

                self.process_custom(&ia, &mut current_address);

                continue;
            }

            while let Some(ref token) = p.parse::<_, Tokenizer>(esil_str) {
                radeco_trace!("ssa_construct_token|{}|{:?}", current_address, token);
                let (lhs, rhs) = p.fetch_operands(token);
                // Determine what to do with the operands and get the result.
                let result = self.process_op(token, &mut current_address, &[lhs, rhs]);
                if let Some(result_) = self.process_out(result, current_address) {
                    p.push(result_);
                }
                current_address.offset += 1;
            }
        }
        // BUG: The last block may not have the biggest address, which means current_address
        // may be not in the last basic block
        // self.phiplacer.add_edge(current_address, MAddress::new(u64::MAX, 0), UNCOND_EDGE);
        self.phiplacer.gather_exits();
        self.phiplacer.finish();
    }

    fn process_memory_op(&mut self,
                         base: &Option<String>,
                         index: &Option<String>,
                         scale: i32,
                         disp: i64,
                         addr: &mut MAddress) -> T::ValueRef {

        let base_node = if let Some(ref reg) = *base {
            self.process_in(&Some(Token::ERegister(reg.clone())), addr)
        } else {
            None
        };

        let index_node = if let Some(ref reg) = *index {
            // Mulitply the index by the scale and return the resulting node
            //    <index> '*' <scale>
            let reg_node = self.process_in(&Some(Token::ERegister(reg.clone())), addr).expect("Invalid op");
            // TODO: s/64/default op size/
            let vt = ValueInfo::new_scalar(ir::WidthSpec::Known(64));
            let mult_node = self.phiplacer.add_op(&MOpcode::OpMul, addr, vt);
            // Make a node for the scale
            let scale_node = self.phiplacer.add_const(addr, scale as u64, None);
            self.phiplacer.op_use(&mult_node, 0, &reg_node);
            self.phiplacer.op_use(&mult_node, 1, &scale_node);
            Some(mult_node)
        } else {
            None
        };

        let disp_node = if disp != 0 {
            Some(self.phiplacer.add_const(addr, disp as u64, None))
        } else {
            None
        };

        // Fold them into a single expression that describes the address
        let mut res = None;
        for nd in &[base_node, index_node, disp_node] {
            res = match (res, *nd) {
                (Some(ref o1), Some(ref o2)) => {
                    // Join by a '+'
                    let vt = ValueInfo::new_scalar(ir::WidthSpec::Known(64));
                    let add_node = self.phiplacer.add_op(&MOpcode::OpAdd, addr, vt);
                    self.phiplacer.op_use(&add_node, 0, o1);
                    self.phiplacer.op_use(&add_node, 1, o2);
                    Some(add_node)
                },
                (None, Some(ref o2)) => {
                    *nd
                },
                (_, _) => res,
            }
        }

        res.expect("Address is `None`")
    }


    fn process_custom<IA: InstructionAnalyzer>(&mut self, ia: &IA, addr: &mut MAddress) {
        let mnemonic = ia.mnemonic().clone();
        let opcode = MOpcode::OpCustom(mnemonic.into_owned());
        let vt = ValueInfo::new_unresolved(ir::WidthSpec::Unknown);

        let custom_opnode = self.phiplacer.add_op(&opcode, addr, vt);
        let mut opidx: u8 = 0;

        let reg_r = ia.registers_read().iter().map(|&x| if let &IOperand::Register(ref s) = x {
            let tok = Token::ERegister(s.clone());
            self.process_in(&Some(tok), addr)
        } else{
            None
        }).filter(|x| x.is_some()).collect::<Vec<_>>();

        // Use read registers as arguments
        for (i, reg) in reg_r.iter().enumerate() {
            if let &Some(ref reg_node) = reg {
                self.phiplacer.op_use(&custom_opnode, i as u8, reg_node);
            }
        }

        opidx = reg_r.len() as u8;

        let reg_w = ia.registers_written().iter().map(|&x| if let &IOperand::Register(ref s) = x {
            Some(s)
        } else{
            None
        }).filter(|x| x.is_some()).collect::<Vec<_>>();

        // Write out modified variables
        for reg in &reg_w {
            if let Some(ref rname) = *reg {
                self.phiplacer.write_register(addr, rname, custom_opnode);
            }
        }

        if ia.has_memory_operand() {
            // <addr> in represented in mem_op, need to extract the mib.
            // base + index*scale +- disp
            let (opcode, maddr) = if let Some(mem_op) = ia.memory_written() {
                // Insert a store
                let st = MOpcode::OpStore;
                let addr = if let &IOperand::Memory { ref base, ref index, ref scale, ref disp } = mem_op {
                    self.process_memory_op(base, index, *scale, *disp, addr)
                } else {
                    unreachable!()
                };
                (st, addr)
            } else if let Some(mem_op) = ia.memory_read() {
                // Insert load
                let ld = MOpcode::OpLoad;
                let addr = if let &IOperand::Memory { ref base, ref index, ref scale, ref disp } = mem_op {
                    self.process_memory_op(base, index, *scale, *disp, addr)
                } else {
                    unreachable!()
                };
                (ld, addr)
            } else {
                // Memory has to be a load or a store operation, cannot be anything else
                unreachable!()
            };

            let mem_op = self.phiplacer.add_op(&opcode, addr, vt);
            let mem_id = self.mem_id();
            let mem = self.phiplacer.read_variable(addr, self.mem_id);

            // Op[Load/Store](mem, addr)
            self.phiplacer.op_use(&mem_op, 0, &mem);
            self.phiplacer.op_use(&mem_op, 1, &maddr);
            
            // Do some additional handling, such as associating the written value,
            // creating new instance of memory etc.
            if opcode == MOpcode::OpStore {
                self.phiplacer.op_use(&mem_op, 2, &custom_opnode);
                // New instance of memory
                self.phiplacer.write_variable(*addr, self.mem_id, mem_op);
            } else {
                // Use memory load in the custom_opnode
                self.phiplacer.op_use(&custom_opnode, opidx, &mem_op);
            }
        }
    }

} // end impl SSAConstruct

#[cfg(test)]
mod test {
    use analysis::sccp;
    use middle::{dot, dce};
    use middle::ir_writer::IRWriter;
    use middle::ssa::ssastorage::SSAStorage;
    use r2api::structs::{LFunctionInfo, LRegInfo};
    use serde_json;
    use std::fs::File;
    use std::io::prelude::*;
    use super::*;


    const REGISTER_PROFILE: &'static str = "test_files/x86_register_profile.json";

    fn before_test(reg_profile: &mut LRegInfo, instructions: &mut LFunctionInfo, from: &str) {
        // Enable for debugging only.
        // enable_logging!();
        let mut register_profile = File::open(REGISTER_PROFILE).unwrap();
        let mut s = String::new();
        register_profile.read_to_string(&mut s).unwrap();
        *reg_profile = serde_json::from_str(&*s).unwrap();
        let mut instruction_file = File::open(from).unwrap();
        let mut s = String::new();
        instruction_file.read_to_string(&mut s).unwrap();
        *instructions = serde_json::from_str(&*s).unwrap();
    }

    #[test]
    fn ssa_simple_test_1() {
        let mut reg_profile = Default::default();
        let mut instructions = Default::default();
        before_test(&mut reg_profile,
                    &mut instructions,
                    "test_files/tiny_sccp_test_instructions.json");
        let mut ssa = SSAStorage::new();
        {
            let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
            constructor.run(instructions.ops.unwrap().as_slice());
        }
        {
            dce::collect(&mut ssa);
        }
        let tmp = dot::emit_dot(&ssa);
        let mut f = File::create("yay.dot").unwrap();
        f.write_all(tmp.as_bytes()).expect("Write failed!");
    }

    #[test]
    fn ssa_const_prop_test_1() {
        let mut reg_profile = Default::default();
        let mut instructions = Default::default();
        before_test(&mut reg_profile,
                    &mut instructions,
                    "test_files/tiny_sccp_test_instructions.json");
        let mut ssa = SSAStorage::new();
        {
            let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
            constructor.run(instructions.ops.unwrap().as_slice());
        }
        {
            dce::collect(&mut ssa);
        }
        let mut ssa = {
            let mut analyzer = sccp::Analyzer::new(&mut ssa);
            analyzer.analyze();
            analyzer.emit_ssa()
        };
        {
            dce::collect(&mut ssa);
        }
        let tmp = dot::emit_dot(&ssa);
        let mut f = File::create("yay.dot").unwrap();
        f.write_all(tmp.as_bytes()).expect("Write failed!");
    }

    #[test]
    fn ssa_bfs_walk() {
        let mut reg_profile = Default::default();
        let mut instructions = Default::default();
        before_test(&mut reg_profile,
                    &mut instructions,
                    "test_files/tiny_sccp_test_instructions.json");
        let mut ssa = SSAStorage::new();
        {
            let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
            constructor.run(instructions.ops.unwrap().as_slice());
        }
        {
            dce::collect(&mut ssa);
        }
        println!("\nBefore Constant Propagation:");
        let mut writer: IRWriter = Default::default();
        println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
        let mut ssa = {
            let mut analyzer = sccp::Analyzer::new(&mut ssa);
            analyzer.analyze();
            analyzer.emit_ssa()
        };
        {
            dce::collect(&mut ssa);
        }
        println!("\nAfter Constant Propagation:");
        let mut writer: IRWriter = Default::default();
        println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
    }
}
