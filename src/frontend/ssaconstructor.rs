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

// TODO: Remove these lints
#![allow(unused_imports, unused_variables, dead_code)]

use std::collections::HashMap;
use petgraph::graph::NodeIndex;
use std::fmt::Debug;
use std::convert::From;
use std::cmp::Ordering;
use std::cmp;

use r2pipe::structs::{LAliasInfo, LOpInfo, LRegInfo};

use esil::parser::{Parse, Parser};
use esil::lexer::{Token, Tokenizer};

use middle::cfg::NodeData as CFGNodeData;
use middle::cfg::EdgeType as CFGEdgeType;
use middle::cfg::{BasicBlock, CFG};
use middle::dce;
use middle::ir::{MAddress, MInst, MOpcode, MVal, MValType};
use middle::phiplacement::PhiPlacer;
use middle::regfile::SubRegisterFile;
use middle::ssa::{BBInfo, SSA, SSAExtra, SSAMod, ValueType};
use middle::ssa::verifier::{VerifiedAdd, Verify};

pub type VarId = usize;

const ESIL_CUR: usize = 0;
const ESIL_OLD: usize = 1;
const ESIL_LASTSZ: usize = 2;

const UNCOND_EDGE: u8 = 2;
const TRUE_EDGE: u8 = 1;
const FALSE_EDGE: u8 = 0;

pub struct SSAConstruct<'a, T>
    where T: 'a + Clone + Debug + SSAMod<BBInfo = MAddress> + SSAExtra
{
    phiplacer: PhiPlacer<'a, T>,
    regfile: SubRegisterFile,
    blocks: Vec<T::ActionRef>,
    intermediates: Vec<T::ValueRef>,
    // TODO: Combine this information with the registers field.
    // even better if this is done at the r2pipe level and we expose API to get
    // alias information.
    alias_info: HashMap<String, String>,
    constants: HashMap<u64, T::ValueRef>,
    ident_map: HashMap<String, u64>,
    // Used to keep track of esil if-else. The reference to the ITE node and the address of this
    // instruction.
    nesting: Vec<(T::ValueRef, MAddress)>,
    // Used to keep track of the offset within an instruction.
    instruction_offset: u64,
    esil_old: Option<T::ValueRef>,
    esil_cur: Option<T::ValueRef>,
    esil_lastsz: Option<u64>,
}

impl<'a, T> SSAConstruct<'a, T>
    where T: 'a + Clone
    + Debug
    + Verify
    + SSAExtra
    + SSAMod<BBInfo=MAddress, ValueRef=NodeIndex, ActionRef=NodeIndex>
{
    pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruct<'a, T> {
        let regfile = SubRegisterFile::new(reg_info);
        let mut sc = SSAConstruct {
            phiplacer: PhiPlacer::new(ssa, regfile.clone()),
            regfile: regfile,
            blocks: Vec::new(),
            intermediates: Vec::new(),
            alias_info: HashMap::new(),
            ident_map: HashMap::new(),
            constants: HashMap::new(),
            nesting: Vec::new(),
            instruction_offset: 0,
            esil_cur: None,
            esil_old: None,
            esil_lastsz: None,
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

        // Add esil_cur, esil_old, and esil_lastsz to the variable list.
        sc.phiplacer.add_variables(vec![
                                   From::from(64 as u16),
                                   From::from(64 as u16),
                                   From::from(64 as u16),
        ]);

        // Add all the registers to the variable list.
        sc.phiplacer.add_variables(sc.regfile.whole_registers.clone());
        sc
    }

    // If the operand is a Token::Identifier, it has to be a register.
    // This is because we never push in a temporary that we create as a
    // Token::Identifier and all ESIL identifiers must be a valid register.
    // It will remain as a Token::Identifier only the first time the parser
    // encounters
    // it, we always push in a Token::Register or a Token::Intermediate.
    fn process_in(&mut self, var: &Option<Token>, address: MAddress) -> Option<T::ValueRef> {
        if var.is_none() {
            return None;
        }
        let ret = match *var.as_ref().unwrap() {
            // Since ESIL has no concept of intermediates, the identifier spotted by parser
            // has to be a register.
            Token::ERegister(ref name) | Token::EIdentifier(ref name) => {
                self.phiplacer.read_register(3, address, name)
            }
            // We arrive at this case only when we have popped an operand that we have pushed
            // into the parser. id refers to the id of the var in our intermediates table.
            Token::EEntry(ref id, _) => {
                self.intermediates[*id]
            }
            Token::EConstant(value) => {
                // Add or retrieve a constant with the value from the table.
                *self.constants.entry(value).or_insert(self.phiplacer.add_const(value))
            }
            // Token::EOld => {
            // self.esil_old.unwrap()
            // }
            // Token::ECur => {
            // self.esil_cur.unwrap()
            // }
            // Token::ELastsz => {
            // *self.constants
            // .entry(self.esil_lastsz.unwrap())
            // .or_insert(self.phiplacer.add_const(self.esil_lastsz.unwrap()))
            // }
            Token::EAddress => {
                // Treat this as retrieving a constant.
                *self.constants
                     .entry(address.address)
                     .or_insert(self.phiplacer.add_const(address.address))
            }
            _ => panic!("SSAConstruct Error: Found something other than a Var as an operand to \
                         an instruction!"),
        };
        Some(ret)
    }

    fn process_out(&mut self, result: Option<T::ValueRef>, address: MAddress) -> Option<Token> {
        // NB 1: Process out is defined for any operation, not only equal as before.
        // Hence, here we should give result a new entry in the "intermediates" table
        // and return a
        // Token::EEntry. This will subsequently be pushed onto the parser stack.
        // If in some case (as in case of '=') we should not push any token onto the
        // parser stack,
        // this function should return `None`.
        // NB 2: We never write to an intermediate twice as this is an SSA form!
        if result.is_none() {
            None
        } else {
            self.intermediates.push(result.unwrap());
            let result_id = self.intermediates.len() - 1;
            let out_size = self.phiplacer.operand_width(result.as_ref().unwrap());
            Some(Token::EEntry(result_id, Some(out_size as u64)))
        }
    }

    fn process_op(&mut self,
                  token: &Token,
                  address: MAddress,
                  operands: &[Option<Token>; 2])
                  -> Option<T::ValueRef> {
        // This is where the real transformation from ESIL to radeco IL happens. This
        // method choose the opcodes to translate from ESIL to radecoIL and also
        // handles assignments
        // and jumps as these are cases that need to be handled a bit differently from
        // the rest of the opcodes.
        let lhs = self.process_in(&operands[0], address);
        let rhs = self.process_in(&operands[1], address);

        // Check if the two operands are of compatible sizes for compare
        let lhs_size = lhs.map_or(0, |i| self.phiplacer.operand_width(&i));
        let rhs_size = rhs.map_or(0, |i| self.phiplacer.operand_width(&i));

        let result_size = cmp::max(lhs_size, rhs_size);

        // Get the radeco Opcode and the output width.
        let (op, vt) = match *token {
            Token::ECmp => {
                let op = MOpcode::OpCmp;
                let vt = ValueType::Integer { width: 1 };
                (op, vt)
            }
            Token::ELt => {
                (MOpcode::OpLt, ValueType::Integer { width: 1 })
            }
            Token::EGt => {
                (MOpcode::OpGt, ValueType::Integer { width: 1 })
            }
            Token::EEq => {
                // This case is the only one that performs a write_register call. Since all
                // assignements in ESIL are only possible to registers, it is reasonable to
                // panic
                // if the register is invalid or not found.
                // If the register being written into is "PC" then we emit a jump (jmp) instead
                // of an assignment.
                if let Some(Token::EIdentifier(ref name)) = operands[0] {
                    if Some("PC".to_owned()) == self.alias_info.get(name).cloned() {
                        let opcode = MOpcode::OpJmp;
                        let jump_target = rhs;
                        // There is a possibility that the jump target is not a constant and we
                        // don't have enough information right now to resolve this target. In this
                        // case, we add a new block and label it unresolved. This maybe resolved as
                        // a part of some other analysis. Right now, the only targets we can
                        // determine are the ones where the rhs is a constant.
                        if let Some(Token::EConstant(target)) = operands[1] {
                            let target_addr = MAddress::new(target, 0);
                            println!("Adding uncond from : {}, to: {}", address, target_addr);
                            self.phiplacer.add_block(target_addr, Some(address), Some(UNCOND_EDGE));
                        }
                    } else {
                        // We are writing into a register.
                        self.esil_old = Some(self.phiplacer.read_register(3, address, &name));
                        self.phiplacer.write_register(3, address, &name, rhs.unwrap());
                        self.esil_cur = Some(rhs.unwrap());
                    }
                } else {
                    // This means that we're performing a memory write. So we need to emit an
                    // OpStore operation.
                    let op_node = self.phiplacer.add_op(&MOpcode::OpStore,
                                                        address,
                                                        ValueType::Integer { width: 0 });
                    self.phiplacer.op_use(&op_node, 0, lhs.as_ref().unwrap());
                    self.phiplacer.op_use(&op_node, 1, rhs.as_ref().unwrap());
                }
                return None;
            }
            // Returns None.
            Token::EIf => {
                // Create a new block for true.
                // The next instruction must be a part of the true block, unless we see an "{"
                let op_node = self.phiplacer.add_op(&MOpcode::OpITE,
                                                    address,
                                                    ValueType::Integer { width: 1 });
                self.nesting.push((op_node, address));
                // Though this is a ternary operator, the other two operands are actually
                // represented throught the control flow edges of the block to which ITE belongs
                // to. For clarity, we will add comments to show the same.
                // Hence: 0 -> compare statement. 1 -> T. 2 -> F.
                let true_address = MAddress::new(address.address, address.offset + 1);
                let true_block = self.phiplacer
                                     .add_block(true_address, Some(address), Some(TRUE_EDGE));
                let true_comment = self.phiplacer.add_comment(address,
                                                              ValueType::Integer { width: 0 },
                                                              format!("T: {}", true_address));
                self.phiplacer.op_use(&op_node, 0, lhs.as_ref().unwrap());
                self.phiplacer.op_use(&op_node, 1, &true_comment);
                return None;
            }
            Token::EEndIf => {
                return None;
            }
            Token::ELsl => {
                (MOpcode::OpLsl, ValueType::Integer { width: result_size })
            }
            Token::ELsr => {
                (MOpcode::OpLsr, ValueType::Integer { width: result_size })
            }
            Token::ERor => {
                unimplemented!()
            }
            Token::ERol => {
                unimplemented!()
            }
            Token::EAnd => {
                (MOpcode::OpAnd, ValueType::Integer { width: result_size })
            }
            Token::EOr => {
                (MOpcode::OpOr, ValueType::Integer { width: result_size })
            }
            Token::ENeg => {
                (MOpcode::OpNot, ValueType::Integer { width: result_size })
            }
            Token::EMul => {
                (MOpcode::OpMul, ValueType::Integer { width: result_size })
            }
            Token::EXor => {
                (MOpcode::OpXor, ValueType::Integer { width: result_size })
            }
            Token::EAdd => {
                (MOpcode::OpAdd, ValueType::Integer { width: result_size })
            }
            Token::ESub => {
                (MOpcode::OpSub, ValueType::Integer { width: result_size })
            }
            Token::EDiv => {
                (MOpcode::OpDiv, ValueType::Integer { width: result_size })
            }
            Token::EMod => {
                (MOpcode::OpMod, ValueType::Integer { width: result_size })
            }
            Token::EPoke(n) => {
                (MOpcode::OpStore, ValueType::Integer { width: n as u16 })
            }
            Token::EPeek(n) => {
                (MOpcode::OpLoad, ValueType::Integer { width: n as u16 })
            }
            Token::EPop => {
                unimplemented!()
            }
            Token::EGoto => {
                unimplemented!()
            }
            Token::EBreak => {
                unimplemented!()
            }
            Token::ENop => {
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

        let op_node;

        // Insert `widen` cast of the two are not of same size and rhs is_some.
        if rhs.is_some() {
            let (lhs, rhs) = match lhs_size.cmp(&rhs_size) {
                Ordering::Greater => {
                    let vt = ValueType::Integer { width: lhs_size };
                    let casted_rhs = self.phiplacer
                                         .add_op(&MOpcode::OpWiden(lhs_size), address, vt);
                    self.phiplacer.op_use(&casted_rhs, 0, rhs.as_ref().unwrap());
                    (lhs.unwrap(), casted_rhs)
                }
                Ordering::Less => {
                    let vt = ValueType::Integer { width: rhs_size };
                    let casted_lhs = self.phiplacer
                                         .add_op(&MOpcode::OpWiden(rhs_size), address, vt);
                    self.phiplacer.op_use(&casted_lhs, 0, lhs.as_ref().unwrap());
                    (casted_lhs, rhs.unwrap())
                }
                Ordering::Equal => {
                    (lhs.unwrap(), rhs.unwrap())
                }
            };
            op_node = self.phiplacer.add_op(&op, address, vt);
            self.phiplacer.op_use(&op_node, 0, &lhs);
            self.phiplacer.op_use(&op_node, 1, &rhs);
        } else {
            // There is only one operand, that is lhs. No need for cast.
            op_node = self.phiplacer.add_op(&op, address, vt);
            self.phiplacer.op_use(&op_node, 0, lhs.as_ref().unwrap());
        }
        Some(op_node)
    }

    fn init_blocks(&mut self) {
        // Create a start block with all registers as variables defined in this block.
        // Seal this block as the start block cannot have any more successors.
        // Create another block and return as "current_block" that we are processing.
        let start_address = MAddress::new(0, 0);

        let start_block = self.phiplacer.add_block(start_address, None, None);

        self.phiplacer.mark_start_node(&start_block);

        let zero = self.phiplacer.add_const(0);
        // TODO: This might not be necessary anymore.
        // self.phiplacer.write_variable(start_address, ESIL_CUR, zero);
        // self.phiplacer.write_variable(start_address, ESIL_OLD, zero);
        // self.phiplacer.write_variable(start_address, ESIL_LASTSZ, zero);

        for (i, name) in self.regfile.whole_names.iter().enumerate() {
            let reg = self.regfile.whole_registers[i];
            // Name the newly created nodes with register names.
            let argnode = self.phiplacer.add_comment(start_address, reg, name.clone());
            // 0, 1 and 2 are esilcur, esilold and lastsz respectively
            self.phiplacer.write_variable(start_address, i, argnode);
        }

        self.phiplacer.sync_register_state(start_block);

        // Add the exit block
        let exit_block = self.phiplacer.add_dynamic();
        self.phiplacer.mark_exit_node(&exit_block);
    }

    // For now, some other component provides SSAConstruct with the instructions
    // that it is
    // supposed to convert into SSA. SSAConstruct does not care from where this
    // ESIL is received,
    // it merely takes this vector of ESIL strings and transforms it into its SSA
    // form.
    pub fn run(&mut self, op_info: Vec<LOpInfo>) {
        let mut p = Parser::init(Some(self.ident_map.clone()), Some(64));
        let mut first_block = true;
        let mut current_address = MAddress::new(0, 0);
        self.init_blocks();
        // println!("Initialization Successfull");
        for op in &op_info {
            if op.esil.is_none() {
                continue;
            }
            let i = op.esil.as_ref().unwrap();
            let offset = op.offset.unwrap_or(0);
            // Reset the instruction offset and remake the current_address.
            // TODO: Improve this mechanism.
            self.instruction_offset = 0;
            let next_address = MAddress::new(offset, self.instruction_offset);

            if !first_block &&
               self.phiplacer.block_of(current_address) != self.phiplacer.block_of(next_address) {
                self.phiplacer.add_edge(current_address, next_address, UNCOND_EDGE);
            }

            current_address = next_address;
            if first_block {
                first_block = false;
                self.phiplacer.add_block(current_address,
                                         Some(MAddress::new(0, 0)),
                                         Some(UNCOND_EDGE));
            }
            // If the nesting vector has a non zero length, then we need to make another
            // block and add connecting false edges, note that this is in accordance to the
            // assumption stated at the top of this file.
            while let Some(ref node) = self.nesting.pop() {
                let src_address = node.1;
                let src_node = &node.0;
                let false_comment = self.phiplacer.add_comment(src_address,
                                                               ValueType::Integer { width: 0 },
                                                               format!("F: {}", current_address));
                self.phiplacer.add_block(current_address, Some(src_address), Some(FALSE_EDGE));
                self.phiplacer.op_use(src_node, 2, &false_comment);
            }

            println!("ESIL: {:?}", i);

            while let Some(ref token) = p.parse::<_, Tokenizer>(i) {
                println!("Got token from parser: {:?}", token);
                let (lhs, rhs) = p.fetch_operands(token);
                // Determine what to do with the operands and get the result.
                let result = self.process_op(token, current_address, &[lhs, rhs]);
                if let Some(result_) = self.process_out(result, current_address) {
                    p.push(result_);
                }
                current_address.offset += 1;
            }
        }
        let last_addr = op_info.last().as_ref().unwrap().offset.unwrap();
        self.phiplacer.add_edge(MAddress::new(last_addr, 0),
                                MAddress::new(0xffffffff, 0),
                                UNCOND_EDGE);
        self.phiplacer.finish();
    }
} // end impl SSAConstruct

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;
    use std::io::prelude::*;
    use rustc_serialize::json;
    use r2pipe::structs::{LAliasInfo, LFunctionInfo, LOpInfo, LRegInfo};
    use middle::ssa::ssastorage::SSAStorage;
    use middle::dot;
    use middle::dce;

    fn before_test(reg_profile: &mut LRegInfo, instructions: &mut LFunctionInfo, from: &str) {
        let mut register_profile = File::open("register_profile").unwrap();
        let mut s = String::new();
        register_profile.read_to_string(&mut s).unwrap();
        *reg_profile = json::decode(&*s).unwrap();
        let mut instruction_file = File::open(from).unwrap();
        let mut s = String::new();
        instruction_file.read_to_string(&mut s).unwrap();
        *instructions = json::decode(&*s).unwrap();
    }

    #[test]
    fn ssa_simple_test_1() {
        let mut reg_profile = Default::default();
        let mut instructions = Default::default();
        before_test(&mut reg_profile, &mut instructions, "instructions_json");
        let mut ssa = SSAStorage::new();
        {
            let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
            constructor.run(instructions.ops.unwrap());
        }
        {
            dce::collect(&mut ssa);
        }
        let tmp = dot::emit_dot(&ssa);
        let mut f = File::create("yay.dot").unwrap();
        f.write_all(tmp.as_bytes());
    }
}
