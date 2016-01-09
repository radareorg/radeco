// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Converts the from ESIL/CFG representation to SSA.

use std::collections::HashMap;
use petgraph::graph::NodeIndex;
use std::fmt::Debug;
use std::convert::From;

use r2pipe::structs::LRegInfo;

use middle::cfg::NodeData as CFGNodeData;
use middle::cfg::EdgeType as CFGEdgeType;
use middle::cfg::{BasicBlock, CFG};
use middle::dce;
use middle::ir::{MInst, MOpcode, MVal, MValType};
use middle::phiplacement::PhiPlacer;
use middle::regfile::SubRegisterFile;
use middle::ssa::{BBInfo, SSA, SSAExtra, SSAMod};
use middle::ssa::verifier::{VerifiedAdd, Verify};

pub type VarId = usize;

const ESIL_CUR: usize = 0;
const ESIL_OLD: usize = 1;
const LASTSZ: usize = 2;

pub struct SSAConstruction<'a, T>
    where T: 'a + Clone + Debug + SSAMod<BBInfo = BBInfo>
{
    pub phiplacer: PhiPlacer<'a, T>,
    pub regfile: SubRegisterFile,
    pub temps: HashMap<String, T::ValueRef>,
}

impl<'a, T> SSAConstruction<'a, T>
where T: 'a + Clone + Debug + Verify + SSAExtra +
         SSAMod<BBInfo=BBInfo, ValueRef=NodeIndex, ActionRef=NodeIndex>
{
    pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruction<'a, T> {
        let mut sc = SSAConstruction {
            phiplacer: PhiPlacer::new(ssa),
            regfile: SubRegisterFile::new(reg_info),
            temps: HashMap::new(),
        };
        sc.phiplacer.add_variables(vec![
			From::from(64 as u16),
			From::from(64 as u16),
			From::from(64 as u16),
			]);

        sc.phiplacer.add_variables(sc.regfile.whole_registers.clone());
        sc
    }

    pub fn run(&mut self, cfg: &CFG) {
        let node_count = cfg.g.node_count();
        let mut blocks = Vec::<T::ActionRef>::with_capacity(node_count);

        for _ in 0..node_count {
            blocks.push(self.phiplacer.ssa.invalid_action());
        }

        {
            // Insert the entry and exit blocks for the ssa.
            let block = self.phiplacer.add_block(BBInfo { addr: 0 });
            self.phiplacer.ssa.mark_start_node(&block);
            let zero = self.phiplacer.ssa.add_const(block, 0);
            self.phiplacer.write_variable(block, 0, zero); // cur = 0
            self.phiplacer.write_variable(block, 1, zero); // old = 0
            self.phiplacer.write_variable(block, 2, zero); // lastsz = 0

            for (i, name) in self.regfile.whole_names.iter().enumerate() {
                let reg = self.regfile.whole_registers[i];
                let argnode = self.phiplacer.ssa.add_comment(block, reg, name.clone());
                self.phiplacer.write_variable(block, i + 3, argnode);
            }

            self.phiplacer.sync_register_state(block);

            blocks[cfg.entry.index()] = block;
            let block = self.phiplacer.add_dynamic();
            self.phiplacer.ssa.mark_exit_node(&block);
            blocks[cfg.exit.index()] = block;
        }

        for (addr, i) in &cfg.bbs {
            let block = self.phiplacer.add_block(BBInfo { addr: *addr });
            blocks[i.index()] = block;
            if let CFGNodeData::Block(ref srcbb) = cfg.g[*i] {
                self.process_block(block, srcbb);
            } else {
                unreachable!();
            }
        }

        for edge in cfg.g.raw_edges() {
            let i = match edge.weight.edge_type {
                CFGEdgeType::False => 0,
                CFGEdgeType::True => 1,
                CFGEdgeType::Unconditional => 2,
            };

            self.phiplacer.ssa.add_control_edge(blocks[edge.source().index()],
                                                blocks[edge.target().index()],
                                                i);
        }

        for &block in &blocks {
            self.phiplacer.seal_block(block);
        }

        dce::collect(self.phiplacer.ssa);
    }

    fn process_in(&mut self, block: T::ActionRef, mval: &MVal, addr: u64) -> T::ValueRef {
        match mval.val_type {
            MValType::Register => {
                let phip = &mut self.phiplacer;
                self.regfile.read_register(phip, 3, block, &mval.name, addr)
            }
            MValType::Temporary => self.temps[&mval.name],
            MValType::Internal => panic!("This value type should be eliminated during parsing"),
            MValType::EsilCur => self.phiplacer.read_variable(block, ESIL_CUR),
            MValType::EsilOld => self.phiplacer.read_variable(block, ESIL_OLD),
            MValType::Lastsz => self.phiplacer.read_variable(block, LASTSZ),
            MValType::Unknown => self.phiplacer.ssa.invalid_value(),
            MValType::Null => self.phiplacer.ssa.invalid_value(),
        }
    }

    fn process_out(&mut self, block: T::ActionRef, inst: &MInst, value: T::ValueRef) {
        let mval = &inst.dst;
        match mval.val_type { 
            MValType::Register => {
                self.regfile.write_register(&mut self.phiplacer,
                                            3,
                                            block,
                                            &mval.name,
                                            value,
                                            inst.addr.val);
            }
            MValType::Temporary => {
                self.temps.insert(mval.name.clone(), value);
            }
            MValType::Null => {}
            _ => panic!(),
        }
    }

    fn process_op(&mut self,
                  block: T::ActionRef,
                  inst: &MInst,
                  n0: T::ValueRef,
                  n1: T::ValueRef)
                  -> T::ValueRef {
        if inst.opcode == MOpcode::OpEq {
            return n0;
        }

        let dsttype = match inst.dst.val_type {
            MValType::Null => From::from(0 as u16), // there is no ValueType::None?
            _ => From::from(inst.dst.size),
        };

        let nn = {
            (*self.phiplacer.ssa)
                .verified_add_op(block, inst.opcode, dsttype, &[n0, n1], Some(inst.addr.val))
        };

        if inst.update_flags {
            let mut nn64 = nn;

            // Add a widen instruction if the width is not 64.
            if inst.dst.size != 64 {
                let ref mut ssa = self.phiplacer.ssa;
                nn64 = ssa.add_op(block,
                                  MOpcode::OpWiden(64),
                                  From::from(64 as u16),
                                  Some(inst.addr.val));
                ssa.op_use(nn64, 0, nn);
            }

            let old = self.phiplacer.read_variable(block, 0);
            self.phiplacer.write_variable(block, 1, old);
            self.phiplacer.write_variable(block, 0, nn64);
        }

        nn
    }

    fn process_block(&mut self, block: T::ActionRef, source: &BasicBlock) {
        let mut machinestate = self.phiplacer.ssa.to_value(block);
        for ref instruction in &source.instructions {
            let n0 = self.process_in(block, &instruction.operand_1, instruction.addr.val);
            let n1 = self.process_in(block, &instruction.operand_2, instruction.addr.val);

            if instruction.opcode == MOpcode::OpJmp {
                // TODO: In case of static jumps, this is trivial and does not need a selector.
                // In case of dynamic jump, the jump targets have to be determined.
                // self.ssa.g.add_edge(block, n0, SSAEdgeData::DynamicControl(0));
                break;
            }

            if instruction.opcode == MOpcode::OpCJmp {
                self.phiplacer.ssa.mark_selector(n0, block);
                continue;
            }

            let nn = self.process_op(block, instruction, n0, n1);

            if instruction.opcode == MOpcode::OpLoad {
                self.phiplacer.ssa.op_use(nn, 3, machinestate);
            }

            if instruction.opcode == MOpcode::OpStore {
                self.phiplacer.ssa.op_use(nn, 3, machinestate);
                machinestate = nn;
            }

            self.process_out(block, &instruction, nn);
        }
    }
}
