// Copyright (c) 2015, The Radare Project. All rights preserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! This Module gathers basic information from first analysis of RadecoFunction.
//! It aims at gathering preserved registers and fix OpCall node. Resule will be
//! Used in VSA.
//!
//! More detais will be accessible at:
//!    *  https://www.zybuluo.com/SmashStack/note/850129
//!

use std::collections::{HashMap, HashSet};
use std::slice::Iter;
use std::marker::PhantomData;
use std::fmt::Debug;
use frontend::bindings::{RadecoBindings, RBind, RBindings};
use frontend::containers::{RadecoFunction, RFunction};
use frontend::containers::{RadecoModule, RModule};
use middle::ssa::ssa_traits::{SSA, SSAMod, SSAExtra};
use middle::ssa::ssa_traits::{NodeType, RegInfo};
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssastorage::SSAStorage;
use middle::ir::MOpcode;
use analysis::cse::ssasort::Sorter;


#[derive(Debug)]
pub struct CallFixer<'a, 'b: 'a, B>
    where B: 'b + RBind + Debug,
{
    rmod: &'a mut RadecoModule<'b, RadecoFunction<RadecoBindings<B>>>,
}

impl<'a, 'b: 'a, B> CallFixer<'a, 'b, B>
where B: 'b + RBind + Debug,
{
    pub fn new(rmod: &'a mut RadecoModule<'b, RadecoFunction<RadecoBindings<B>>>) 
        -> CallFixer<'a, 'b, B> {
            CallFixer {
                rmod: rmod,
            }
        }

    pub fn analysis(&mut self, rfn_addr: &u64) {
        radeco_trace!("Analyze {:#}", rfn_addr);

        // Sort operands for commutative opcode first.
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            radeco_trace!("RadecoFunction: {:?}", rfn.name);
            let ssa = rfn.ssa_mut();
            let mut sorter = Sorter::new(ssa);
            sorter.run();
        }

        // Data structures which are used to analyze preserved registers
        let mut reg_offset: HashMap<String, i64> = HashMap::new();
        let mut preserves: HashSet<String> = HashSet::new();
        
        // analysis entry block
        let entry_offset = { 
            let rfn = self.rmod.functions.get(rfn_addr)
                                .expect("RadecoFunction Not Found!");
            let ssa = rfn.ssa_ref();
            self.analysis_entry_offset(ssa)
        };
    }



    // Analyze stack offset of the first basic blocks
    fn analysis_entry_offset(&self, ssa: &SSAStorage) 
            -> HashMap<<SSAStorage as SSA>::ValueRef, i64> {
        let mut stack_offset: HashMap<<SSAStorage as SSA>::ValueRef, i64> = HashMap::new();
        let blocks = ssa.succs_of(ssa.start_node());
        assert_eq!(blocks.len(), 1);
        radeco_trace!("Found: {:?}", blocks);

        // At the entry of a function, SP should decrease first, thus initial
        // SP will in the comment node
        let reg_state = ssa.registers_at(&ssa.start_node());
        let nodes = ssa.args_of(reg_state);
        for node in &nodes {
            if ssa.get_comment(node).is_none() {
                continue;
            }
            radeco_trace!("Comment Node: {:?}", ssa.get_node_data(node));
            if ssa.register(node).is_none() {
                continue;
            }
            if ssa.register(node).unwrap().alias_info 
                != Some(String::from("SP")) {
                    continue;
            }
            radeco_trace!("Initial data is {:?}\n", ssa.get_node_data(node));
            stack_offset.insert(*node, 0);
            break;
        }

        let nodes = ssa.exprs_in(&blocks[0]);
        for node in &nodes {
            if let Some(opc) = ssa.get_opcode(node) {
                // We should stop digging stack offset when me met a call.
                if opc == MOpcode::OpCall {
                    break;
                }

                // Consider OpWiden, especially for 32-bits binary.
                match opc {
                    MOpcode::OpWiden(_) | MOpcode::OpNarrow(_) => {
                        radeco_trace!("Widen/Narrow Node: {:?}", node);
                        radeco_trace!("Widen/Narrow argumes: {:?}\n", 
                                        ssa.get_operands(node));
                        let args = ssa.get_operands(node);
                        if stack_offset.contains_key(&args[0]) {
                            let num = stack_offset.get(&args[0])
                                                    .unwrap()
                                                    .clone();
                            stack_offset.insert(*node, num);
                            continue;
                        }
                    }
                    _ => {  }
                }

                // We only consider SP/BP.
                if ssa.register(node).is_none() {
                    continue;
                }
                let alias_info = ssa.register(node).unwrap().alias_info;
                if alias_info.is_none() {
                    continue;
                }
                let alias_name = alias_info.unwrap();
                if alias_name != String::from("SP") && 
                    alias_name != String::from("BP") {
                        continue;
                    }
                radeco_trace!("Found {:?} {:?}, with {:?}", node, 
                                    ssa.get_node_data(node), 
                                    ssa.register(node));

                // At the entry of a function, only OpSub/OpAdd working on SP/BP
                // could help our analysis
                let args = ssa.get_operands(node);
                if args.len() != 2 {
                    continue;
                }

                let const_arg: i64;
                let opcode_arg: i64;
                match opc {
                    MOpcode::OpSub => {
                        // OpSub is not commutative, so the seconde operand is const
                        const_arg = 1;
                        opcode_arg = 0;
                    }
                    MOpcode::OpAdd => {
                        // OpAdd is commutative, as we have sorted operands, const 
                        // operand will becomde the first one. 
                        const_arg = 0;
                        opcode_arg = 1;
                    }
                    // Some compiler will initial SP with and 0xfffffff0
                    MOpcode::OpAnd => {
                        stack_offset.clear();
                        stack_offset.insert(*node, 0);
                        continue;
                    }
                    _ => {
                        continue;
                    }
                }
                if ssa.get_opcode(&args[opcode_arg as usize]).is_some() || 
                        ssa.get_comment(&args[opcode_arg as usize]).is_some() {
                    if let Some(MOpcode::OpConst(num)) = 
                                ssa.get_opcode(&args[const_arg as usize]) {
                        radeco_trace!("SP/BP operation found {:?}, with {:?}", node,
                                 ssa.register(node));
                        radeco_trace!("Node data is {:?}", ssa.get_node_data(&node));
                        radeco_trace!("Equal to {:?} +/- {:?}", 
                                 ssa.get_node_data(&args[0]),
                                 ssa.get_node_data(&args[1]));
                        // TODO: Some special cases may by not consided
                        if !stack_offset.contains_key(&args[opcode_arg as usize]) {
                            continue;
                        }
                        let base = stack_offset.get(&args[opcode_arg as usize])
                                                                .unwrap()
                                                                .clone() as i64;
                        // A trick to distinguish OpAdd/OpSub.
                        stack_offset.insert(*node, 
                                    base + (opcode_arg - const_arg) * (num as i64));
                        radeco_trace!("New offset for it: {:?}\n", 
                                    base + (opcode_arg - const_arg) *
                                    (num as i64));
                        continue;
                    }
                }
                radeco_warn!("No SP/BP algorithm Found!");
            }
        }
        radeco_trace!("Stack_offset: {:?}", stack_offset);
        stack_offset
    }
}



///// Calculate preserved registers for RadecoFunction.
//pub fn analysis<B: RBind>(rfn: &mut RadecoFunction<RadecoBindings<B>>) {
//   let reg_info = rfn.reg_info.clone();
//   let preserved_regs = gather(&rfn.ssa, reg_info);
//
//   let mut bindings = rfn.bindings.bindings_mut();
//   while let Some(bind) = bindings.next() {
//       if preserved_regs.contains(&bind.name()) {
//           bind.mark_preserved();
//       }
//   }
//}
//

#[cfg(test)]
mod test {
    use super::*;
    use frontend::source::FileSource;
    use frontend::containers::RadecoModule;

    #[test]
    fn module_test() {
        let mut fsource = FileSource::open(Some("./test_files/ct1_sccp_ex/ct1_sccp_ex"));
        let mut rmod = RadecoModule::from(&mut fsource);
        let functions = rmod.functions.clone();
        let mut matched_func_vec: Vec<(u64, &String)> =
            functions.iter().map(|(fn_addr, rfn)| (fn_addr.clone(), &rfn.name)).collect();

        // Analyze preserved for all functions.
        {
            let mut callfixer = CallFixer::new(&mut rmod);
            for func in &matched_func_vec {
                callfixer.analysis(&func.0);
            }    
        }
    }
}
