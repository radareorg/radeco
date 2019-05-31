// Copyright (c) 2015, The Radare Project. All rights preserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! This Module gathers basic information from first analysis of RadecoFunction.
//! It aims at gathering preserved registers and fix OpCall node. Results will be
//! Used in VSA.
//!
//! More detais will be accessible at:
//!    *  https://www.zybuluo.com/SmashStack/note/850129
//!

use petgraph::prelude::NodeIndex;
use std::collections::{HashMap, HashSet, VecDeque};

use crate::analysis::cse::ssasort::Sorter;
use crate::frontend::radeco_containers::RadecoModule;
use crate::middle::ir::MOpcode;
use crate::middle::ssa::cfg_traits::CFG;
use crate::middle::ssa::ssa_traits::{SSAMod, SSA};
use crate::middle::ssa::ssastorage::SSAStorage;

use super::digstack;

type LValueRef = <SSAStorage as SSA>::ValueRef;

#[derive(Debug)]
pub struct CallFixer<'a> {
    rmod: &'a mut RadecoModule,
    sp_offsets: HashMap<u64, Option<i64>>,
    sp_name: Option<String>,
    bp_name: Option<String>,
}

impl<'a> CallFixer<'a> {
    pub fn new(
        rmod: &'a mut RadecoModule,
        bp_name: Option<String>,
        sp_name: Option<String>,
    ) -> CallFixer<'a> {
        CallFixer {
            bp_name: bp_name,
            sp_name: sp_name,
            rmod: rmod,
            sp_offsets: HashMap::new(),
        }
    }

    // Make a ROUNDED analyze for the RadecoModule.
    pub fn rounded_analysis(&mut self) {
        let functions = self.rmod.functions.clone();
        let matched_func_vec: Vec<u64> = functions
            .iter()
            .map(|(fn_addr, _)| fn_addr.clone())
            .collect();
        // Do the first analysis.
        radeco_trace!("CallFixer|Do the first analysis.");
        for fn_addr in &matched_func_vec {
            self.analysis(fn_addr);
        }
        // Do basic fix.
        radeco_trace!("CallFixer|Do the basic fix.");
        for fn_addr in &matched_func_vec {
            self.fix(fn_addr);
        }
        // Do the second analysis.
        radeco_trace!("CallFixer|Do the second analysis.");
        for fn_addr in &matched_func_vec {
            self.reanalysis(fn_addr);
        }
        // Redo fix.
        radeco_trace!("CallFixer|Redo fix.");
        for fn_addr in &matched_func_vec {
            self.fix(fn_addr);
        }
    }

    // Second analysis, at this point, we have at least BP for every callee, now, we could
    // do a global search for all the nodes' stack offset. Then, we could make a more accurate
    // preserved fix.
    pub fn reanalysis(&mut self, rfn_addr: &u64) {
        // Sort operands for commutative opcode first.
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr);
            if rfn.is_none() {
                radeco_err!("RadecoFunction Not Found!");
                return;
            };
            let rfn = rfn.unwrap();
            radeco_trace!("CallFixer|RadecoFunction: {:?}", rfn.name);
            let ssa = rfn.ssa_mut();
            let mut sorter = Sorter::new(ssa);
            sorter.run();
        }

        let (entry_store, exit_load) = {
            let rfn = self.rmod.functions.get(rfn_addr).unwrap();
            let ssa = rfn.ssa();
            let sp_name = self.sp_name.clone().unwrap_or(String::new());
            let bp_name = self.bp_name.clone().unwrap_or(String::new());
            let stack_offset = digstack::rounded_analysis(&ssa, sp_name, bp_name);
            // Here, we check the assumption we made in first analysis.
            // If the SP is not balanced, we will throw a WARN or PANIC.
            // TODO: if the SP is not balanced, please UNDO the fix.
            radeco_trace!("CallFixer|Global stack_offset: {:?}", stack_offset);
            let sp_offset_opt = self.sp_offsets.get(rfn_addr).unwrap_or_else(|| {
                radeco_err!("sp_offsets.get({:?}) == None", rfn_addr);
                &None
            });
            if let &Some(sp_offset) = sp_offset_opt {
                let mut max_offset: i64 = 0;
                // The last SP offset should be the biggest
                for offset in &stack_offset {
                    radeco_trace!(
                        "CallFixer|{:?} with {:?}: {}",
                        offset.0,
                        ssa.node_data(*offset.0),
                        offset.1
                    );
                    if offset.1 > &max_offset {
                        max_offset = *offset.1;
                    }
                }
                if max_offset != sp_offset {
                    radeco_warn!(
                        "Stack is not Balanced in fn_addr {:?}! First analysis {:?} with seconde \
                         analysis {:?}",
                        rfn_addr,
                        sp_offset,
                        max_offset
                    );
                    println!(
                        "  [*] WARN: Stack is not Balanced in function @ {:#}! Output analysis \
                         may be not accurate",
                        rfn_addr
                    );
                }
            }
            (
                self.analysis_entry_store(ssa, stack_offset.clone()),
                self.analysis_exit_load(ssa, stack_offset),
            )
        };
        radeco_trace!("CallFixer|Entry_store {:?}", entry_store);
        radeco_trace!("CallFixer|Exit_load {:?}", exit_load);

        self.mark_preserved(rfn_addr, entry_store, exit_load);
    }

    // First analysis, only based on the assumption the SP is balanced.
    // After this, we could at least make sure whether BP is balanced,
    // and then we could spread all the stack offset in the whole funciton.
    pub fn analysis(&mut self, rfn_addr: &u64) {
        radeco_trace!("CallFixer|Analyze {:#}", rfn_addr);

        if self.rmod.functions.get_mut(rfn_addr).is_none() {
            radeco_err!("RadecoFunction Not Found!");
            return;
        }
        // Sort operands for commutative opcode first.
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr).unwrap();
            radeco_trace!("CallFixer|RadecoFunction: {:?}", rfn.name);
            let ssa = rfn.ssa_mut();
            let mut sorter = Sorter::new(ssa);
            sorter.run();
        }

        // At the first time for analysis, we only assume that SP will balance
        // at entry_point and exit_point. So we only analyze entry block and
        // exit block separately.
        let (entry_store, exit_load) = {
            let rfn = self.rmod.functions.get(rfn_addr).unwrap();
            let ssa = rfn.ssa();

            // analysis entry block
            let entry_store = {
                let sp_name = self.sp_name.clone().unwrap_or(String::new());
                let bp_name = self.bp_name.clone().unwrap_or(String::new());
                let entry_offset = digstack::frontward_analysis(&ssa, sp_name, bp_name);
                self.analysis_entry_store(ssa, entry_offset)
            };

            // analysis exit blocks
            let exit_load = {
                let sp_name = self.sp_name.clone().unwrap_or(String::new());
                let exit_offset = digstack::backward_analysis(&ssa, sp_name);
                self.analysis_exit_load(ssa, exit_offset)
            };
            (entry_store, exit_load)
        };

        let sp_offset = self.mark_preserved(rfn_addr, entry_store, exit_load);
        self.sp_offsets.insert(*rfn_addr, sp_offset);
    }

    // Fix the call_site with the callees' preserved register,
    // which will make later analysis much easier.
    pub fn fix(&mut self, rfn_addr: &u64) {
        if self.rmod.functions.get(rfn_addr).is_none() {
            radeco_err!("RadecoFunction Not Found!");
            return;
        }
        let call_info: Vec<(LValueRef, Vec<String>)> = {
            let rfn = self.rmod.functions.get(rfn_addr).unwrap();
            let callees = rfn.callees(&self.rmod.callgraph).clone();
            let addr_callees = callees
                .into_iter()
                .filter_map(|node| self.rmod.callgraph.node_weight(node).map(|a| (*a, node)))
                .collect::<Vec<_>>();
            self.preserves_for_call_context(addr_callees)
        };
        radeco_trace!("CallFixer|Call site: {:?}", call_info);

        {
            let rfn = self.rmod.functions.get_mut(rfn_addr).unwrap();
            let ssa = rfn.ssa_mut();

            for (node, mut regs) in call_info {
                // Add SP for every function.
                if let Some(ref name) = self.sp_name {
                    regs.push(name.clone());
                }
                for reg in regs {
                    let node_sets = vec![ssa.operands_of(node), ssa.uses_of(node)];
                    let mut replace_pair: Vec<LValueRef> = Vec::with_capacity(2);
                    for node_set in node_sets {
                        for sub_node in node_set {
                            if ssa.registers(sub_node).contains(&reg) {
                                replace_pair.push(sub_node);
                                break;
                            }
                        }
                    }

                    if replace_pair.len() != 2 {
                        radeco_trace!("CallFixer|{:?} with {:?} Not Found!", node, reg);
                        radeco_trace!("CallFixer| Found replace_pair {:?}", replace_pair);
                        continue;
                    }

                    ssa.replace_value(replace_pair[1], replace_pair[0]);
                }
            }
        }
    }

    /// Below is helper function.

    // Before we calcluate, we have to finger out the SP offset between entry node
    // and exit bode. The reason cause this difference is that in SSA form, we didn't
    // split OpCall into STORE PC stage and JMP stage, but we split RET statements
    // into LOAD PC stage and JMP stage. That means, SP at exit point will be higher
    // than SP at entry point.
    // TODO: We could not finger whether the STORE PC stage in Call use a PUSH stack or
    // MOV register. Thus, we will do a probable estimation in analysis function and
    // check it in the reanalysis stage.
    fn mark_preserved(
        &mut self,
        rfn_addr: &u64,
        entry_store: HashMap<String, i64>,
        exit_load: HashMap<String, i64>,
    ) -> Option<i64> {
        if self.rmod.functions.get_mut(rfn_addr).is_none() {
            radeco_err!("RadecoFunction Not Found!");
            return None;
        }
        let (preserves, sp_offset) = {
            let mut sp_offset: Option<i64> = None;
            let mut preserves: HashSet<String> = HashSet::new();
            for (name, en_offset) in entry_store {
                if let Some(ex_offset) = exit_load.get(&name).cloned() {
                    // Same reg name have appeared in entry and exit;
                    match sp_offset {
                        None => {
                            sp_offset = Some(en_offset - ex_offset);
                            preserves.insert(name);
                        }
                        Some(off) if en_offset - ex_offset == off => {
                            preserves.insert(name);
                        }
                        Some(_) => {
                            preserves.clear();
                            sp_offset = None;
                            break;
                        }
                    }
                }
            }
            (preserves, sp_offset)
        };

        radeco_trace!("CallFixer|{:?} with {:?}", preserves, sp_offset);

        // Store data into RadecoFunction
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr).unwrap();
            for bind in rfn.bindings_mut().into_iter() {
                if preserves.contains(bind.name()) {
                    bind.mark_preserved();
                }
                radeco_trace!("CallFixer|Bind: {:?}", bind);
            }
        }

        sp_offset
    }

    // Analyze exit block's load for preserved registers
    fn analysis_exit_load(
        &self,
        ssa: &SSAStorage,
        exit_offset: HashMap<LValueRef, i64>,
    ) -> HashMap<String, i64> {
        let mut exit_load: HashMap<String, i64> = HashMap::new();
        let mut worklist: VecDeque<LValueRef> = VecDeque::new();
        let mut visited: HashSet<LValueRef> = HashSet::new();

        let reg_state = registers_in_err!(ssa, exit_node_err!(ssa));
        worklist.push_back(reg_state);

        while let Some(node) = worklist.pop_front() {
            if !visited.contains(&node) {
                visited.insert(node);
            } else {
                continue;
            }
            radeco_trace!("CallFixer|Pop {:?} with {:?}", node, ssa.node_data(node));
            radeco_trace!("CallFixer|Register is {:?}", ssa.registers(node));
            let args = ssa.operands_of(node);
            for arg in args {
                if ssa.registers(arg).is_empty() {
                    continue;
                }

                if ssa.is_phi(arg) {
                    worklist.push_back(arg);
                    continue;
                }
                match ssa.opcode(arg) {
                    // OpNarrow and OpWiden are transfromed data
                    Some(MOpcode::OpNarrow(_)) | Some(MOpcode::OpZeroExt(_)) => {
                        worklist.push_back(arg);
                    }
                    Some(MOpcode::OpLoad) => {
                        let operands = ssa.operands_of(arg);
                        if !exit_offset.contains_key(&operands[1]) {
                            continue;
                        }

                        let base = exit_offset
                            .get(&operands[1])
                            .unwrap_or_else(|| {
                                radeco_err!("Invalid operands: {:?}", operands[1]);
                                &0
                            })
                            .clone();
                        let names = ssa.registers(arg);
                        for name in names {
                            radeco_trace!("CallFixer|Found {:?} with {:?}", name, base);
                            if exit_load.contains_key(&name) {
                                continue;
                            }
                            exit_load.insert(name, base);
                        }
                    }
                    _ => {}
                }
            }
        }

        radeco_trace!("CallFixer|Exit_load: {:?}", exit_load);
        exit_load
    }

    // Analyze entry blocks' store for preserved registers
    fn analysis_entry_store(
        &self,
        ssa: &SSAStorage,
        entry_offset: HashMap<LValueRef, i64>,
    ) -> HashMap<String, i64> {
        let mut entry_store: HashMap<String, i64> = HashMap::new();

        let reg_state = registers_in_err!(ssa, entry_node_err!(ssa));
        let nodes = ssa.operands_of(reg_state);
        for node in &nodes {
            if ssa.comment(*node).is_none() {
                continue;
            }
            if ssa.registers(*node).is_empty() {
                continue;
            }
            let reg_names = ssa.registers(*node);
            let users = ssa.uses_of(*node);
            for reg_name in reg_names {
                for user in &users {
                    if Some(MOpcode::OpStore) == ssa.opcode(*user) {
                        let args = ssa.operands_of(*user);
                        if entry_offset.contains_key(&args[1]) {
                            let num = entry_offset.get(&args[1]).unwrap_or_else(|| {
                                radeco_err!("Error entry_offset.get({:?})", args[1]);
                                &0
                            });
                            entry_store.insert(reg_name.clone(), *num);
                        }
                    }
                }
            }
        }

        radeco_trace!("CallFixer|Entry_store is {:?}", entry_store);
        entry_store
    }

    // Get callee's node and its preserved registers
    fn preserves_for_call_context(
        &self,
        callees: Vec<(u64, NodeIndex)>,
    ) -> Vec<(NodeIndex, Vec<String>)> {
        let mut result: Vec<(NodeIndex, Vec<String>)> = Vec::new();

        for (callee, node) in callees.into_iter() {
            let mut preserves: Vec<String> = Vec::new();
            if let Some(rfn) = self.rmod.functions.get(&callee) {
                // Callee is man made function
                for bind in rfn.bindings().into_iter() {
                    if bind.is_preserved() {
                        preserves.push(bind.name().to_string());
                    }
                }
                result.push((node, preserves));
            } else {
                // Callee is library function
                let bp_name = vec![self.sp_name.clone().unwrap_or(String::new())];
                result.push((node, bp_name));
            }
        }

        result
    }

    // Function used to see graph start from node id.
    // If we have a fast graph generation, this could be removed.
    #[allow(dead_code)]
    fn debug(&self, start: usize, mut number: i64, ssa: &SSAStorage) {
        let mut worklist: VecDeque<LValueRef> = VecDeque::new();
        let mut visited: HashSet<LValueRef> = HashSet::new();
        let id = NodeIndex::new(start);
        worklist.push_back(id);
        println!("Initial Id: {:?}", id);
        println!("\tInitial Data: {:?}", ssa.node_data(id));
        println!("\tInitial Register: {:?}", ssa.registers(id));
        while let Some(node) = worklist.pop_front() {
            if !visited.contains(&node) {
                visited.insert(node);
            } else {
                continue;
            }
            number -= 1;
            println!("Id: {:?}", node);
            println!("\tData: {:?}", ssa.node_data(node));
            println!("\tRegister: {:?}", ssa.registers(node));
            println!("\tArgs: {:?}", ssa.operands_of(node));
            println!("\tUses: {:?}", ssa.uses_of(node));

            if number == 0 {
                break;
            }

            match ssa.opcode(node) {
                Some(MOpcode::OpConst(_)) => {
                    continue;
                }
                _ => {}
            }
            for arg in ssa.operands_of(node) {
                println!("\tArg_Id: {:?}", arg);
                println!("\t\tArg_Data: {:?}", ssa.node_data(arg));
                println!("\t\tArg_Register: {:?}", ssa.registers(arg));
                println!("\t\tArg_Args: {:?}", ssa.operands_of(arg));
                println!("\t\tArg_Uses: {:?}", ssa.uses_of(arg));
                worklist.push_back(arg);
            }
            for user in ssa.uses_of(node) {
                println!("\tUse_Id: {:?}", user);
                println!("\t\tUse_Data: {:?}", ssa.node_data(user));
                println!("\t\tUse_Register: {:?}", ssa.registers(user));
                println!("\t\tUse_Args: {:?}", ssa.operands_of(user));
                println!("\t\tUse_Uses: {:?}", ssa.uses_of(user));
                worklist.push_back(user);
            }
        }
        println!("Number: {}", number);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::frontend::radeco_containers::RadecoModule;

    #[test]
    #[ignore]
    fn analysis_test() {
        let mut rmod = RadecoModule::new("./test_files/ct1_sccp_ex/ct1_sccp_ex".to_string());
        let functions = rmod.functions.clone();
        let matched_func_vec: Vec<u64> = functions
            .iter()
            .map(|(fn_addr, _)| fn_addr.clone())
            .collect();

        // Analyze preserved for all functions.
        {
            let mut callfixer = CallFixer::new(&mut rmod, None, None);
            for func in &matched_func_vec {
                callfixer.analysis(&func);
            }
        }
    }

    #[test]
    #[ignore]
    fn fix_test() {
        let mut rmod = RadecoModule::new("./test_files/ct1_sccp_ex/ct1_sccp_ex".to_string());
        let functions = rmod.functions.clone();
        let matched_func_vec: Vec<u64> = functions
            .iter()
            .map(|(fn_addr, _)| fn_addr.clone())
            .collect();

        // Analyze preserved for all functions.
        {
            let mut callfixer = CallFixer::new(&mut rmod, None, None);
            for func in &matched_func_vec {
                callfixer.analysis(&func);
            }
            for func in &matched_func_vec {
                callfixer.fix(&func);
            }
        }
    }

    #[test]
    #[ignore]
    fn reanalysis_test() {
        let mut rmod = RadecoModule::new("./test_files/ct1_sccp_ex/ct1_sccp_ex".to_string());
        let functions = rmod.functions.clone();
        let matched_func_vec: Vec<u64> = functions
            .iter()
            .map(|(fn_addr, _)| fn_addr.clone())
            .collect();

        // Analyze preserved for all functions.
        {
            let mut callfixer = CallFixer::new(&mut rmod, None, None);
            for func in &matched_func_vec {
                callfixer.analysis(&func);
            }
            for func in &matched_func_vec {
                callfixer.fix(&func);
            }
            for func in &matched_func_vec {
                callfixer.reanalysis(&func);
            }
        }
    }

    #[test]
    #[ignore]
    fn rounded_analysis_test() {
        let mut rmod = RadecoModule::new("./test_files/ct1_sccp_ex/ct1_sccp_ex".to_string());

        // Analyze preserved for all functions.
        {
            let mut callfixer = CallFixer::new(&mut rmod, None, None);
            callfixer.rounded_analysis();
        }
    }

    #[test]
    #[ignore]
    fn bin_file_rounded_analysis_test() {
        let mut rmod = RadecoModule::new("./test_files/ct1_sccp_ex/ct1_sccp_ex".to_string());
        {
            let mut callfixer = CallFixer::new(&mut rmod, None, None);
            callfixer.rounded_analysis();
        }
    }
}
