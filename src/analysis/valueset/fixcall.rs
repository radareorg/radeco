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

use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use petgraph::prelude::NodeIndex;

use analysis::cse::ssasort::Sorter;
use frontend::bindings::{RadecoBindings, RBind, RBindings};
use frontend::containers::{RadecoFunction, RFunction};
use frontend::containers::{RadecoModule, RModule};
use frontend::containers::CallContext;
use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{SSA, SSAMod, SSAExtra, SSAWalk};
use middle::ssa::ssa_traits::{NodeType};
use middle::ssa::ssastorage::SSAStorage;

use super::digstack;

type LValueRef = <SSAStorage as SSA>::ValueRef;
type LIdx<B> = <RadecoBindings<B> as RBindings>::Idx;


#[derive(Debug)]
pub struct CallFixer<'a, 'b: 'a, B>
    where B: 'b + RBind + Debug + Clone,
{
    rmod: &'a mut RadecoModule<'b, RadecoFunction<RadecoBindings<B>>>,
    SP_offsets: HashMap<u64, Option<i64>>,
    SP_name: Option<String>,
    BP_name: Option<String>,
}

impl<'a, 'b: 'a, B> CallFixer<'a, 'b, B>
    where B: 'b + RBind + Debug + Clone,
{
    pub fn new(rmod: &'a mut RadecoModule<'b, RadecoFunction<RadecoBindings<B>>>) 
        -> CallFixer<'a, 'b, B> {
            CallFixer {
                BP_name: rmod.regfile.clone()
                    .unwrap().get_name_by_alias(&"BP".to_string()),
                SP_name: rmod.regfile.clone()
                    .unwrap().get_name_by_alias(&"SP".to_string()),
                rmod: rmod,
                SP_offsets: HashMap::new(),
            }
        }

    // Make a ROUNDED analyze for the RadecoModule. 
    pub fn rounded_analysis(&mut self) {
        let functions = self.rmod.functions.clone();
        let mut matched_func_vec: Vec<u64> =
            functions.iter().map(|(fn_addr, rfn)| fn_addr.clone()).collect();
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
            let rfn = self.rmod.functions.get_mut(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            radeco_trace!("CallFixer|RadecoFunction: {:?}", rfn.name);
            let ssa = rfn.ssa_mut();
            let mut sorter = Sorter::new(ssa);
            sorter.run();
        }

        let (entry_store, exit_load) = {
            let rfn = self.rmod.functions.get(rfn_addr)
                                .expect("RadecoFunction Not Found!");
            let ssa = rfn.ssa_ref();
            let SP_name = self.SP_name.clone().unwrap_or(String::new());
            let BP_name = self.BP_name.clone().unwrap_or(String::new());
            let stack_offset = digstack::rounded_analysis(&ssa, SP_name, BP_name);
            // Here, we check the assumption we made in first analysis.
            // If the SP is not balanced, we will throw a WARN or PANIC.
            // TODO: if the SP is not balanced, please UNDO the fix.
            radeco_trace!("CallFixer|Global stack_offset: {:?}", stack_offset);
            if let &Some(SP_offset) = self.SP_offsets.get(rfn_addr).unwrap() {
                let mut max_offset: i64 = 0;
                // The last SP offset should be the biggest
                for offset in &stack_offset {
                    radeco_trace!("CallFixer|{:?} with {:?}: {}", offset.0,
                             ssa.get_node_data(offset.0), offset.1);
                    if offset.1 > &max_offset {
                        max_offset = *offset.1;
                    }
                }
                if max_offset != SP_offset {
                    radeco_warn!("Stack is not Balanced in fn_addr {:?}! \
                                 First analysis {:?} with seconde analysis {:?}",
                                rfn_addr, SP_offset, max_offset);
                    println!("  [*] WARN: Stack is not Balanced in function @ {:#}! Output \
                             analysis may be not accurate",
                             rfn_addr);
                }
            }
            (
                self.analysis_entry_store(ssa, stack_offset.clone()),
                self.analysis_exit_load(ssa, stack_offset)
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

        // Sort operands for commutative opcode first.
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            radeco_trace!("CallFixer|RadecoFunction: {:?}", rfn.name);
            let ssa = rfn.ssa_mut();
            let mut sorter = Sorter::new(ssa);
            sorter.run();
        }

        // At the first time for analysis, we only assume that SP will balance 
        // at entry_point and exit_point. So we only analyze entry block and 
        // exit block separately.
        let (entry_store, exit_load) = {
            let rfn = self.rmod.functions.get(rfn_addr)
                                .expect("RadecoFunction Not Found!");
            let ssa = rfn.ssa_ref();

                // analysis entry block
            let entry_store = { 
                let SP_name = self.SP_name.clone().unwrap_or(String::new());
                let BP_name = self.BP_name.clone().unwrap_or(String::new());
                let entry_offset = digstack::frontward_analysis(&ssa, SP_name, BP_name);
                self.analysis_entry_store(ssa, entry_offset)
            };

                // analysis exit blocks
            let exit_load = {
                let SP_name = self.SP_name.clone().unwrap_or(String::new());
                let exit_offset = digstack::backward_analysis(&ssa, SP_name);
                self.analysis_exit_load(ssa, exit_offset)
            };
            (entry_store, exit_load)
        };

        let SP_offset = self.mark_preserved(rfn_addr, entry_store, exit_load);
        self.SP_offsets.insert(*rfn_addr, SP_offset);
    }

    // Fix the call_site with the callees' preserved register,
    // which will make later analysis much easier.
    pub fn fix(&mut self, rfn_addr: &u64) {
        let call_info: Vec<(LValueRef, Vec<String>)> = {
            let rfn = self.rmod.functions.get(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            self.preserves_for_call_context(rfn.call_sites())
        };
        radeco_trace!("CallFixer|Call site: {:?}", call_info);

        {
            let rfn = self.rmod.functions.get_mut(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            let ssa = rfn.ssa_mut();

            for (node, mut regs) in call_info {
                // Add SP for every function.
                if let Some(ref name) = self.SP_name {
                    regs.push(name.clone());
                }
                for reg in regs {
                    let node_sets = vec![ssa.get_operands(&node), ssa.get_uses(&node)];
                    let mut replace_pair: Vec<LValueRef> = Vec::with_capacity(2);
                    for node_set in node_sets {
                        for sub_node in node_set {
                            if ssa.get_register(&sub_node).contains(&reg) {
                                replace_pair.push(sub_node);
                                break
                            }
                        }
                    }

                    if replace_pair.len() != 2 {
                        radeco_trace!("CallFixer|{:?} with {:?} Not Found!", node, reg);
                        radeco_trace!("CallFixer|Foudn replace_pair {:?}", replace_pair);
                        continue;
                    }

                    ssa.replace(replace_pair[1], replace_pair[0]);
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
    fn mark_preserved(&mut self, rfn_addr: &u64, entry_store: HashMap<String, i64>,
                            exit_load: HashMap<String, i64>) -> Option<i64> {
        let (preserves, SP_offset) = { 
            let mut SP_offset: Option<i64> = None;
            let mut preserves: HashSet<String> = HashSet::new();
            for (name, en_offset) in entry_store {
                if let Some(ex_offset) = exit_load.get(&name).cloned() {
                    // Same reg name have appeared in entry and exit;
                    match SP_offset {
                        None => {
                            SP_offset = Some(en_offset - ex_offset);
                            preserves.insert(name);
                        }
                        Some(off) if en_offset - ex_offset == off => {
                            preserves.insert(name);
                        }
                        Some(_) => {
                            preserves.clear();
                            SP_offset = None;
                            break;
                        } 
                    }
                }
            }
            (preserves, SP_offset)
        };

        radeco_trace!("CallFixer|{:?} with {:?}", preserves, SP_offset);

        // Store data into RadecoFunction
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            let mut bindings = rfn.bindings.bindings_mut();
            while let Some(bind) = bindings.next() {
                if preserves.contains(&bind.name()) {
                    bind.mark_preserved();
                }
                radeco_trace!("CallFixer|Bind: {:?}", bind);
            }
        }
        
        SP_offset
    }

    

    // Analyze exit block's load for preserved registers
    fn analysis_exit_load(&self, ssa: &SSAStorage, 
            exit_offset: HashMap<LValueRef, i64>)
            -> HashMap<String, i64> {
        let mut exit_load: HashMap<String, i64> = HashMap::new();
        let mut worklist: VecDeque<LValueRef> = VecDeque::new();
        let mut visited: HashSet<LValueRef> = HashSet::new();

        let reg_state = ssa.registers_at(&ssa.exit_node());
        worklist.push_back(reg_state);
        
        while let Some(node) = worklist.pop_front() {
            if !visited.contains(&node) {
                visited.insert(node);
            } else {
                continue;
            }
            radeco_trace!("CallFixer|Pop {:?} with {:?}", node, ssa.get_node_data(&node));
            radeco_trace!("CallFixer|Register is {:?}", ssa.get_register(&node));
            let args = ssa.args_of(node);
            for arg in args {
                if ssa.get_register(&arg).is_empty() {
                    continue;
                }

                if ssa.is_phi(&arg) {
                    worklist.push_back(arg);
                    continue;
                }
                match ssa.get_opcode(&arg) {
                    // OpNarrow and OpWiden are transfromed data
                    Some(MOpcode::OpNarrow(_)) |
                    Some(MOpcode::OpWiden(_)) => {
                        worklist.push_back(arg);    
                    }
                    Some(MOpcode::OpLoad) => {
                        let operands = ssa.get_operands(&arg);
                        if !exit_offset.contains_key(&operands[1]) {
                            continue;
                        }

                        let base = exit_offset.get(&operands[1]).unwrap().clone();
                        let names = ssa.get_register(&arg);
                        for name in names {
                            radeco_trace!("CallFixer|Found {:?} with {:?}", name, base);
                            if exit_load.contains_key(&name) {
                                continue;
                            }
                            exit_load.insert(name, base);
                        }
                    }
                    _ => {  }   
                }
            }
        }

        radeco_trace!("CallFixer|Exit_load: {:?}", exit_load);
        exit_load
    }


    // Analyze entry blocks' store for preserved registers
    fn analysis_entry_store(&self, ssa: &SSAStorage, 
            entry_offset: HashMap<LValueRef, i64>)
            -> HashMap<String, i64> {
        let mut entry_store: HashMap<String, i64> = HashMap::new();

        let reg_state = ssa.registers_at(&ssa.start_node());
        let nodes = ssa.args_of(reg_state);
        for node in &nodes {
            if ssa.get_comment(node).is_none(){
                continue;
            }
            if ssa.get_register(node).is_empty() {
                continue;
            }
            let reg_names = ssa.get_register(node);
            let users = ssa.get_uses(node);
            for reg_name in reg_names {
                for user in &users {
                    if Some(MOpcode::OpStore) == ssa.get_opcode(user) {
                        let args = ssa.get_operands(user);
                        if entry_offset.contains_key(&args[1]) {
                            let num = entry_offset.get(&args[1]).unwrap();
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
    fn preserves_for_call_context(&self, 
            call_context: Vec<CallContext<LIdx<B>, LValueRef>>)
            -> Vec<(LValueRef, Vec<String>)> 
    {
        let mut result: Vec<(LValueRef, Vec<String>)> = Vec::new(); 

        for con in call_context {
            // We only analyze the call context which have ssa node.
            if con.ssa_ref.is_none() {
                continue;
            }
            // If callee is not certain
            if con.callee.is_none() {
                result.push((con.ssa_ref.unwrap()
                        , vec![self.SP_name.clone().unwrap_or(String::new())]));
                continue;
            }
            let mut preserves: Vec<String> = Vec::new();
            if let Some(rfn) = self.rmod.functions.get(con.callee.as_ref().unwrap()) {
                // Callee is man made function
                let mut bindings = rfn.bindings.bindings();
                while let Some(bind) = bindings.next() {
                    if bind.is_preserved() {
                        preserves.push(bind.name());
                    }
                }
                result.push((con.ssa_ref.unwrap(), preserves));
            } else {
                // Callee is library function
                result.push((con.ssa_ref.unwrap()
                        , vec![self.BP_name.clone().unwrap_or(String::new())]));
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
        println!("\tInitial Data: {:?}", ssa.get_node_data(&id));
        println!("\tInitial Register: {:?}", ssa.get_register(&id));
        while let Some(node) = worklist.pop_front() {
            if !visited.contains(&node) {
                visited.insert(node);
            } else {
                continue;
            }
            number -= 1;
            println!("Id: {:?}", node);
            println!("\tData: {:?}", ssa.get_node_data(&node));
            println!("\tRegister: {:?}", ssa.get_register(&node));
            println!("\tArgs: {:?}", ssa.get_operands(&node));
            println!("\tUses: {:?}", ssa.get_uses(&node));
            
            if number == 0 {
                break;
            }

            match ssa.get_opcode(&node) {
                Some(MOpcode::OpConst(_)) => { continue; }
                _ => {  }
            }
            for arg in ssa.get_operands(&node) {
                println!("\tArg_Id: {:?}", arg);
                println!("\t\tArg_Data: {:?}", ssa.get_node_data(&arg));
                println!("\t\tArg_Register: {:?}", ssa.get_register(&arg));
                println!("\t\tArg_Args: {:?}", ssa.get_operands(&arg));
                println!("\t\tArg_Uses: {:?}", ssa.get_uses(&arg));
                worklist.push_back(arg);
            }
            for user in ssa.get_uses(&node) {
                println!("\tUse_Id: {:?}", user);
                println!("\t\tUse_Data: {:?}", ssa.get_node_data(&user));
                println!("\t\tUse_Register: {:?}", ssa.get_register(&user));
                println!("\t\tUse_Args: {:?}", ssa.get_operands(&user));
                println!("\t\tUse_Uses: {:?}", ssa.get_uses(&user));
                worklist.push_back(user);
            }
        }
        println!("Number: {}", number);
    } 
}



#[cfg(test)]
mod test {
    use super::*;
    use r2api::api_trait::R2Api;
    use r2pipe::r2::R2;
    use r2api::structs::{FunctionInfo, LFlagInfo, LOpInfo, LRegInfo, LSectionInfo, LStringInfo};
    use frontend::source::FileSource;
    use frontend::containers::RadecoModule;

    #[test]
    fn analysis_test() {
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

    #[test]
    fn fix_test() {
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
            for func in &matched_func_vec {
                callfixer.fix(&func.0);
            }    
        }
    }

    #[test]
    fn reanalysis_test() {
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
            for func in &matched_func_vec {
                callfixer.fix(&func.0);
            }    
            for func in &matched_func_vec {
                callfixer.reanalysis(&func.0);
            }    
        }
    }

    #[test]
    fn rounded_analysis_test() {
        let mut fsource = FileSource::open(Some("./test_files/ct1_sccp_ex/ct1_sccp_ex"));
        let mut rmod = RadecoModule::from(&mut fsource);

        // Analyze preserved for all functions.
        {
            let mut callfixer = CallFixer::new(&mut rmod);
            callfixer.rounded_analysis();
        }
    }

    #[test]
    fn bin_file_rounded_analysis_test() {
        //let mut r2 = R2::new(Some("./test_files/bin_file")).expect("Failed to open r2");
        //r2.init();
        //let mut fsource = FileSource::from(r2);
        let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
        let mut rmod = RadecoModule::from(&mut fsource);
        {
            let mut callfixer = CallFixer::new(&mut rmod);
            callfixer.rounded_analysis();
        }
    } 
}
