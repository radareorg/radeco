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
use std::env;
use std::fmt::Debug;
use std::slice::Iter;
use petgraph::prelude::NodeIndex;

use analysis::cse::ssasort::Sorter;
use frontend::bindings::{RadecoBindings, RBind, RBindings};
use frontend::containers::{RadecoFunction, RFunction};
use frontend::containers::{RadecoModule, RModule};
use frontend::containers::CallContext;
use middle::ir::MOpcode;
use middle::dot::emit_dot;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{SSA, SSAMod, SSAExtra};
use middle::ssa::ssa_traits::{NodeType, RegInfo};
use middle::ssa::ssastorage::SSAStorage;

type LValueRef = <SSAStorage as SSA>::ValueRef;
type LIdx<B> = <RadecoBindings<B> as RBindings>::Idx;


#[derive(Debug)]
pub struct CallFixer<'a, 'b: 'a, B>
    where B: 'b + RBind + Debug + Clone,
{
    rmod: &'a mut RadecoModule<'b, RadecoFunction<RadecoBindings<B>>>,
    SP_offsets: HashMap<u64, Option<i64>>,
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
                rmod: rmod,
                SP_offsets: HashMap::new(),
            }
        }

    // Make a ROUNDED analyze for the RadecoModule. 
    pub fn rounded_analysis(&mut self) {
        let functions = self.rmod.functions.clone();
        let mut matched_func_vec: Vec<(u64, &String)> =
            functions.iter().map(|(fn_addr, rfn)| (fn_addr.clone(), &rfn.name)).collect();
        // Do the first analysis.
        for func in &matched_func_vec {
            self.analysis(&func.0);
        }
        // Do basic fix.
        for func in &matched_func_vec {
            //if !func.1.starts_with("sym.leave") {
            //    continue;
            //}
            self.fix(&func.0);
        }
    }

    // First analysis, only based on the assumption the SP is balanced.
    // After this, we could at least make sure whether BP is balanced,
    // and then we could spread all the stack offset in the whole funciton.
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

        // At the first time for analysis, we only assume that SP will balance 
        // at entry_point and exit_point. So we only analyze entry block and 
        // exit block separately.
        let (entry_store, exit_load) = {
            let rfn = self.rmod.functions.get(rfn_addr)
                                .expect("RadecoFunction Not Found!");
            let ssa = rfn.ssa_ref();

                // analysis entry block
            let entry_store = { 
                let entry_offset = self.analysis_entry_offset(ssa);
                self.analysis_entry_store(ssa, entry_offset)
            };

                // analysis exit blocks
            let exit_load = {
                let exit_offset = self.analysis_exit_offset(ssa);
                self.analysis_exit_load(ssa, exit_offset)
            };
            (entry_store, exit_load)
        };

        // Before we calcluate, we have to finger out the SP offset between entry node 
        // and exit bode. The reason cause this difference is that in SSA form, we didn't
        // split OpCall into STORE PC stage and JMP stage, but we split RET statements
        // into LOAD PC stage and JMP stage. That means, SP at exit point will be higher 
        // than SP at entry point.
        // TODO: We could not finger whether the STORE PC stage in Call use a PUSH stack or
        // MOV register. Thus, we will do a probable estimation in analysis function and 
        // the it in the reanalysis stage.
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

        radeco_trace!("{:?} with {:?}", preserves, SP_offset);

        // Store data into RadecoFunction
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            let mut bindings = rfn.bindings.bindings_mut();
            while let Some(bind) = bindings.next() {
                if preserves.contains(&bind.name()) {
                    bind.mark_preserved();
                }
                radeco_trace!("Bind: {:?}", bind);
            }
        }

        self.SP_offsets.insert(*rfn_addr, SP_offset);
    }


    // Analyze exit block's load for preserved registers
    fn analysis_exit_load(&self, ssa: &SSAStorage, 
            exit_offset: HashMap<LValueRef, i64>)
            -> HashMap<String, i64> {
        let mut exit_load: HashMap<String, i64> = HashMap::new();
        let mut worklist: VecDeque<LValueRef> = VecDeque::new();


        // Initialize worklist with the register state of exit_node.
        let reg_state = ssa.registers_at(&ssa.exit_node());
        worklist.push_back(reg_state);
        
        while let Some(node) = worklist.pop_front() {
            radeco_trace!("Pop {:?} with {:?}", node, ssa.get_node_data(&node));
            radeco_trace!("Register is {:?}", ssa.register(&node));
            let args = ssa.args_of(node);
            for arg in args {
                radeco_trace!("Arg: {:?} with {:?}", arg, ssa.get_node_data(&arg));
                radeco_trace!("Register is {:?}", ssa.register(&arg));

                // We only consider registers.
                if ssa.register(&arg).is_none() {
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
                        // Seconde operand will be the target address.
                        radeco_trace!("OpLoad {:?} with {:?}", arg,
                                    ssa.get_node_data(&arg));
                        radeco_trace!("Register: {:?}", ssa.register(&arg));
                        radeco_trace!("Target {:?} with {:?}", operands[1],
                                    ssa.get_node_data(&operands[1]));
                        radeco_trace!("Target's register: {:?}", ssa.register(&operands[1]));
                        
                        // Consider the target address
                        if !exit_offset.contains_key(&operands[1]) {
                            continue;
                        }

                        let base = exit_offset.get(&operands[1]).unwrap().clone();
                        let name = ssa.register(&arg).unwrap().name;
                        radeco_trace!("Found {:?} with {:?}", name, base);
                        // We store the nearest OpLoad
                        if exit_load.contains_key(&name) {
                            continue;
                        }
                        exit_load.insert(name, base);
                    }
                    _ => {  }   
                }
            }
        }

        radeco_trace!("Exit_load: {:?}", exit_load);
        exit_load
    }


    // Analyze entry blocks' store for preserved registers
    fn analysis_entry_store(&self, ssa: &SSAStorage, 
            entry_offset: HashMap<LValueRef, i64>)
            -> HashMap<String, i64> {
        let mut entry_store: HashMap<String, i64> = HashMap::new();

        // reg_state's operands is initial registers
        let reg_state = ssa.registers_at(&ssa.start_node());
        let nodes = ssa.args_of(reg_state);
        for node in &nodes {
            if ssa.get_comment(node).is_none(){
                continue;
            }
            radeco_trace!("Comment Node: {:?}", ssa.get_node_data(node));
            if ssa.register(node).is_none() {
                continue;
            }
            let reg_name = ssa.register(node).unwrap().name;
            radeco_trace!("Register's name: {:?}", reg_name);
            
            let users = ssa.get_uses(node);
            radeco_trace!("Uses: {:?} with {:?}", users, ssa.get_node_data(&users[0]));

            for user in &users {
                if Some(MOpcode::OpStore) == ssa.get_opcode(user) {
                    let args = ssa.get_operands(user);
                    radeco_trace!("OpStore's operands {:?}", args);
                    // OpStore is not commutative, thus the second operand will be the address
                    if entry_offset.contains_key(&args[1]) {
                        let num = entry_offset.get(&args[1]).unwrap();
                        entry_store.insert(reg_name.clone(), *num);
                    }
                }
            }
        }

        radeco_trace!("Entry_store is {:?}", entry_store);
        entry_store
    } 


    // Analyze stack offset of the exit basic block.
    // Because different structure and order of exit block and entry block,
    // we use different algorithms for them
    fn analysis_exit_offset(&self, ssa:&SSAStorage)
            -> HashMap<LValueRef, i64> {
        let mut stack_offset: HashMap<LValueRef, i64> = HashMap::new();
        let mut worklist: VecDeque<LValueRef> = VecDeque::new();
        let mut visited: HashSet<LValueRef> = HashSet::new();

        let reg_state = ssa.registers_at(&ssa.exit_node());
        let nodes = ssa.args_of(reg_state);
        for node in &nodes {
            radeco_trace!("{:?} with {:?}", node, ssa.get_node_data(node));
            if let Some(reg) = ssa.register(node) {
                if reg.alias_info == Some(String::from("SP")) {
                    radeco_trace!("Initial data {:?} is {:?}", node, ssa.get_node_data(node));
                    stack_offset.insert(*node, 0);
                    worklist.push_back(*node);
                }
            }
        }
        
        while let Some(node) = worklist.pop_front() {
            if !visited.contains(&node) {
                visited.insert(node);
            } else {
                continue;
            }

            let base = stack_offset.get(&node).unwrap().clone();
            let args = ssa.get_operands(&node);

            // For exit node, it's possible that there is not only one node.
            radeco_trace!("Pop {:?} with {:?}", node, ssa.get_node_data(&node));
            radeco_trace!("Register: {:?}", ssa.register(&node));
            radeco_trace!("Args: {:?}", args);

            // We have to consider the OpNarrow/OpWiden which uses node.
            let users = ssa.get_uses(&node);
            for user in users {
                match ssa.get_opcode(&user) {
                    Some(MOpcode::OpWiden(_)) |
                    Some(MOpcode::OpNarrow(_)) => {
                        stack_offset.insert(user, base);
                        worklist.push_back(user);
                    }
                    _ => { }
                }
            }

            // The node maybe a comment, for call statements made it
            if ssa.get_comment(&node).is_some() {
                continue;
            }

            // Every SP met at one phi node will have the same value.
            if ssa.is_phi(&node) {
                for arg in args {
                    stack_offset.insert(arg, base);
                    worklist.push_back(arg);
                }
                continue;
            }
            
            // Consider exprission.
            if ssa.get_opcode(&node).is_none() {
                continue;
            }

            let opc = ssa.get_opcode(&node).unwrap();
            match opc {
                MOpcode::OpWiden(_) | 
                MOpcode::OpNarrow(_) => {
                    stack_offset.insert(args[0], base);
                    worklist.push_back(args[0]);
                }
                MOpcode::OpSub => {
                    if let Some(MOpcode::OpConst(num)) = ssa.get_opcode(&args[1]) {
                        stack_offset.insert(args[0], base + (num as i64));
                        worklist.push_back(args[0]);
                    }
                }
                MOpcode::OpAdd => {
                    if let Some(MOpcode::OpConst(num)) = ssa.get_opcode(&args[0]) {
                        stack_offset.insert(args[1], base - (num as i64));
                        worklist.push_back(args[1]);
                    }
                }
                _ => { }
            }
        }        
    
        radeco_trace!("Stack_offset: {:?}", stack_offset);
        stack_offset
    }

    // Analyze stack offset of the first basic block
    fn analysis_entry_offset(&self, ssa: &SSAStorage) 
            -> HashMap<LValueRef, i64> {
        let mut stack_offset: HashMap<LValueRef, i64> = HashMap::new();
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
            radeco_trace!("Comment Node {:?}: {:?}", node, ssa.get_node_data(node));
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
                        radeco_trace!("Widen/Narrow argumes: {:?}", 
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
                        radeco_trace!("New offset for it: {:?}", 
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

    
    // Fix the call_site with the callees' preserved register,
    // which will make later analysis much easier.
    pub fn fix(&mut self, rfn_addr: &u64) {
        let call_info: Vec<(LValueRef, Vec<String>)> = {
            let rfn = self.rmod.functions.get(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            radeco_trace!("RadecoFunction: {:?}", rfn.name);

            // calcluate call_info into preserved registers and OpCall node
            self.preserves_for_call_context(rfn.call_sites())
        };
        radeco_trace!("Call site: {:?}", call_info);

        // Edit OpCalls' arguments and uses, by preserved registers
        {
            let rfn = self.rmod.functions.get_mut(rfn_addr)
                            .expect("RadecoFunction Not Found!");
            let ssa = rfn.ssa_mut();

            for (node, regs) in call_info {
                // Consider every register
                radeco_trace!("Consider {:?} with {:?}", node, ssa.get_node_data(&node));
                radeco_trace!("Callee is {:?}", 
                         ssa.get_node_data(&ssa.get_operands(&node)[0]));
                for reg in regs {
                    // Calculate replacement and replacer together
                    let node_sets = vec![ssa.get_operands(&node), ssa.get_uses(&node)];
                    let mut replace_pair: Vec<LValueRef> = Vec::with_capacity(2);
                    for node_set in node_sets {
                        for sub_node in node_set {
                            if let Some(sub_reg) = ssa.register(&sub_node) {
                                if sub_reg.name == reg {
                                    replace_pair.push(sub_node);
                                    break
                                }
                            }
                        }
                    }

                    // We have to get a complete replacement pair
                    if replace_pair.len() != 2 {
                        radeco_trace!("{:?} with {:?} Not Found!", node, reg);
                        radeco_trace!("Foudn replace_pair {:?}", replace_pair);
                        continue;
                    }
                    radeco_trace!("{:?} with {:?} Found {:?}", node, reg, replace_pair);

                    ssa.replace(replace_pair[1], replace_pair[0]);
                }
            }
        }
    }


    // Get callee's node and its preserved registers
    fn preserves_for_call_context(&self, 
            call_context: Vec<CallContext<LIdx<B>, LValueRef>>)
            -> Vec<(LValueRef, Vec<String>)> 
    {
        let mut result: Vec<(LValueRef, Vec<String>)> = Vec::new(); 

        for con in call_context {
            // We only analyze the call context which have callee and ssa node.
            if con.callee.is_none() || con.ssa_ref.is_none() {
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
                result.push((con.ssa_ref.unwrap()
                        , vec![self.BP_name.clone().unwrap_or(String::new())]))
            }
        }

        result
    }
    

    // Function used to see graph start from node id.
    // If we have a fast graph generation, this could be removed.
    #[allow(dead_code)]
    fn println(&self, start: usize, mut number: i64, ssa: &SSAStorage) {
        let mut worklist: VecDeque<LValueRef> = VecDeque::new();
        let mut visited: HashSet<LValueRef> = HashSet::new();
        let id = NodeIndex::new(start);
        worklist.push_back(id);
        println!("Initial Id: {:?}", id);
        println!("\tInitial Data: {:?}", ssa.get_node_data(&id));
        println!("\tInitial Register: {:?}", ssa.register(&id));
        while let Some(node) = worklist.pop_front() {
            if !visited.contains(&node) {
                visited.insert(node);
            } else {
                continue;
            }
            number -= 1;
            println!("Id: {:?}", node);
            println!("\tData: {:?}", ssa.get_node_data(&node));
            println!("\tRegister: {:?}", ssa.register(&node));
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
                println!("\t\tArg_Register: {:?}", ssa.register(&arg));
                println!("\t\tArg_Args: {:?}", ssa.get_operands(&arg));
                println!("\t\tArg_Uses: {:?}", ssa.get_uses(&arg));
                worklist.push_back(arg);
            }
            for user in ssa.get_uses(&node) {
                println!("\tUse_Id: {:?}", user);
                println!("\t\tUse_Data: {:?}", ssa.get_node_data(&user));
                println!("\t\tUse_Register: {:?}", ssa.register(&user));
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
}
