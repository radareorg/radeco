// Copyright (c) 2015, The Radare Project. All rights preserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! This Module provides information about stack offset with variable,
//! which will help other modules analyze.

use std::collections::{HashMap, HashSet, VecDeque};
use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{SSA, SSAWalk};
use middle::ssa::ssastorage::SSAStorage;

type LValueRef = <SSAStorage as SSA>::ValueRef;


/// Analyze stack offset backward, by assuming the stack of last 
/// SP register is ZERO. 
/// You have to offer RadecoIL SSA and SP register name into analyzer.
pub fn backward_analysis(ssa:&SSAStorage, sp_name: String)
        -> HashMap<LValueRef, i64> {
    let mut stack_offset: HashMap<LValueRef, i64> = HashMap::new();
    let mut worklist: VecDeque<LValueRef> = VecDeque::new();
    let mut visited: HashSet<LValueRef> = HashSet::new();

    // Initial the last SP register offset to ZERO.
    let reg_state = ssa.registers_in(ssa.exit_node()
                                            .expect("Incomplete CFG graph"))
                        .expect("No register state node found");
    let nodes = ssa.operands_of(reg_state);
    for node in &nodes {
        if ssa.registers(*node).contains(&sp_name) {
            stack_offset.insert(*node, 0);
            worklist.push_back(*node);
        }
    }
    
    while let Some(node) = worklist.pop_front() {
        if !visited.contains(&node) {
            visited.insert(node);
        } else {
            continue;
        }

        let base = stack_offset.get(&node).unwrap().clone();
        let args = ssa.operands_of(node);

        let users = ssa.uses_of(node);
        for user in users {
            match ssa.opcode(user) {
                Some(MOpcode::OpZeroExt(_)) |
                Some(MOpcode::OpNarrow(_)) => {
                    stack_offset.insert(user, base);
                    worklist.push_back(user);
                }
                _ => { }
            }
        }

        // The node maybe a comment, for call statements made it
        if ssa.comment(node).is_some() {
            continue;
        }

        // Every SP met at one phi node will have the same value.
        if ssa.is_phi(node) {
            for arg in args {
                stack_offset.insert(arg, base);
                worklist.push_back(arg);
            }
            continue;
        }
        
        // Consider exprission.
        if ssa.opcode(node).is_none() {
            continue;
        }

        let opc = ssa.opcode(node).unwrap();
        match opc {
            MOpcode::OpZeroExt(_) | 
            MOpcode::OpNarrow(_) => {
                stack_offset.insert(args[0], base);
                worklist.push_back(args[0]);
            }
            MOpcode::OpSub => {
                if let Some(MOpcode::OpConst(num)) = ssa.opcode(args[1]) {
                    stack_offset.insert(args[0], base + (num as i64));
                    worklist.push_back(args[0]);
                }
            }
            MOpcode::OpAdd => {
                if let Some(MOpcode::OpConst(num)) = ssa.opcode(args[0]) {
                    stack_offset.insert(args[1], base - (num as i64));
                    worklist.push_back(args[1]);
                }
            }
            _ => { }
        }
    }        

    radeco_trace!("CallFixer|Stack_offset: {:?}", stack_offset);
    stack_offset
}


/// Analyze stack offset frontward, only for first basic block.
/// You have to offer RadecoIL SSA and SP & BP register name into analyzer.
pub fn frontward_analysis(ssa: &SSAStorage,
                       sp_name: String,
                       bp_name: String)
        -> HashMap<LValueRef, i64> {
   generic_frontward_analysis(ssa, sp_name, bp_name, false)  
}


/// Analyze stack offset frontward, for the whole SSA.
/// You have to offer RadecoIL SSA and SP & BP register name into analyzer.
pub fn rounded_analysis(ssa: &SSAStorage,
                       sp_name: String,
                       bp_name: String)
        -> HashMap<LValueRef, i64> {
   generic_frontward_analysis(ssa, sp_name, bp_name, true)  
}

// Analyze stack offset frontward, for the first block or whole SSA.
fn generic_frontward_analysis(ssa: &SSAStorage, 
                         sp_name: String,
                         bp_name: String,
                         is_global: bool) 
        -> HashMap<LValueRef, i64> {
    let mut stack_offset: HashMap<LValueRef, i64> = HashMap::new();
    {
        let reg_state = ssa.registers_in(ssa.entry_node()
                                                .expect("Incomplete CFG graph"))
                            .expect("No register state node found");
        let nodes = ssa.operands_of(reg_state);
        for node in &nodes {
            if ssa.comment(*node) != Some(sp_name.clone()) {
                continue;
            }
            stack_offset.insert(*node, 0);
            break;
        }
    }

    let nodes = if is_global {
        let mut nodes: Vec<LValueRef> = Vec::new();
        let mut walker = ssa.bfs_walk().nodes;    
        while let Some(node) = walker.pop_front() {
            nodes.push(node);
        }
        nodes
    } else {
        let blocks = ssa.succs_of(ssa.entry_node().expect("Incomplete CFG graph"));
        assert_eq!(blocks.len(), 1);
        ssa.exprs_in(blocks[0])
    };

    for node in &nodes {
        if let Some(opc) = ssa.opcode(*node) {
            if opc == MOpcode::OpCall && !is_global {
                break;
            }

            match opc {
                MOpcode::OpZeroExt(_) | MOpcode::OpNarrow(_) => {
                    let args = ssa.operands_of(*node);
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
            if ssa.registers(*node).is_empty() {
                continue;
            }
            let regnames = ssa.registers(*node);
            if !regnames.contains(&sp_name) && 
                !regnames.contains(&bp_name) { 
                    continue;
                }

            let args = ssa.operands_of(*node);
            if args.len() != 2 {
                continue;
            }

            let const_arg: i64;
            let opcode_arg: i64;
            match opc {
                MOpcode::OpSub => {
                    const_arg = 1;
                    opcode_arg = 0;
                }
                MOpcode::OpAdd => {
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
            if ssa.opcode(args[opcode_arg as usize]).is_some() || 
                ssa.comment(args[opcode_arg as usize]).is_some() ||
                (ssa.is_phi(args[opcode_arg as usize]) && is_global) {
                if let Some(MOpcode::OpConst(num)) = 
                            ssa.opcode(args[const_arg as usize]) {
                    // TODO: Some special cases may by not consided
                    if !stack_offset.contains_key(&args[opcode_arg as usize]) {
                        continue;
                    }
                    let base = stack_offset.get(&args[opcode_arg as usize])
                                                            .unwrap()
                                                            .clone() as i64;
                    stack_offset.insert(*node, 
                                base + (opcode_arg - const_arg) * (num as i64));
                    continue;
                }
            }
            radeco_warn!("No SP/BP algorithm Found!");
        }

        // If we are doing a global search, we should consider phi node.
        if ssa.is_phi(*node) && is_global {
            let args = ssa.operands_of(*node);
            let mut nums: Vec<i64> = Vec::new();
            for arg in &args {
                if stack_offset.contains_key(arg) {
                    let num = stack_offset.get(arg).unwrap().clone();
                    if !nums.contains(&num) {
                        nums.push(num);
                    }
                }
            }
            if nums.len() == 1 {
                stack_offset.insert(*node, nums[0]);
            } 
        }
    }
    radeco_trace!("CallFixer|Stack_offset: {:?}", stack_offset);
    stack_offset
}


#[cfg(test)]
mod test {
    use super::*;
    use serde_json;
    use r2api::structs::LFunctionInfo;
    use std::fs::File;
    use std::io::prelude::*;

    use frontend::ssaconstructor::SSAConstruct;
    use middle::ssa::ssastorage::SSAStorage;
    use middle::dce;

    const REGISTER_PROFILE: &'static str = "test_files/x86_register_profile.json";
    const BIN_LS_INSTRUCTIONS: &'static str = "test_files/bin_ls_instructions.json";
    const CT1_INSTRUCTIONS: &'static str = "test_files/ct1_instructions.json";

    #[test]
    fn bin_ls_test() {
        let instructions: LFunctionInfo;
        let mut register_profile = File::open(REGISTER_PROFILE).unwrap();
        let mut s = String::new();
        register_profile.read_to_string(&mut s).unwrap();
        let reg_profile = serde_json::from_str(&*s).unwrap();
        let mut instruction_file = File::open(BIN_LS_INSTRUCTIONS).unwrap();
        let mut s = String::new();
        instruction_file.read_to_string(&mut s).unwrap();
        instructions = serde_json::from_str(&*s).unwrap();
        let mut ssa = SSAStorage::new();
        {
            let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
            constructor.run(instructions.ops.unwrap().as_slice());
        }
        {
            dce::collect(&mut ssa);
        }
        
        frontward_analysis(&ssa, "rsp".to_string(), "rbp".to_string());
        backward_analysis(&ssa, "rsp".to_string());
        rounded_analysis(&ssa, "rsp".to_string(), "rbp".to_string());
    }

    #[test]
    fn ct1_test() {
        let instructions: LFunctionInfo;
        let mut register_profile = File::open(REGISTER_PROFILE).unwrap();
        let mut s = String::new();
        register_profile.read_to_string(&mut s).unwrap();
        let reg_profile = serde_json::from_str(&*s).unwrap();
        let mut instruction_file = File::open(CT1_INSTRUCTIONS).unwrap();
        let mut s = String::new();
        instruction_file.read_to_string(&mut s).unwrap();
        instructions = serde_json::from_str(&*s).unwrap();
        let mut ssa = SSAStorage::new();
        {
            let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
            constructor.run(instructions.ops.unwrap().as_slice());
        }
        {
            dce::collect(&mut ssa);
        }
        
        frontward_analysis(&ssa, "rsp".to_string(), "rbp".to_string());
        backward_analysis(&ssa, "rsp".to_string());
        rounded_analysis(&ssa, "rsp".to_string(), "rbp".to_string());
    }
}
