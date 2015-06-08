//! This module is used to convert the instructions generated from esil into
//! a CFG.
//!
//! Control Flow Graphs (CFG) aid in the analysis and recovery of the program
//! structure.

#![allow(dead_code, unused_variables)]

extern crate petgraph;
extern crate num;

use self::petgraph::graph::{Graph, NodeIndex};
use std::ops;


use super::dot;
use super::esil::*;

/// A `BasicBlock` is the basic unit in a CFG. Every `Instruction` must be a
/// part of one and only one `BasicBlock`. Every instruction in the
/// `BasicBlock` executes sequentially. This `BasicBlock` implementation
/// does not store the instructions themselves but rather just stores the
/// indices of the instructions that belong to a block as a `Range`.
/// Note that rust ranges are half-open. This means that the last value is not
/// included as a part of this `Range`, i.e. (a..b) => [a, b).
pub struct BasicBlock {
    pub range: ops::Range<u64>,
    pub reachable: bool,
    pub instructions: Vec<Instruction>,
    pub label: String,
    // Do we need a jump from and a jump to field considering that we already
    // have a graph that we can traverse?
}

impl BasicBlock {
    fn new() -> BasicBlock {
        BasicBlock { 
            range: (0..0),
            reachable: false,
            instructions: Vec::new(),
            label: String::new(),
        }
    }

    fn add_instruction(&mut self, inst: Instruction) {
        let addr = inst.clone().addr;
        self.instructions.push(inst);
    }
}

pub struct CFG {
    pub g: Graph<BasicBlock, u8>,
    pub insts: Vec<Instruction>,
    pub start: NodeIndex,
}

impl CFG {
    pub fn new() -> CFG {
        // Initializing the cfg must init with 2 default nodes. One 'entry'
        // and another 'exit' node.
        let mut g = Graph::new();
        // Add a dummy start node.
        // For now, the start block will have no instructions.
        let mut start_block = BasicBlock::new();
        start_block.label = "n0".to_string();
        // TODO: Replace by a new 'start' Instruction ?
        let i = Instruction::new(Opcode::OpNop, Value::null(), Value::null(),
                                 Value::null(), None);
        start_block.instructions.push(i);
        let start = g.add_node(start_block);
        CFG {
            g: g,
            insts: Vec::new(),
            start: start,
        }
    }

    pub fn build(&mut self, insts: &Vec<Instruction>) {
        // First we iterate through all the instructions and identify the
        // 'leaders'. 'leaders' are first instruction of some basic block.
        let mut leaders: Vec<Address> = Vec::new();
        let mut i = 0;
        let len = insts.len();
        while i < len {
            let inst = insts[i].clone();
            // If the inst does not change the control flow, simply continue.
            if inst.opcode != Opcode::OpCJmp && inst.opcode != Opcode::OpJmp {
                i += 1;
                continue;
            }
            let addr: i64;
            if inst.operand_2.location == Location::Constant {
                addr = inst.operand_2.value;
            } else {
                // TODO: Need to decide what we should do if ever this
                // case arises. Right now, we just panic!.
                unreachable!("Unresolved jump!");
            }

            leaders.push(addr as u64);
            // Next instruction will also be a leader of a new block.
            if i + 1 < len {
                leaders.push(insts[i+1].addr);
            }
            i += 1;
        }

        // Reverse sort the leaders.
        leaders.sort_by(|a, b| b.cmp(a));
        let mut n = self.add_new_block();
        for inst in insts {
            // If there are no more leaders or the current instruction is not
            // a leader, simply add it to the BB.
            if leaders.len() == 0 || inst.addr != *(leaders.last().unwrap()) {
                self.get_block(n).add_instruction((*inst).clone());
                continue;
            }
            n = self.add_new_block();
            self.get_block(n).add_instruction((*inst).clone());
            let _bb = leaders.pop();
        }
        println!("{:?}", leaders);
    }

    pub fn add_new_block(&mut self) -> NodeIndex {
        let mut bb = BasicBlock::new();
        // Be default a block is always labeled as nX.
        bb.label = format!("n{}", self.g.node_count());
        self.g.add_node(bb)
    }

    pub fn add_block(&mut self, bb: BasicBlock) {
        self.g.add_node(bb);
    }

    pub fn get_block(&mut self, n: NodeIndex) -> &mut BasicBlock {
        self.g.node_weight_mut(n).unwrap()
    }

}

// Dummy test function to call from main for testing purposes.
pub fn make_graph(insts: Vec<Instruction>) {
    let mut cfg = CFG::new();
    let n = NodeIndex::new;
    
    {
        //let mut bb: &mut BasicBlock;
        let n = cfg.add_new_block();
        for inst in insts {
            // Just add all the instruction into one basic block.
            cfg.get_block(n).add_instruction(inst.clone());
        }
    }

    cfg.g.add_edge(n(0), n(1), 0);
    dot::make_dot(cfg);
}

