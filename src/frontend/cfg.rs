//! This module is used to convert the instructions generated from esil into
//! a CFG.
//!
//! Control Flow Graphs (CFG) aid in the analysis and recovery of the program
//! structure.

#![allow(dead_code, unused_variables)]

extern crate petgraph;

use self::petgraph::graph::{Graph, NodeIndex};
use super::esil::*;
use std::ops;
use std::collections::HashMap;

use super::dot;

/// A `BasicBlock` is the basic unit in a CFG. Every `Instruction` must be a
/// part of one and only one `BasicBlock`. Every instruction in the
/// `BasicBlock` executes sequentially. This `BasicBlock` implementation
/// does not store the instructions themselves but rather just stores the
/// indices of the instructions that belong to a block as a `Range`.
/// Note that rust ranges are half-open. This means that the last value is not
/// included as a part of this `Range`, i.e. (a..b) => [a, b).
pub struct BasicBlock {
	pub range: ops::Range<u64>,
    pub index: NodeIndex,
    pub reachable: bool,
    pub instructions: HashMap<String, Instruction>,
    pub label: String,
    // Do we need a jump from and a jump to field considering that we already
    // have a graph that we can traverse?
}

impl BasicBlock {
    fn new() -> BasicBlock {
        BasicBlock { 
            range: (0..0),
            index: NodeIndex::new(0),
            reachable: false,
            instructions: HashMap::new(),
            label: String::new(),
        }
    }

    fn add_instruction(&mut self, inst: Instruction) {
        let addr = inst.clone().addr;
        self.instructions.insert(addr, inst);
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
        let i = Instruction::new(Opcode::OpNop, Value::null(), Value::null(), Value::null(), None);
        start_block.instructions.insert("0x0".to_string(), i);
        let start = g.add_node(start_block);
        CFG {
            g: g,
            insts: Vec::new(),
            start: start,
        }
    }

    pub fn build(&mut self) {
        // First we iterate through all the instructions and identify the
        // 'leaders'. 'leaders' are first instruction of some basic block.
        let mut leaders: Vec<&Instruction> = Vec::new();
        for inst in &self.insts {
            match inst.opcode {
                Opcode::OpIf | Opcode::OpBr => leaders.push(inst),
                _ => continue,
            }
        }
    }

    pub fn add_new_block(&mut self) -> &mut BasicBlock {
        let mut bb = BasicBlock::new();
        // Be default a block is always labeled as nX.
        bb.label = format!("n{}", self.g.node_count());
        let n = self.g.add_node(bb);
        self.g.node_weight_mut(n).unwrap()
    }

    pub fn add_block(&mut self, bb: BasicBlock) {
        self.g.add_node(bb);
    }

}

// Dummy test function to call from main for testing purposes.
pub fn make_graph(insts: Vec<Instruction>) {
    let mut cfg = CFG::new();
    let n = NodeIndex::new;
    
    {
        let mut bb: &mut BasicBlock;
        bb = cfg.add_new_block();
        for inst in insts {
            // Just add all the instruction into one basic block.
            bb.add_instruction(inst.clone());
        }
    }

    cfg.g.add_edge(n(0), n(1), 0);
    dot::make_dot(cfg);
}

