//! This module is used to convert the instructions generated from esil into
//! a CFG.
//!
//! Control Flow Graphs (CFG) aid in the analysis and recovery of the program
//! structure.

#![allow(dead_code, unused_variables)]

extern crate petgraph;
//use self::petgraph::*;
use self::petgraph::graph::{Graph, NodeIndex};
use super::esil::*;
use std::ops;

/// A `BasicBlock` is the basic unit in a CFG. Every `Instruction` must be a
/// part of one and only one `BasicBlock`. Every instruction in the
/// `BasicBlock` executes sequentially. This `BasicBlock` implementation
/// does not store the instructions themselves but rather just stores the
/// indices of the instructions that belong to a block as a `Range`.
/// Note that rust ranges are half-open. This means that the last value is not
/// included as a part of this `Range`, i.e. (a..b) => [a, b).
pub struct BasicBlock {
	range: ops::Range<u64>,
    index: NodeIndex,
    reachable: bool,
    // Do we need a jump from and a jump to field considering that we already
    // have a graph that we can traverse?
}

impl BasicBlock {
    fn new(i: NodeIndex) -> BasicBlock {
        BasicBlock { 
            range: (0..0),
            index: i,
            reachable: false,
        }
    }
}

pub struct CFG {
    g: Graph<BasicBlock, u8>,
    insts: Vec<Instruction>,
}

impl CFG {
    pub fn new(insts: Vec<Instruction>) -> CFG {
        CFG {
            g: Graph::new(),
            insts: insts,
        }
        // Initializing the cfg must init with 2 default nodes. One 'entry'
        // and another 'exit' node.
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

}

