//! This module is used to convert the instructions generated from esil into
//! a CFG.
//!
//! Control Flow Graphs (CFG) aid in the analysis and recovery of the program
//! structure.

#![allow(dead_code, unused_variables)]

extern crate petgraph;

use self::petgraph::graph::{Graph, NodeIndex};
use std::ops;
use std::collections::BTreeMap;

use super::dot;
use super::esil::*;

/// A `BasicBlock` is the basic unit in a CFG. Every `Instruction` must be a
/// part of one and only one `BasicBlock`.
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

// Consts used for edge-weights.
// TODO: Make a separate struct for edge-weight to store addtional details.
pub const FORWARD: u8 = 0;
pub const BACKWARD: u8 = 1;

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
        let mut bbs: BTreeMap<Address, NodeIndex> = BTreeMap::new();
        let mut leaders: Vec<Address> = Vec::new();
        let mut i = 0;
        let len = insts.len();
        let cur = self.add_new_block();
        
        self.g.add_edge(self.start, cur, 0);
        bbs.insert(insts[0].clone().addr, cur);

        // First we iterate through all the instructions and identify the
        // 'leaders'. 'leaders' are first instruction of some basic block.
        while i < len {
            let inst = insts[i].clone();
            // If the inst does not change the control flow, simply continue.
            if inst.opcode != Opcode::OpCJmp && inst.opcode != Opcode::OpJmp {
                i += 1;
                continue;
            }

            let addr: i64;
            let operand;
            if inst.opcode == Opcode::OpCJmp {
                operand = inst.operand_2;
            } else {
                operand = inst.operand_1;
            }

            if operand.location == Location::Constant {
                addr = operand.value;
            } else {
                // TODO: Need to decide what we should do if ever this
                // case arises. Right now, we just panic!.
                unreachable!("Unresolved jump!");
            }

            bbs.entry((addr as u64)).or_insert(self.add_new_block());
            leaders.push(addr as u64);
            // Next instruction will also be a leader of a new block.
            if i + 1 >= len {
                break;
            }

            bbs.entry(insts[i+1].addr).or_insert(self.add_new_block());
            leaders.push(insts[i+1].addr);
            i += 1;
        }

        // Reverse sort the leaders.
        leaders.sort_by(|a, b| b.cmp(a));
        let mut i = leaders.len() - 1;
        let mut n = *(bbs.get(&(insts[0].addr)).unwrap());
        for inst in insts {
            // If there are no more leaders or the current instruction is not
            // a leader, simply add it to the BB.
            if inst.addr != leaders[i] {
                self.get_block(n).add_instruction((*inst).clone());
                continue;
            }
            n = *(bbs.get(&(inst.addr)).unwrap());
            self.get_block(n).add_instruction((*inst).clone());
            if i > 0 { i -= 1; }
        }

        // Logic to add edges.
        // In case of an unconditional jump, there will only be one target BasicBlock for the jump.
        // In case of a conditional jump, there will be two possible paths, one if the condition is
        // true and the other 'natural' path when the condition evaluates to false.
        // The false path can never lead to a back-edge as it follows the natural flow of the
        // execution.
        // Iterate through every node, check the jump target of the last instruction. Add an edge
        // to the corresponding BB by referring to BTreeMap.

        for (key, n) in bbs.iter() {
            let mut target: NodeIndex;
            let inst: Instruction;
            {
                let bb = self.get_block(*n);
                inst = bb.instructions.last().unwrap().clone();
            }

            if inst.opcode == Opcode::OpJmp {
                target = *(bbs.get(&(inst.operand_1.value as u64)).unwrap());
                let mut wt = FORWARD;
                if inst.addr > (inst.operand_1.value as u64) { wt = BACKWARD; };
                self.g.add_edge(*n, target, wt);
                continue;
            }

            // Find the BasicBlock the next instruction belongs to.
            // Remember, the next instruction has to be a leader.
            if let Some(addr) = leaders.pop() {
                // This should never create a back-edge as this represents the natural flow path.
                // i.e. the path taken if jumps are ignored.
                if addr > inst.addr {
                    let next = *(bbs.get(&addr).unwrap());
                    self.g.add_edge(*n, next, 0);
                }
            }

            if inst.opcode == Opcode::OpCJmp {
                target = *(bbs.get(&(inst.operand_2.value as u64)).unwrap());
                let mut wt = 0;
                if inst.addr > (inst.operand_2.value as u64) { wt = 1; };
                self.g.add_edge(*n, target, wt);
            }
        }
    }

    pub fn add_new_block(&mut self) -> NodeIndex {
        let mut bb = BasicBlock::new();
        // By default a block is always labeled as nX.
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

