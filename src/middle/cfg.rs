//! This module is used to convert the instructions generated from esil into
//! a CFG.
//!
//! Control Flow Graphs (CFG) aid in the analysis and recovery of the program
//! structure.

#![allow(dead_code, unused_variables)]

use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use petgraph::{Dfs, DfsIter, EdgeDirection};
use std::collections::BTreeMap;

use super::ir::*;

/// A `BasicBlock` is the basic unit in a CFG.
/// Every `Instruction` must be a part of one and only one `BasicBlock`.
pub struct BasicBlock {
    pub reachable: bool,
    pub instructions: Vec<Instruction>,
    pub label: String,
}

impl BasicBlock {
    fn new() -> BasicBlock {
        BasicBlock { 
            reachable: false,
            instructions: Vec::new(),
            label: String::new(),
        }
    }

    fn add_instruction(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }
}

pub enum NodeData {
    Block(BasicBlock),
    Entry,
    Exit,
}

impl NodeData {
    pub fn label(&self) -> String {
        match *self {
            NodeData::Block(ref block) => (*(block.label)).to_string(),
            NodeData::Entry => "n0".to_string(),
            NodeData::Exit => "n1".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EdgeType {
    True,
    False,
    Unconditional,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Direction { d: u8, }
pub const FORWARD: Direction  = Direction { d: 0 };
pub const BACKWARD: Direction = Direction { d: 1 };

pub struct EdgeData {
    pub direction: Direction,
    pub edge_type: EdgeType,
    src_addr: Address,
    dst_addr: Address,
}

impl EdgeData {
    pub fn new(direction: Direction, edge_type: EdgeType, src_addr: Address, dst_addr: Address) -> EdgeData {
        EdgeData {
            direction: direction,
            edge_type: edge_type,
            src_addr: src_addr,
            dst_addr: dst_addr,
        }
    }

    pub fn new_forward_true(src_addr: Address, dst_addr: Address) -> EdgeData {
        EdgeData::new(FORWARD, EdgeType::True, src_addr, dst_addr)
    }

    pub fn new_forward_false(src_addr: Address, dst_addr: Address) -> EdgeData {
        EdgeData::new(FORWARD, EdgeType::False, src_addr, dst_addr)
    }

    pub fn new_backward_true(src_addr: Address, dst_addr: Address) -> EdgeData {
        EdgeData::new(BACKWARD, EdgeType::True, src_addr, dst_addr)
    }

    pub fn new_backward_false(src_addr: Address, dst_addr: Address) -> EdgeData {
        EdgeData::new(BACKWARD, EdgeType::False, src_addr, dst_addr)
    }

    pub fn new_forward_uncond(src_addr: Address, dst_addr: Address) -> EdgeData {
        EdgeData::new(FORWARD, EdgeType::Unconditional, src_addr, dst_addr)
    }

    pub fn new_backward_uncond(src_addr: Address, dst_addr: Address) -> EdgeData {
        EdgeData::new(BACKWARD, EdgeType::Unconditional, src_addr, dst_addr)
    }
}

pub struct CFG {
    pub g: Graph<NodeData, EdgeData>,
    pub entry: NodeIndex,
    pub exit: NodeIndex,
}

impl CFG {
    pub fn new() -> CFG {
        // Initializing the cfg must init with 2 default nodes. One 'entry'
        // and another 'exit' node.
        let mut g: Graph<NodeData, EdgeData> = Graph::new();
        let entry = g.add_node(NodeData::Entry);
        let exit = g.add_node(NodeData::Exit);

        CFG {
            g: g,
            entry: entry,
            exit: exit,
        }
    }

    pub fn build(&mut self, insts: &Vec<Instruction>) {
        let mut bbs: BTreeMap<Address, NodeIndex> = BTreeMap::new();
        let mut leaders: Vec<Address> = Vec::new();
        let mut i = 0;
        let len = insts.len();

        { 
            let cur = self.add_new_block();
            let first_addr = insts[0].clone().addr;
            let entry_index = self.entry;
            self.add_edge(entry_index, cur, EdgeData::new_forward_uncond(0, first_addr));
            bbs.insert(first_addr, cur);
        }

        // First we iterate through all the instructions and identify the
        // 'leaders'. 'leaders' are first instruction of some basic block.
        while i < len {
            let inst = insts[i].clone();
            let addr: i64;
            let operand;

            // If the inst does not change the control flow, simply continue.
            if inst.opcode != Opcode::OpCJmp && inst.opcode != Opcode::OpJmp {
                i += 1;
                continue;
            }

            if inst.opcode == Opcode::OpCJmp {
                operand = inst.operand_2;
            } else {
                operand = inst.operand_1;
            }

            if operand.location == Location::Constant {
                addr = operand.value;
            } else {
                unreachable!("Unresolved Jump!");
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
                if let &mut NodeData::Block(ref mut block) = self.get_block(n) {
                    block.add_instruction((*inst).clone());
                }
                continue;
            }

            n = *(bbs.get(&(inst.addr)).unwrap());
            if let &mut NodeData::Block(ref mut block) = self.get_block(n) {
                block.add_instruction((*inst).clone());
            }

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

        for (_, n) in bbs.iter() {
            let mut target: NodeIndex;
            let mut target_addr: Address;
            let mut edge_data: EdgeData;
            let mut is_false_branch = false;
            let inst: Instruction;

            if let &mut NodeData::Block(ref mut block) = self.get_block(*n) {
                inst = block.instructions.last().unwrap().clone();
            } else {
                unreachable!("Found something other than a BasicBlock!");
            }

            if inst.opcode == Opcode::OpJmp {
                target_addr = inst.operand_1.value as u64;
                edge_data = EdgeData::new_forward_uncond(inst.addr, target_addr);
                target = *(bbs.get(&target_addr).unwrap());
                if inst.addr > target_addr {
                    edge_data.direction = BACKWARD;
                }
                self.add_edge(*n, target, edge_data);
                continue;
            }

            if inst.opcode == Opcode::OpCJmp {
                target_addr = inst.operand_2.value as u64;
                edge_data = EdgeData::new_forward_true(inst.addr, target_addr);
                target = *(bbs.get(&target_addr).unwrap());
                if inst.addr > target_addr {
                    edge_data.direction = BACKWARD;
                }
                self.add_edge(*n, target, edge_data);
                is_false_branch = true;
            }

            // Find the BasicBlock the next instruction belongs to.
            // Remember, the next instruction has to be a leader.
            target = self.exit;
            edge_data = EdgeData::new_forward_uncond(inst.addr, 0);
            if let Some(target_addr) = leaders.pop() {
                // This should never create a back-edge as this represents the natural flow path.
                // i.e. the path taken if jumps are ignored.
                if target_addr > inst.addr {
                    target = *(bbs.get(&target_addr).unwrap());
                    edge_data = EdgeData::new_forward_uncond(inst.addr, target_addr);
                    if is_false_branch { edge_data.edge_type = EdgeType::False; };
                }
            }
            self.add_edge(*n, target, edge_data);
        }

        self.mark_reachable();
    }

    pub fn mark_reachable(&mut self) {
        let mut dfs = Dfs::new(&(self.g), self.entry);
        while let Some(n) = dfs.next(&self.g) {
            if let &mut NodeData::Block(ref mut block) = &mut(self.g[n]) {
                block.reachable = true;
            }
        }
    }

    pub fn add_new_block(&mut self) -> NodeIndex {
        let mut bb = BasicBlock::new();
        // By default a block is always labeled as nX.
        bb.label = format!("n{}", self.g.node_count());
        self.g.add_node(NodeData::Block(bb))
    }

    pub fn add_block(&mut self, bb: BasicBlock) {
        self.g.add_node(NodeData::Block(bb));
    }

    pub fn get_block(&mut self, n: NodeIndex) -> &mut NodeData {
        self.g.node_weight_mut(n).unwrap()
    }

    pub fn add_edge(&mut self, src: NodeIndex, target: NodeIndex, edge_data: EdgeData) {
        self.g.add_edge(src, target, edge_data);
    }
}
