//! This module is used to convert the instructions generated from esil into
//! a CFG.
//!
//! Control Flow Graphs (CFG) aid in the analysis and recovery of the program
//! structure.

#![allow(dead_code, unused_variables)]

use petgraph::graph::{Graph, NodeIndex};
use petgraph::{Dfs};
use std::collections::BTreeMap;

use super::ir::*;

/// A `BasicBlock` is the basic unit in a CFG.
/// Every `MInst` must be a part of one and only one `BasicBlock`.
pub struct BasicBlock {
    pub reachable: bool,
    pub instructions: Vec<MInst>,
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

    fn add_instruction(&mut self, inst: MInst) {
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

    pub fn new_true(src_addr: Address, dst_addr: Address) -> EdgeData {
        if src_addr > dst_addr {
            EdgeData::new_backward_true(src_addr, dst_addr)
        } else {
            EdgeData::new_forward_true(src_addr, dst_addr)
        }
    }

    pub fn new_false(src_addr: Address, dst_addr: Address) -> EdgeData {
        if src_addr > dst_addr {
            EdgeData::new_backward_false(src_addr, dst_addr)
        } else {
            EdgeData::new_forward_false(src_addr, dst_addr)
        }
    }

    pub fn new_uncond(src_addr: Address, dst_addr: Address) -> EdgeData {
        if src_addr > dst_addr {
            EdgeData::new_backward_uncond(src_addr, dst_addr)
        } else {
            EdgeData::new_forward_uncond(src_addr, dst_addr)
        }
    }
}

pub struct CFG {
    pub g: Graph<NodeData, EdgeData>,
    pub entry: NodeIndex,
    pub exit: NodeIndex,
    pub bbs: BTreeMap<Address, NodeIndex>,
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
            bbs: BTreeMap::new(),
        }
    }

    // Iterate through the instructions and create new BasicBlocks.
    pub fn assign_bbs(&mut self, insts: &Vec<MInst>) {
        let mut insts_iter = insts.iter().peekable();
        let first_addr = insts[0].addr.clone();
        let last_addr = insts.last().unwrap().addr.clone();

        // First MInst will be the start of the first BasicBlock.
        {
            let bb = self.add_new_block();
            let entry = self.entry;
            self.add_edge(entry, bb, EdgeData::new_forward_uncond(0, first_addr.val));
            self.bbs.insert(first_addr.val, bb);
        }

        while let Some(ref i) = insts_iter.next() {
            let inst = i.clone();
            let operand = match inst.opcode {
                MOpcode::OpCJmp => &inst.operand_2,
                MOpcode::OpJmp  => &inst.operand_1,
                _               => continue,
            };
            // TODO: Resolve the address if it's not a constant.
            let addr = match operand.val_type {
                MValType::Constant => operand.value as u64,
                _                  => continue,
            };

            if !self.bbs.contains_key(&addr) {
                if addr > last_addr.val || addr < first_addr.val {
                    self.bbs.insert(addr, self.exit);
                } else {
                    let bb = self.add_new_block();
                    self.bbs.insert(addr, bb);
                }
            }

            if let Some(j) = insts_iter.peek() {
                if !self.bbs.contains_key(&(j.addr.val)) {
                    let bb = self.add_new_block();
                    self.bbs.insert(j.addr.val, bb);
                }
            }
        }
    }

    fn build_edges(&mut self, current: NodeIndex, next: NodeIndex, inst: MInst, next_inst: MInst) {
        let exit = self.exit.clone();
        match inst.opcode {
            MOpcode::OpJmp => {
                let target_addr = inst.operand_1.value as u64;
                let edge_data = EdgeData::new_forward_uncond(inst.addr.val, target_addr);
                let target = *(self.bbs.get(&target_addr).unwrap_or(&exit));
                self.add_edge(current, target, edge_data);
            },
            MOpcode::OpCJmp => {
                let target_addr = inst.operand_2.value as u64;
                let edge_data = EdgeData::new_true(inst.addr.val, target_addr);
                let target = *(self.bbs.get(&target_addr).unwrap_or(&exit));
                self.add_edge(current, target, edge_data);

                let edge_data = EdgeData::new_false(inst.addr.val, next_inst.addr.val);
                self.add_edge(current, next, edge_data);
            },
            _ => {
                let edge_data = EdgeData::new_uncond(inst.addr.val, next_inst.addr.val);
                self.add_edge(current, next, edge_data);
            },
        }
    }

    pub fn build(&mut self, insts: &mut Vec<MInst>) {
        insts.sort_by(|a, b| a.addr.val.cmp(&b.addr.val));
        // Identify the first statement of every BasicBlock and assign them.
        self.assign_bbs(insts);
        let mut current = self.bbs.get(&insts[0].addr.val).unwrap().clone();
        let exit = self.exit.clone();
        let mut insts_iter = insts.iter_mut().peekable();
        let mut next = current.clone();

        while let Some(inst) = insts_iter.next() {
            match insts_iter.peek() {
                Some(next_inst) => { 
                    if let Some(x) = self.bbs.get(&next_inst.addr.val) {
                        next = x.clone();
                    }
                    if next != current {
                        self.build_edges(current, next, inst.clone(), (*next_inst).clone());
                    }
                },
                None => {
                    self.add_edge(current, exit, EdgeData::new_forward_uncond(inst.addr.val, 0));
                },
            }

            if let &mut NodeData::Block(ref mut block) = self.get_block(current) {
                block.add_instruction((*inst).clone());
            }
            current = next.clone();
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
