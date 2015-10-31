// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! This module is used to convert the instructions generated from esil into
//! a CFG.
//!
//! Control Flow Graphs (CFG) aid in the analysis and recovery of the program
//! structure.

use petgraph::graph::Graph;
use petgraph::graph;
use petgraph::Dfs;
use std::collections::BTreeMap;

use super::ir::*;
use super::dot::{DotAttrBlock, GraphDot};

macro_rules! add_strings {
	( $( $x: expr ),* ) => {
		{
			let mut s = String::new();
			$(
				s = format!("{}{}", s, $x);
			 )*
				s
		}
	};
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub reachable: bool,
    pub instructions: Vec<MInst>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum NodeData {
    Block(BasicBlock),
    Entry,
    Exit,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EdgeType {
    True,
    False,
    Unconditional,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Direction {
    d: u8,
}
pub const FORWARD: Direction = Direction { d: 0 };
pub const BACKWARD: Direction = Direction { d: 1 };

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct EdgeData {
    pub direction: Direction,
    pub edge_type: EdgeType,
    src_addr: Address,
    dst_addr: Address,
}

#[derive(Clone)]
pub struct CFG {
    pub g: Graph<NodeData, EdgeData>,
    pub entry: graph::NodeIndex,
    pub exit: graph::NodeIndex,
    pub bbs: BTreeMap<Address, graph::NodeIndex>,
}

impl BasicBlock {
    fn new() -> BasicBlock {
        BasicBlock {
            reachable: false,
            instructions: Vec::new(),
            name: String::new(),
        }
    }

    fn add_instruction(&mut self, inst: MInst) {
        self.instructions.push(inst);
    }
}

impl NodeData {
    pub fn name(&self) -> String {
        match *self {
            NodeData::Block(ref block) => (*(block.name)).to_string(),
            NodeData::Entry => "n0".to_string(),
            NodeData::Exit => "n1".to_string(),
        }
    }
}

impl EdgeData {
    pub fn new(direction: Direction,
               edge_type: EdgeType,
               src_addr: Address,
               dst_addr: Address)
               -> EdgeData {
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
                MOpcode::OpJmp => &inst.operand_1,
                _ => continue,
            };
            // TODO: Resolve the address if it's not a constant.
            let addr = match self.const_value_of(operand) {
                Some(addr) => addr,
                _ => continue,
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

    fn const_value_of(&self, val: &MVal) -> Option<u64> {
        val.as_literal
    }

    fn build_edges(&mut self,
                   current: graph::NodeIndex,
                   next: graph::NodeIndex,
                   inst: MInst,
                   next_inst: MInst) {
        let exit = self.exit.clone();
        match inst.opcode {
            MOpcode::OpJmp => {
                let target_addr = self.const_value_of(&inst.operand_1).unwrap_or(0); // unwrap?
                let edge_data = EdgeData::new_forward_uncond(inst.addr.val, target_addr);
                let target = *(self.bbs.get(&target_addr).unwrap_or(&exit));
                self.add_edge(current, target, edge_data);
            }
            MOpcode::OpCJmp => {
                let target_addr = self.const_value_of(&inst.operand_2).unwrap_or(0); // unwrap?
                let edge_data = EdgeData::new_true(inst.addr.val, target_addr);
                let target = *(self.bbs.get(&target_addr).unwrap_or(&exit));
                self.add_edge(current, target, edge_data);

                let edge_data = EdgeData::new_false(inst.addr.val, next_inst.addr.val);
                self.add_edge(current, next, edge_data);
            }
            _ => {
                let edge_data = EdgeData::new_uncond(inst.addr.val, next_inst.addr.val);
                self.add_edge(current, next, edge_data);
            }
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
                }
                None => {
                    self.add_edge(current,
                                  exit,
                                  EdgeData::new_forward_uncond(inst.addr.val, 0));
                }
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
            if let &mut NodeData::Block(ref mut block) = &mut (self.g[n]) {
                block.reachable = true;
            }
        }
    }

    pub fn add_new_block(&mut self) -> graph::NodeIndex {
        let mut bb = BasicBlock::new();
        // By default a block is always nameed as nX.
        bb.name = format!("n{}", self.g.node_count());
        self.g.add_node(NodeData::Block(bb))
    }

    pub fn add_block(&mut self, bb: BasicBlock) {
        self.g.add_node(NodeData::Block(bb));
    }

    pub fn get_block(&mut self, n: graph::NodeIndex) -> &mut NodeData {
        self.g.node_weight_mut(n).unwrap()
    }

    pub fn add_edge(&mut self,
                    src: graph::NodeIndex,
                    target: graph::NodeIndex,
                    edge_data: EdgeData) {
        self.g.add_edge(src, target, edge_data);
    }
}

/// ///////////////////////////////////////////////////////////////////////////
/// / Trait implementations to emit dot for CFG
/// ///////////////////////////////////////////////////////////////////////////

impl GraphDot for CFG {
	type NodeIndex = graph::NodeIndex;
	type EdgeIndex = graph::EdgeIndex;

    fn configure(&self) -> String {
        add_strings!("digraph cfg {\n", "splines=\"true\";\n")
    }

    fn node_count(&self) -> usize {
        self.g.node_count()
    }

    fn edge_count(&self) -> usize {
        self.g.edge_count()
    }

    fn node_index_new(i: usize) -> Self::NodeIndex {
        graph::NodeIndex::new(i)
    }

    fn edge_index_new(i: usize) -> Self::EdgeIndex {
        graph::EdgeIndex::new(i)
    }

    fn nodes(&self) -> Vec<Self::NodeIndex> {
        (0..self.node_count()).map(|n| graph::NodeIndex::new(n)).collect()
    }

    fn edge_source(&self, edge: &Self::EdgeIndex) -> Self::NodeIndex {
        self.g.raw_edges()[edge.index()].source()
    }

    fn edge_target(&self, edge: &Self::EdgeIndex) -> Self::NodeIndex {
        self.g.raw_edges()[edge.index()].target()
    }

    fn edge_attrs(&self, edge: &Self::EdgeIndex) -> DotAttrBlock {
        let edge = &self.g.raw_edges()[edge.index()];
        let wt = &edge.weight;
        let mut direction = "forward";
        let prefix = format!("n{} -> n{}", edge.source().index(), edge.target().index());
        let (color, label) = match wt.edge_type {
            EdgeType::True => ("green", "label=T "),
            EdgeType::False => ("red", "label=F "),
            EdgeType::Unconditional => ("black", ""),
        };
        if wt.direction == BACKWARD {
            direction = "back";
        }
        DotAttrBlock::Raw(add_strings!(prefix,
                                       " [",
                                       label,
                                       "color=",
                                       color,
                                       " dir=",
                                       direction,
                                       "]"))
    }

    fn node_attrs(&self, i: &Self::NodeIndex) -> DotAttrBlock {
        let node = &self.g[*i];
        let mut result = String::new();
        let mut color = "black";
        result = add_strings!(result,
                              "<<table border=\"0\" cellborder=\"0\" cellpadding=\"1\">");
        result = add_strings!(result,
                              "<tr><td align=\"left\"><font color=\"grey50\" point-size=\"9\">// \
                               NodeIndex: ",
                              node.name(),
                              "</font></td></tr>");
        let res = match *node {
            NodeData::Block(ref block) => {
                let mut _result = String::new();
                if !block.reachable {
                    color = "red";
                }

                // iterate throught the instructions and convert them to dot.
                for inst in &block.instructions {
                    if let MOpcode::OpConst(_) = inst.opcode {
                        continue;
                    }
                    let instrtext = format!("{}", inst);
                    let inst_dot = format!("<tr><td align=\"left\" cellspacing=\"1\"><font \
                                            color=\"grey50\"
								 \
                                            point-size=\"9\">0x{:08x}:</font></td><td \
                                            align=\"left\">{}</td></tr>",
                                           inst.addr.val,
                                           instrtext.replace("&", "&amp;")
                                                    .replace("<", "&lt;")
                                                    .replace(">", "&gt;"));
                    _result = add_strings!(_result, inst_dot);
                }
                _result
            }
            NodeData::Entry => "<tr><td>Entry</td></tr>".to_string(),
            NodeData::Exit => "<tr><td>Exit</td></tr>".to_string(),
        };
        result = add_strings!(result, res, "</table>>");
        let prefix = format!("n{}", i.index());
        DotAttrBlock::Raw(add_strings!(prefix,
                                       " [style=rounded label=",
                                       result,
                                       " shape=box color=",
                                       color,
                                       "]"))
    }
}
