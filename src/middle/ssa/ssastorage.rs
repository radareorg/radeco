// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that holds the struct and trait implementations for the ssa form.

use std::fmt::{self, Debug};
use std::collections::{HashMap, VecDeque, HashSet, BinaryHeap};
use std::{default, u64};
use std::cmp::{PartialOrd, PartialEq, Ordering};
use petgraph::visit::{IntoEdgeReferences, EdgeRef};
use petgraph::EdgeDirection;
use petgraph::stable_graph::StableDiGraph;
use petgraph::graph::{EdgeIndex,  NodeIndex};
use middle::ir::{MAddress, MOpcode};

use super::ssa_traits::NodeData as TNodeData;
use super::ssa_traits::NodeType as TNodeType;
use super::ssa_traits::{SSA, SSAExtra, SSAMod, SSAWalk, ValueType, RegInfo};
use super::cfg_traits::{CFG, CFGMod};
use utils::logger;

/// Structure that represents data that maybe associated with an node in the
/// SSA
#[derive(Clone, Debug)]
pub struct AdditionalData {
    comments: Option<String>,
    flag: Option<String>,
    mark: bool,
    color: Option<u8>,
    // Phi nodes and Opcode node which EEq into registers and directly 
    // influence registers' width will have register AdditionalData.
    register: Option<RegInfo>,
}

impl AdditionalData {
    fn new() -> AdditionalData {
        AdditionalData {
            comments: None,
            flag: None,
            mark: false,
            color: None,
            register: None,
        }
    }
}

impl default::Default for AdditionalData {
    fn default() -> AdditionalData {
        AdditionalData {
            comments: None,
            flag: None,
            mark: false,
            color: None,
            register: None,
        }
    }
}

pub type AssociatedData = HashMap<NodeIndex, AdditionalData>;

/// Node type for the SSAStorage-internal petgraph.
/// Both actions and values are represented using this same enum.
///
/// Value nodes are `Op`, `Phi`, `Comment`, `Undefined` and `Removed`.
/// Action nodes are `Unreachable`, `BasicBlock`, `DynamicAction`
/// `RegisterState` is neither.
/// Value nodes have a `ValueType` that can be extracted with
/// `SSA::get_node_data`
#[derive(Clone, Debug)]
pub enum NodeData {
    /// Represents on operation.
    Op(MOpcode, ValueType),
    /// Represents a phi node.
    Phi(ValueType, String),
    /// Represents an undefined node with a comment.
    Comment(ValueType, String),
    /// Represents an undefined node without comment.
    Undefined(ValueType),
    /// Placeholder for value nodes.
    Removed,
    /// Placeholder for action nodes.
    Unreachable,
    /// Represents a basic block.
    BasicBlock(MAddress),
    /// Represents an action that doesn't contain any value nodes, described
    /// by its associated RegisterState (which includes the instruction
    /// pointer).
    DynamicAction,
    /// Represents the state of the register file at the moment of entry into
    /// the associated action node.
    RegisterState,
}

// Implement display helper for NodeData to make it a little nicer to read prefix notation.
impl fmt::Display for NodeData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            NodeData::Op(op, _) => format!("{}", op),
            NodeData::Phi(_, _) => "Phi".to_owned(),
            NodeData::Comment(_, ref s) => s.clone(),
            // Don't care about these
            _ => String::new(),
        };
        write!(f, "{}", s)
    }
}

/// Edge type for the SSAStorage-internal petgraph.
#[derive(Clone, Copy, Debug)]
pub enum EdgeData {
    /// Edge from action to action. Represents control flow. The number is
    /// used to distinguish true branch, false branch, etc.
    Control(u8),
    /// Edge from value or RegisterState to value. Represents data flow. The
    /// number describes the howmanyeth argument of the edge source is encoded
    /// by this edge.
    Data(u8),
    /// Edge from value to BasicBlock.
    ContainedInBB(MAddress),
    /// Edge from BasicBlock to value. Points from a basic block with multiple
    /// successors to a value that decides which branch will be taken.
    Selector,
    /// Edge from action to RegisterState. Represents the values of all
    /// registers at entry to the action.
    RegisterState,
    /// Edge from Removed to value or from Unreachable to action. Created when
    /// calling replace() in stable indices mode.
    ReplacedBy,
}

/// A petgraph based SSA storage.
#[derive(Debug, Clone)]
pub struct SSAStorage {
    pub g: StableDiGraph<NodeData, EdgeData>,
    pub start_node: NodeIndex,
    pub exit_node: NodeIndex,
    pub assoc_data: AssociatedData,
    pub regnames: Vec<String>,
    last_key: usize,
}

impl default::Default for SSAStorage {
    fn default() -> SSAStorage {
        SSAStorage {
            g: StableDiGraph::new(),
            start_node: NodeIndex::end(),
            exit_node: NodeIndex::end(),
            assoc_data: HashMap::new(),
            regnames: Vec::new(),
            last_key: 0,
        }
    }
}

impl SSAStorage {
    pub fn new() -> SSAStorage {
        SSAStorage {
            g: StableDiGraph::new(),
            start_node: NodeIndex::end(),
            exit_node: NodeIndex::end(),
            assoc_data: HashMap::new(),
            regnames: Vec::new(),
            last_key: 0,
        }
    }

    //////////////////////////////////////////////////////////////////////////
    //// Revised API for SSAStorage.
    //////////////////////////////////////////////////////////////////////////

    // Regulate all access to SSAStorage, especially insertions and deletions
    // through this API to prevent stablemap from being out of sync.

    fn insert_node(&mut self, d: NodeData) -> NodeIndex {
        let ret = self.g.add_node(d);
        radeco_trace!(logger::Event::SSAInsertNode(&ret));
        ret
    }

    fn remove_node(&mut self, exi: NodeIndex) {
        radeco_trace!(logger::Event::SSARemoveNode(&exi));
        // Remove the current association.
        self.g.remove_node(exi);
    }

    fn replace_node(&mut self, i: NodeIndex, j: NodeIndex) {
        radeco_trace!(logger::Event::SSAReplaceNode(&i, &j));
        // Before replace, we need to copy over the edges.

        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Data(d) = self.g[edge] {
                match self.g[othernode] {
                    NodeData::Op(_, _) | NodeData::RegisterState => {
                        self.op_use(othernode, d, j);
                    }
                    NodeData::Phi(_, _) => {
                        self.phi_use(othernode, j);
                    }
                    _ => {}
                }
            } else if let EdgeData::Selector = self.g[edge] {
                let bb = self.block_of(&i);
                self.mark_selector(j, bb);
            }
            self.g.remove_edge(edge); // TODO: Need suggestion. Remove edges? if not, there might be an operation to get successor on the preds(i) which would result in access to this deleted node
        }

        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            self.g.remove_edge(edge);
        }

        if self.start_node == i {
            self.start_node = j;
        }

        self.remove_node(i);
    }

    fn insert_edge(&mut self, i: NodeIndex, j: NodeIndex, e: EdgeData) -> EdgeIndex {
        //Don't insert a duplicate edge between i and j with same EdgeData
        let edge = self.find_edge(&i, &j);
        let mut flag = false;
        if edge != self.invalid_edge() {
            flag = match (self.g[edge], e) {
                (EdgeData::Control(i), EdgeData::Control(j)) |
                (EdgeData::Data(i), EdgeData::Data(j)) => {
                    if i == j {
                        true
                    } else {
                        false
                    }
                },
                (EdgeData::ContainedInBB(i), EdgeData::ContainedInBB(j)) => {
                    if i == j {
                        true
                    } else {
                        false
                    }
                },
                (EdgeData::Selector, EdgeData::Selector) => true,
                (EdgeData::RegisterState, EdgeData::RegisterState) => true,
                (EdgeData::ReplacedBy, EdgeData::ReplacedBy) => true,
                _ => false,
            }
        }
        if flag {
            edge
        } else {
            radeco_trace!(logger::Event::SSAInsertEdge(&i, &j));
            self.g.add_edge(i, j, e)
        }
    }

    #[allow(dead_code)]
    fn update_edge(&mut self, i: NodeIndex, j: NodeIndex, e: EdgeData) -> EdgeIndex {
        radeco_trace!(logger::Event::SSAUpdateEdge(&i, &j));
        self.g.update_edge(i, j, e)
    }

    fn delete_edge(&mut self, i: NodeIndex, j: NodeIndex) {
        radeco_trace!(logger::Event::SSARemoveEdge(&i, &j));
        let e = self.g.find_edge(i, j);
        if let Some(ei) = e {
            self.g.remove_edge(ei);
        }
    }

    pub fn valid_nodes(&self) -> Vec<NodeIndex> {
        self.g.node_indices().collect()
    }

    pub fn read_const(&self, ni: NodeIndex) -> Option<u64> {
        if let NodeData::Op(MOpcode::OpConst(n), _) = self.g[ni] {
            Some(n)
        } else {
            None
        }
    }

    fn gather_adjacent(&self,
                       node: NodeIndex,
                       direction: EdgeDirection,
                       data: bool)
                       -> Vec<NodeIndex> {
        let mut adjacent = Vec::new();
        let mut walk = self.g.neighbors_directed(node, direction).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            match (data, self.g[edge]) {
                (true, EdgeData::Data(i)) |
                (false, EdgeData::Control(i)) => adjacent.push((i, othernode)),
                _ => {}
            }
        }
        adjacent.sort_by(|a, b| a.0.cmp(&b.0));
        adjacent.iter().map(|x| x.1).collect::<Vec<_>>()
    }

    pub fn is_block(&self, node: NodeIndex) -> bool {
        if let NodeData::BasicBlock(_) = self.g[node] {
            true
        } else {
            false
        }
    }

    pub fn is_action(&self, action: NodeIndex) -> bool {
        match self.g[action] {
            NodeData::BasicBlock(_) | NodeData::DynamicAction => true,
            _ => false,
        }
    }
}

impl CFG for SSAStorage {
	type ActionRef = NodeIndex;
	type CFEdgeRef = EdgeIndex;

    fn blocks(&self) -> Vec<NodeIndex> {
        self.g.node_indices().filter(|x| match self.g[*x] {
            NodeData::BasicBlock(_) => true,
            _ => false,
        }).collect()
    }

    fn start_node(&self) -> NodeIndex {
        assert_ne!(self.start_node, NodeIndex::end());
        self.start_node
    }

    fn exit_node(&self) -> NodeIndex {
        assert_ne!(self.exit_node, NodeIndex::end());
        self.exit_node
    }

    fn get_unconditional(&self, i: &Self::ActionRef) -> Self::ActionRef {
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Control(2) = self.g[edge] {
                return othernode;
            }
        }
        NodeIndex::end()
    }

    fn preds_of(&self, exi: NodeIndex) -> Vec<NodeIndex> {
        self.gather_adjacent(exi, EdgeDirection::Incoming, false)
    }

    fn succs_of(&self, exi: NodeIndex) -> Vec<NodeIndex> {
        self.gather_adjacent(exi, EdgeDirection::Outgoing, false)
    }

    fn invalid_action(&self) -> NodeIndex {
        NodeIndex::end()
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Edge accessors and helpers
    ///////////////////////////////////////////////////////////////////////////
    fn edges_of(&self, i: &NodeIndex) -> Vec<(EdgeIndex, u8)> {
        let mut edges = Vec::new();
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Outgoing).detach();
        while let Some((edge, _)) = walk.next(&self.g) {
            if let EdgeData::Control(i) = self.g[edge] {
                edges.push((edge, i));
            }
        }
        edges
    }

    fn info(&self, i: &EdgeIndex) -> (NodeIndex, NodeIndex) {
        if let Some(edge_data) = self.g.edge_references().find(|x| x.id() == *i) {
            (edge_data.source(),
            edge_data.target())
        } else {
            (NodeIndex::end(), NodeIndex::end())
        }
    }

    fn find_edge(&self, source: &NodeIndex, target: &NodeIndex) -> EdgeIndex {
        if *source == self.invalid_value() || *target == self.invalid_value() {
            return self.invalid_edge();
        }

        self.g.find_edge(*source, *target).unwrap_or_else(EdgeIndex::end)
    }

    fn true_edge_of(&self, i: &NodeIndex) -> EdgeIndex {
        let edges = self.edges_of(i);
        for &(ref edge, ety) in &edges {
            if ety == 1 {
                return *edge;
            }
        }
        EdgeIndex::end()
    }

    fn false_edge_of(&self, i: &NodeIndex) -> EdgeIndex {
        let edges = self.edges_of(i);
        for &(ref edge, ety) in &edges {
            if ety == 0 {
                return *edge;
            }
        }
        EdgeIndex::end()
    }

    fn next_edge_of(&self, i: &NodeIndex) -> EdgeIndex {
        let edges = self.edges_of(i);
        for &(ref edge, ety) in &edges {
            if ety == 2 {
                return *edge;
            }
        }
        EdgeIndex::end()
    }

    fn incoming_edges(&self, i: &NodeIndex) -> Vec<(EdgeIndex, u8)> {
        let mut edges = Vec::new();
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Incoming).detach();
        while let Some((edge, _)) = walk.next(&self.g) {
            if let EdgeData::Control(i) = self.g[edge] {
                edges.push((edge, i));
            }
        }
        edges
    }

    fn address(&self, si: &Self::ActionRef) -> Option<MAddress> {
        if let NodeData::BasicBlock(ref addr) = self.g[*si] {
            Some(*addr)
        } else if let NodeData::DynamicAction = self.g[*si] {
            Some(MAddress::new(u64::MAX, 0))
        } else {
            None
        }
    }

    fn invalid_edge(&self) -> EdgeIndex {
        EdgeIndex::end()
    }
}

impl CFGMod for SSAStorage {

	type BBInfo = MAddress;

    fn mark_start_node(&mut self, si: &Self::ActionRef) {
        self.start_node = *si;
    }

    fn mark_exit_node(&mut self, ei: &Self::ActionRef) {
        self.exit_node = *ei;
    }

    fn add_block(&mut self, info: Self::BBInfo) -> NodeIndex {
        let bb = self.insert_node(NodeData::BasicBlock(info));
        let rs = self.insert_node(NodeData::RegisterState);
        self.insert_edge(bb, rs, EdgeData::RegisterState);
        self.insert_edge(rs, bb, EdgeData::ContainedInBB(info));
        bb
    }

    fn add_dynamic(&mut self) -> NodeIndex {
        let a = self.insert_node(NodeData::DynamicAction);
        let rs = self.insert_node(NodeData::RegisterState);
        self.insert_edge(a, rs, EdgeData::RegisterState);
        self.insert_edge(rs, a, EdgeData::ContainedInBB(MAddress::invalid_address()));
        a
    }

    fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8) {
        self.insert_edge(source, target, EdgeData::Control(index));
    }

    fn remove_block(&mut self, exi: Self::ActionRef) {
        assert!(self.is_block(exi));

        let regstate = self.registers_at(&exi);
        self.remove(regstate);

        let node = exi;
        let mut expressions = Vec::<NodeIndex>::new();
        let mut walk = self.g.neighbors_directed(node, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::ContainedInBB(_) = self.g[edge] {
                expressions.push(othernode);
            }
        }

        for expr in expressions {
            self.remove(expr);
        }

        let preds = self.preds_of(exi);
        self.remove(exi);

        // block removal can make predecessors lose selectors
        for pred_e in preds {
            if self.succs_of(pred_e).len() == 1 {
                let pred = pred_e;
                let mut walk = self.g.neighbors_directed(pred, EdgeDirection::Incoming).detach();
                let mut seledge = None;
                while let Some((edge, _)) = walk.next(&self.g) {
                    if let EdgeData::Selector = self.g[edge] {
                        seledge = Some(edge);
                        break;
                    }
                }
                if let Some(edge) = seledge {
                    self.g.remove_edge(edge);
                }
            }
        }
    }

    fn remove_control_edge(&mut self, edge: Self::CFEdgeRef) {
        self.g.remove_edge(edge);
    }

}

/// ////////////////////////////////////////////////////////////////////////////
/// / Implementation of SSA for SSAStorage.
/// ////////////////////////////////////////////////////////////////////////////

impl SSA for SSAStorage {
	type ValueRef = NodeIndex;

    fn get_address(&self, ni: &NodeIndex) -> MAddress {
        for edge in self.g.edges(*ni) {
            if let EdgeData::ContainedInBB(addr) = *edge.weight() {
                return addr;
            }
        }

        MAddress::invalid_address()
    }

    fn exprs_in(&self, i: &NodeIndex) -> Vec<NodeIndex> {
        let mut expressions = Vec::new();
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::ContainedInBB(addr) = self.g[edge] {
                if let NodeData::Op(_, _) = self.g[othernode] {
                    expressions.push((othernode, addr));
                }
            }
        }

        expressions.sort_by(|a, b| {
            a.1.cmp(&b.1)
        });

        expressions.iter().map(|x| x.0).collect()
    }

    fn is_expr(&self, exi: &NodeIndex) -> bool {
        let i = exi;
        match self.g[*i] {
            NodeData::Op(_, _) => true,
            _ => false,
        }
    }

    fn is_phi(&self, exi: &NodeIndex) -> bool {
        let i = exi;
        match self.g[*i] {
            NodeData::Phi(_, _) => true,
            _ => false,
        }
    }

    fn get_phis(&self, exi: &NodeIndex) -> Vec<NodeIndex> {
        if self.invalid_value() == *exi {
            return Vec::new();
        }
        let i = exi;
        let mut phis = Vec::<NodeIndex>::new();
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::ContainedInBB(_) = self.g[edge] {
                if let NodeData::Phi(_, _) = self.g[othernode] {
                    phis.push(othernode);
                }
            }
        }
        phis
    }

    fn get_uses(&self, i: &NodeIndex) -> Vec<NodeIndex> {
        let mut uses = Vec::new();
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Data(_) = self.g[edge] {
                uses.push(othernode);
            }
        }
        uses
    }

    fn get_block(&self, i: &NodeIndex) -> NodeIndex {
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::ContainedInBB(_) = self.g[edge] {
                return othernode;
            }
        }
        NodeIndex::end()
    }

    fn selects_for(&self, i: &NodeIndex) -> NodeIndex {
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Selector = self.g[edge] {
                return othernode;
            }
        }
        NodeIndex::end()
    }

    fn get_branches(&self, exi: &NodeIndex) -> (NodeIndex, NodeIndex) {
        let selects_for_e = self.selects_for(exi);
        // Make sure that we have a block for the selector.
        assert_ne!(selects_for_e, NodeIndex::end());
        let selects_for = selects_for_e;

        let mut true_branch = NodeIndex::end();
        let mut false_branch = NodeIndex::end();
        let mut walk = self.g.neighbors_directed(selects_for, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Control(0) = self.g[edge] {
                false_branch = othernode;
            } else if let EdgeData::Control(1) = self.g[edge] {
                true_branch = othernode;
            }
        }
        (false_branch, true_branch)
    }

    fn registers_at(&self, exi: &NodeIndex) -> NodeIndex {
        assert!(self.is_action(*exi));
        let i = exi;
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::RegisterState = self.g[edge] {
                return othernode;
            }
        }
        NodeIndex::end()
    }

    fn get_operands(&self, exi: &NodeIndex) -> Vec<NodeIndex> {
        let mut args = self.get_sparse_operands(exi);
        args.sort_by(|a, b| a.0.cmp(&b.0));
        args.iter().map(|a| a.1).collect()
    }

    fn get_sparse_operands(&self, exi: &NodeIndex) -> Vec<(u8, NodeIndex)> {
        let i = exi;
        let mut args = Vec::new();
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Data(index) = self.g[edge] {
                args.push((index, othernode));
            }
        }
        args
    }

    fn get_node_data(&self, i: &NodeIndex) -> Result<TNodeData, Box<Debug>> {
        match self.g[*i] {
            NodeData::Op(opc, vt) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Op(opc),
            }),
            NodeData::Phi(vt, _) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Phi,
            }),
            NodeData::Comment(vt, ref s) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Comment(s.clone()),
            }),
            NodeData::Undefined(vt) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Undefined,
            }),
            NodeData::Removed |
            NodeData::Unreachable |
            NodeData::BasicBlock(_) |
            NodeData::DynamicAction |
            NodeData::RegisterState => Err(Box::new(self.g[*i].clone())),
        }
    }

    fn get_comment(&self, i: &NodeIndex) -> Option<String> {
        if let Ok(ndata) = self.get_node_data(i) {
            if let TNodeType::Comment(s) = ndata.nt {
                Some(s.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn get_opcode(&self, i: &NodeIndex) -> Option<MOpcode> {
        if let Ok(ndata) = self.get_node_data(i) {
            if let TNodeType::Op(opc) = ndata.nt {
                Some(opc.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn is_selector(&self, exi: &Self::ValueRef) -> bool {
        let i = exi;
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Incoming).detach();
        while let Some((edge, _)) = walk.next(&self.g) {
            if let EdgeData::Selector = self.g[edge] {
                return true;
            }
        }
        false
    }


    fn selector_of(&self, i: &Self::ActionRef) -> Option<Self::ValueRef> {
        let mut walk = self.g.neighbors_directed(*i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Selector = self.g[edge] {
                return Some(othernode);
            }
        }
        None
    }

    fn get_target(&self, exi: &NodeIndex) -> NodeIndex {
        let cur_block = self.get_block(exi);
        let mut walk = self.g.neighbors_directed(cur_block, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Control(_) = self.g[edge] {
                return othernode;
            }
        }
        NodeIndex::end()
    }

    fn args_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
        self.gather_adjacent(node, EdgeDirection::Outgoing, true)
    }

    fn uses_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
        self.gather_adjacent(node, EdgeDirection::Incoming, true)
    }

    fn invalid_value(&self) -> NodeIndex {
        NodeIndex::end()
    }
    fn to_value(&self, n: NodeIndex) -> NodeIndex {
        n
    }
    fn to_action(&self, n: NodeIndex) -> NodeIndex {
        n
    }

    fn nodes(&self) -> Vec<NodeIndex> {
        self.valid_nodes()
    }

    fn node_count(&self) -> usize {
        self.g.node_count()
    }

    fn edge_count(&self) -> usize {
        self.g.edge_count()
    }
}

impl SSAMod for SSAStorage {
    fn set_addr(&mut self, i: &Self::ValueRef, addr: MAddress) {
        let mut e = None;
        for edge in self.g.edges(*i) {
            if let EdgeData::ContainedInBB(_) = *edge.weight() {
                e = Some(edge.id());
                break;
            }
        }

        if let Some(edge) = e {
            if let &mut EdgeData::ContainedInBB(ref mut x) = &mut self.g[edge] {
                *x = addr;
            }
        }
    }

    fn add_op(&mut self, opc: MOpcode, vt: ValueType, _: Option<u64>) -> NodeIndex {
        self.insert_node(NodeData::Op(opc, vt))
    }

    fn add_const(&mut self, value: u64) -> NodeIndex {

        let data = NodeData::Op(MOpcode::OpConst(value),
                                ValueType::Integer { width: 64 });

        self.insert_node(data)
    }

    fn add_phi(&mut self, vt: ValueType) -> NodeIndex {
        self.insert_node(NodeData::Phi(vt, "".to_owned()))
    }

    fn add_undefined(&mut self, vt: ValueType) -> NodeIndex {
        self.insert_node(NodeData::Undefined(vt))
    }

    fn add_comment(&mut self, vt: ValueType, msg: String) -> NodeIndex {
        self.insert_node(NodeData::Comment(vt, msg))
    }

    fn mark_selector(&mut self, node: Self::ValueRef, block: Self::ActionRef) {
        self.insert_edge(block, node, EdgeData::Selector);
    }

    fn phi_use(&mut self, phi: NodeIndex, node: NodeIndex) {
        self.insert_edge(phi, node, EdgeData::Data(0));
    }

    fn phi_unuse(&mut self, phi: NodeIndex, node: NodeIndex) {
        self.delete_edge(phi, node);
    }

    fn op_use(&mut self, node: NodeIndex, index: u8, argument: NodeIndex) {
        if argument == NodeIndex::end() {
            return;
        }
        self.insert_edge(node, argument, EdgeData::Data(index));
    }

    fn disconnect(&mut self, op: &NodeIndex, operand: &NodeIndex) {
        self.delete_edge(*op, *operand);
    }

    fn replace(&mut self, node: NodeIndex, replacement: NodeIndex) {
        //self.replace_node(node, replacement);
        if let Some(adata) = self.assoc_data.remove(&node) {
            self.assoc_data.insert(replacement, adata);
        }
        self.replace_node(node, replacement);
    }

    fn remove(&mut self, node: NodeIndex) {
        self.assoc_data.remove(&node);
        self.remove_node(node);
    }

    fn remove_edge(&mut self, i: &Self::CFEdgeRef) {
        let src_node = self.source_of(i);
        if let Some(selector) = self.selector_of(&src_node) {
            self.remove(selector);
        }

        let other_edge = match self.g[*i] {
            EdgeData::Control(j) if j <= 2 => {
                match j {
                    0 => Some(self.true_edge_of(&src_node)),
                    1 => Some(self.false_edge_of(&src_node)),
                    2 => None,
                    _ => unreachable!(),
                }
            }
            _ => panic!("Found something other than a control edge!"),
        };

        if let Some(oe) = other_edge {
            if oe != EdgeIndex::end() {
                let wt = self.g.edge_weight_mut(oe).expect("No weight found!");
                *wt = EdgeData::Control(2);
            }
        }

        self.g.remove_edge(*i);
    }

    fn add_to_block(&mut self, node: Self::ValueRef, block: Self::ActionRef, at: MAddress) {
        self.insert_edge(node, block, EdgeData::ContainedInBB(at));
    }

    fn map_registers(&mut self, regs: Vec<String>) {
        self.regnames = regs;
    }
}

impl SSAExtra for SSAStorage {
    fn mark(&mut self, i: &Self::ValueRef) {
        radeco_trace!(logger::Event::SSAMarkNode(i));
        let data = self.assoc_data.entry(*i).or_insert_with(AdditionalData::new);
        data.mark = true;
    }

    fn clear_mark(&mut self, i: &Self::ValueRef) {
        radeco_trace!(logger::Event::SSAClearMark(i));
        if let Some(ref mut data) = self.assoc_data.get_mut(i) {
            data.mark = false;
        }
    }

    fn set_color(&mut self, i: &Self::ValueRef, color: u8) {
        let data = self.assoc_data.entry(*i).or_insert_with(AdditionalData::new);
        data.color = Some(color);
    }

    fn set_comment(&mut self, i: &Self::ValueRef, comment: String) {
        let data = self.assoc_data.entry(*i).or_insert_with(AdditionalData::new);
        data.comments = Some(comment);
    }

    fn set_register(&mut self, i: &Self::ValueRef, regname: RegInfo) {
        let data = self.assoc_data.entry(*i).or_insert_with(AdditionalData::new);
        data.register = Some(regname);
    }



    fn add_flag(&mut self, i: &Self::ValueRef, f: String) {
        let data = self.assoc_data.entry(*i).or_insert_with(AdditionalData::new);
        data.flag = Some(f);
    }

    fn is_marked(&self, i: &Self::ValueRef) -> bool {
        self.assoc_data.get(i).unwrap_or(&AdditionalData::new()).mark
    }

    fn color(&self, i: &Self::ValueRef) -> Option<u8> {
        self.assoc_data.get(i).and_then(|data| data.color)
    }

    fn comments(&self, i: &Self::ValueRef) -> Option<String> {
        self.assoc_data.get(i).and_then(|data| data.comments.clone())
    }

    fn register(&self, i: &Self::ValueRef) -> Option<RegInfo> {
        self.assoc_data.get(i).and_then(|data| data.register.clone())
    }

    fn addr(&self, i: &Self::ValueRef) -> Option<String> {
        for edge in self.g.edges(*i) {
            if let EdgeData::ContainedInBB(addr) = *edge.weight() {
                return Some(format!("{}", addr))
            }
        }
        None
    }

    fn flags(&self, i: &Self::ValueRef) -> Option<String> {
        self.assoc_data.get(i).and_then(|data| data.flag.clone())
    }
}

#[derive(Clone, Copy, Debug)]
struct InorderKey {
    pub address: MAddress,
    pub value: NodeIndex,
}

impl InorderKey {
    pub fn new(addr: MAddress, node: NodeIndex) -> InorderKey {
        InorderKey {
            address: addr,
            value: node,
        }
    }
}

impl PartialOrd for InorderKey {
    fn partial_cmp(&self, other: &InorderKey) -> Option<Ordering> {
        let c = other.address.address.cmp(&self.address.address);
        match c {
            Ordering::Equal => Some(other.address.offset.cmp(&self.address.offset)),
            _ => Some(c),
        }
    }
}

impl Eq for InorderKey { }

impl Ord for InorderKey {
    fn cmp(&self, other: &InorderKey) -> Ordering {
        let c = other.address.address.cmp(&self.address.address);
        match c {
            Ordering::Equal => other.address.offset.cmp(&self.address.offset),
            _ => c,
        }
    }
}

impl PartialEq for InorderKey {
    fn eq(&self, other: &InorderKey) -> bool {
        match self.address.address.cmp(&other.address.address) {
            Ordering::Equal => {
                match self.address.offset.cmp(&other.address.offset) {
                    Ordering::Equal => true,
                    _ => false,
                }
            }
            _ => false,
        }
    }
}


impl SSAWalk<Walker> for SSAStorage {
    fn bfs_walk(&self) -> Walker {
        let mut walker = Walker { nodes: VecDeque::new() };
        {
            let mut visited = HashSet::new();
            let mut explorer = VecDeque::new();
            explorer.push_back(self.start_node());
            let nodes = &mut walker.nodes;
            while let Some(ref block) = explorer.pop_front() {
                if visited.contains(block) {
                    continue;
                }
                visited.insert(*block);
                nodes.push_back(self.to_value(*block));
                let mut exprs = self.exprs_in(block)
                                    .iter()
                                    .chain(self.get_phis(block).iter())
                                    .cloned()
                                    .collect::<Vec<NodeIndex>>();

                exprs.sort_by(|x, y| {
                    let addr_x = self.get_address(x);
                    let addr_y = self.get_address(y);
                    addr_x.cmp(&addr_y)
                });

                for expr in &exprs {
                    nodes.push_back(*expr);
                }
                let mut outgoing = self.edges_of(block);
                outgoing.sort_by(|a, b| (a.1).cmp(&b.1));
                explorer.extend(outgoing.iter().map(|x| self.target_of(&x.0)));
            }
        }
        walker
    }

    fn inorder_walk(&self) -> Walker {
        let mut walker = Walker { nodes: VecDeque::new() };
        {
            let mut visited = HashSet::new();
            let mut explorer = BinaryHeap::<InorderKey>::new();
            explorer.push(InorderKey::new(MAddress::new(0, 0), self.start_node()));
            let nodes = &mut walker.nodes;
            while let Some(ref key) = explorer.pop() {
                let block = &key.value;
                if visited.contains(block) {
                    continue;
                }
                visited.insert(*block);
                nodes.push_back(self.to_value(*block));
                let mut exprs = self.exprs_in(block)
                                    .iter()
                                    .chain(self.get_phis(block).iter())
                                    .cloned()
                                    .collect::<Vec<NodeIndex>>();

                exprs.sort_by(|x, y| {
                    let addr_x = self.get_address(x);
                    let addr_y = self.get_address(y);
                    addr_x.cmp(&addr_y)
                });
                for expr in &exprs {
                    nodes.push_back(*expr);
                }
                for outedge in self.edges_of(block) {
                    let target = self.target_of(&outedge.0);
                    let addr = self.address(&target).unwrap();
                    let key = InorderKey::new(addr, target);
                    explorer.push(key);
                }
            }
        }
        walker
    }

    fn dfs_walk(&self) -> Walker {
        unimplemented!()
    }
}

// Iterators for `SSAStorage`
pub struct Walker {
    pub nodes: VecDeque<NodeIndex>,
}

impl Iterator for Walker {
    type Item = NodeIndex;
    fn next(&mut self) -> Option<NodeIndex> {
        self.nodes.pop_front()
    }
}
