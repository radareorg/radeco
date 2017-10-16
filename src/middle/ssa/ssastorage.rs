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
use petgraph::visit::{IntoEdgeReferences, IntoNodeReferences, EdgeRef};
use petgraph::EdgeDirection;
use petgraph::stable_graph::StableDiGraph;
use petgraph::graph::{EdgeIndex,  NodeIndex};
use middle::ir::{self, MAddress, MOpcode};

use super::ssa_traits::NodeData as TNodeData;
use super::ssa_traits::NodeType as TNodeType;
use super::ssa_traits::{SSA, SSAExtra, SSAMod, SSAWalk, ValueInfo};
use super::cfg_traits::{CFG, CFGMod};
use super::graph_traits::{Graph, EdgeInfo, ConditionInfo};
use utils::logger;

/// Structure that represents data that maybe associated with an node in the
/// SSA
#[derive(Clone, Debug)]
pub struct AdditionalData {
    comments: Option<String>,
    flag: Option<String>,
    mark: bool,
    color: Option<u8>,
}

impl AdditionalData {
    fn new() -> AdditionalData {
        AdditionalData {
            comments: None,
            flag: None,
            mark: false,
            color: None,
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
        }
    }
}

pub type AssociatedData = HashMap<NodeIndex, AdditionalData>;

/// Both actions and values are represented using this same enum.
///
/// Value nodes are `Op`, `Phi`, `Comment`, `Undefined` and `Removed`.
/// Action nodes are `Unreachable`, `BasicBlock`, `DynamicAction`
/// `RegisterState` is neither.
/// Value nodes have a `ValueInfo` that can be extracted with
/// `SSA::get_node_data`
#[derive(Clone, Debug)]
pub enum NodeData {
    /// Represents on operation.
    Op(MOpcode, ValueInfo),
    /// Represents a phi node.
    Phi(ValueInfo, String),
    /// Represents an undefined node with a comment.
    Comment(ValueInfo, String),
    /// Represents an undefined node without comment.
    Undefined(ValueInfo),
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

impl NodeData {
    pub fn set_valueinfo(&mut self, vi: ValueInfo) {
        match *self {
            NodeData::Op(_, ref mut vif) |
            NodeData::Phi(ref mut vif, _) |
            NodeData::Comment(ref mut vif, _) => *vif = vi,
            _ => {},
        }
    }

    pub fn get_valueinfo(&self) -> Option<&ValueInfo> {
        match self {
            &NodeData::Op(_, ref vif) |
            &NodeData::Phi(ref vif, _) |
            &NodeData::Comment(ref vif, _) => Some(vif),
            _ => None,
        }
    }

    pub fn get_valueinfo_mut(&mut self) -> Option<&mut ValueInfo> {
        match *self {
            NodeData::Op(_, ref mut vif) |
            NodeData::Phi(ref mut vif, _) |
            NodeData::Comment(ref mut vif, _) => Some(vif),
            _ => None,
        }
    }
}

// Implement display helper for NodeData to make it a little nicer to read prefix notation.
impl fmt::Display for NodeData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            NodeData::Op(ref op, _) => format!("{}", op),
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
    /// Edeg from value or RegisterState to comment. Represent register infor-
    /// mation for every value.
    RegisterInfo,
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
    entry_node: NodeIndex,
    exit_node: NodeIndex,
    pub assoc_data: AssociatedData,
    pub regnames: Vec<String>,
    pub constants: HashMap<u64, NodeIndex>,
}

impl default::Default for SSAStorage {
    fn default() -> SSAStorage {
        SSAStorage {
            g: StableDiGraph::new(),
            entry_node: NodeIndex::end(),
            exit_node: NodeIndex::end(),
            assoc_data: HashMap::new(),
            regnames: Vec::new(),
            constants: HashMap::new(),
        }
    }
}

impl SSAStorage {
    pub fn new() -> SSAStorage {
        SSAStorage {
            g: StableDiGraph::new(),
            entry_node: NodeIndex::end(),
            exit_node: NodeIndex::end(),
            assoc_data: HashMap::new(),
            regnames: Vec::new(),
            constants: HashMap::new(),
        }
    }

}

/// //////////////////////////////////////////////////////////////////////////
/// //// Revised API for SSAStorage.
/// //////////////////////////////////////////////////////////////////////////

/// Regulate all access to SSAStorage, especially insertions and deletions
/// through this API to prevent stablemap from being out of sync.
impl Graph for SSAStorage {
    type GraphNodeRef = NodeIndex;
    type GraphEdgeRef = EdgeIndex;

    type NodeData = NodeData;
    type EdgeData = EdgeData;

    fn nodes(&self) -> Vec<Self::GraphNodeRef> {
        self.g.node_indices().collect()
    }

    fn nodes_count(&self) -> usize {
        self.g.node_count()
    }

    fn edges_count(&self) -> usize {
        self.g.edge_count()
    }

    fn edge_info(&self, i: Self::GraphEdgeRef) -> Option<EdgeInfo<Self::GraphNodeRef>> {
        if let Some(edge_data) = self.g.edge_references().find(|x| x.id() == i) {
            Some(EdgeInfo::new(edge_data.source(), edge_data.target()))
        } else {
            Some(EdgeInfo::new(NodeIndex::end(), NodeIndex::end()))
        }
    }

    fn insert_node(&mut self, d: Self::NodeData) -> Option<Self::GraphNodeRef> {
        let ret = self.g.add_node(d);
        radeco_trace!(logger::Event::SSAInsertNode(&ret));
        if ret == NodeIndex::end() {
            None
        } else {
            Some(ret)
        }
    }

    fn remove_node(&mut self, exi: Self::GraphNodeRef) {
        radeco_trace!(logger::Event::SSARemoveNode(&exi));
        // Remove the current association.
        if let Some(val) = self.constant(exi) {
            let uses = self.uses_of(exi);
            if uses.is_empty() {
                self.g.remove_node(exi);
                self.constants.remove(&val);
            }
        } else {
            self.g.remove_node(exi);
        }
    }

    // TODO: When i is a constant, there may be some bugs.
    fn replace_node(&mut self, i: Self::GraphNodeRef, j: Self::GraphNodeRef) {
        radeco_trace!(logger::Event::SSAReplaceNode(&i, &j));
        // Before replace, we need to copy over the edges.
        assert!(self.constant(i).is_none());

        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Data(d) = self.g[edge] {
                if othernode == j {
                    // Avoid recursion use
                    self.g.remove_edge(edge);
                    continue;
                }
                match self.g[othernode] {
                    NodeData::Op(_, _) | NodeData::RegisterState => {
                        self.op_use(othernode, d, j);
                    }
                    NodeData::Phi(_, _) => {
                        self.phi_use(othernode, j);
                    }
                    _ => {  }
                }
            } else if let EdgeData::Selector = self.g[edge] {
                let bb = self.block_for(i)
                             .expect("Value node does'n belong to any block");
                self.set_selector(j, bb);
            }
            self.g.remove_edge(edge); // TODO: Need suggestion. Remove edges? if not, there might be an operation to get successor on the preds(i) which would result in access to this deleted node
        }

        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            match self.g[edge] {
                EdgeData::RegisterInfo => {
                    if othernode == j {
                        self.g.remove_edge(edge);
                        continue;
                    }
                    self.insert_edge(j, othernode, EdgeData::RegisterInfo);
                }
                _ => {  }
            }
            self.g.remove_edge(edge);
        }

        if self.entry_node == i {
            self.entry_node = j;
        }
        self.remove_node(i);
    }

    fn insert_edge(&mut self, i: Self::GraphNodeRef, j: Self::GraphNodeRef, e: Self::EdgeData) -> Option<Self::GraphEdgeRef> {
        //Don't insert a duplicate edge between i and j with same EdgeData
        let edges = self.find_edges_between(i, j);
        let mut flag = false;
        let mut exist_edge: EdgeIndex = EdgeIndex::end();
        for edge in edges {
            if edge != self.invalid_edge().expect("Invalid Edge is not defined") {
                flag = match (self.g[edge], e) {
                    (EdgeData::Control(i), EdgeData::Control(j)) |
                    (EdgeData::Data(i), EdgeData::Data(j)) => {
                        if i == j {
                            exist_edge = edge;
                            true
                        } else {
                            false
                        }
                    },
                    (EdgeData::ContainedInBB(i), EdgeData::ContainedInBB(j)) => {
                        if i == j {
                            exist_edge = edge;
                            true
                        } else {
                            false
                        }
                    },
                    (EdgeData::RegisterInfo, EdgeData::RegisterInfo) |
                    (EdgeData::Selector, EdgeData::Selector) |
                    (EdgeData::RegisterState, EdgeData::RegisterState) |
                    (EdgeData::ReplacedBy, EdgeData::ReplacedBy) => {
                        exist_edge = edge;
                        true
                    },
                    _ => false,
                };
                if flag {
                    break;
                }
            }
        }
        if flag {
            Some(exist_edge)
        } else {
            radeco_trace!(logger::Event::SSAInsertEdge(&i, &j));
            Some(self.g.add_edge(i, j, e))
        }
    }

    fn update_edge(&mut self, i: Self::GraphNodeRef, j: Self::GraphNodeRef, e: Self::EdgeData) -> Option<Self::GraphEdgeRef> {
        radeco_trace!(logger::Event::SSAUpdateEdge(&i, &j));
        Some(self.g.update_edge(i, j, e))
    }

    // It's possible that two same nodes have mutilple edges
    fn find_edges_between(&self, source: Self::GraphNodeRef, target: Self::GraphNodeRef) -> Vec<Self::GraphEdgeRef> {
        if Some(source) == self.invalid_value() || Some(target) == self.invalid_value() {
            return vec![];
        }

        let mut edges: Vec<EdgeIndex> = Vec::new();
        let mut walk = self.g.neighbors_directed(source, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if othernode == target {
                edges.push(edge);
            }
        }
        edges
    }

    // It's possible that there are mutilple edges between two same nodes. like: xor eax, eax
    fn remove_edges_between(&mut self, i: Self::GraphNodeRef, j: Self::GraphNodeRef) {
        radeco_trace!(logger::Event::SSARemoveEdge(&i, &j));
        while let Some(ei) = self.g.find_edge(i, j) {
            self.g.remove_edge(ei);
        }
    }

    fn gather_adjacences(&self,
                       node: Self::GraphNodeRef,
                       direction: EdgeDirection,
                       data: bool)
                       -> Vec<Self::GraphNodeRef> {
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
}

/// ////////////////////////////////////////////////////////////////////////////
/// / Implementation of CFG for SSAStorage.
/// ////////////////////////////////////////////////////////////////////////////

impl CFG for SSAStorage {
    type ActionRef = <SSAStorage as Graph>::GraphNodeRef;
    type CFEdgeRef = <SSAStorage as Graph>::GraphEdgeRef;

    fn is_block(&self, node: Self::ActionRef) -> bool {
        if let NodeData::BasicBlock(_) = self.g[node] {
            true
        } else {
            false
        }
    }

    fn is_action(&self, action: Self::ActionRef) -> bool {
        match self.g[action] {
            NodeData::BasicBlock(_) | NodeData::DynamicAction => true,
            _ => false,
        }
    }

    fn blocks(&self) -> Vec<Self::ActionRef> {
        let mut blocks = self.nodes();
        blocks.retain(|x| self.is_block(*x));
        blocks
    }

    fn entry_node(&self) -> Option<Self::ActionRef> {
        if self.entry_node == NodeIndex::end() {
            None
        } else {
            Some(self.entry_node)
        }
    }

    fn exit_node(&self) -> Option<Self::ActionRef> {
        if self.exit_node == NodeIndex::end() {
            None
        } else {
            Some(self.exit_node)
        }
    }

    fn preds_of(&self, exi: Self::ActionRef) -> Vec<Self::ActionRef> {
        self.gather_adjacences(exi, EdgeDirection::Incoming, false)
    }

    fn succs_of(&self, exi: Self::ActionRef) -> Vec<Self::ActionRef> {
        self.gather_adjacences(exi, EdgeDirection::Outgoing, false)
    }

    fn unconditional_block(&self, i: Self::ActionRef) -> Option<Self::ActionRef> {
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Control(2) = self.g[edge] {
                return Some(othernode);
            }
        }
        None
    }

    fn conditional_blocks(&self, i: Self::ActionRef) -> Option<ConditionInfo<Self::ActionRef>> {
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        let mut false_block = NodeIndex::end();
        let mut true_block = NodeIndex::end();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            match self.g[edge] {
                EdgeData::Control(0) => false_block = othernode,
                EdgeData::Control(1) => true_block = othernode,
                EdgeData::Control(_) => return None,
                _ => {},
            } 
        }
        if false_block == NodeIndex::end() || true_block == NodeIndex::end() {
            None
        } else {
            Some(ConditionInfo::new(true_block, false_block))
        }
    }

    fn invalid_action(&self) -> Option<Self::ActionRef> {
        Some(NodeIndex::end())
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Edge accessors and helpers
    ///////////////////////////////////////////////////////////////////////////

    fn conditional_edges(&self, i: Self::ActionRef) -> Option<ConditionInfo<Self::CFEdgeRef>> {
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        let mut false_edge = EdgeIndex::end();
        let mut true_edge = EdgeIndex::end();
        while let Some((edge, _)) = walk.next(&self.g) {
            match self.g[edge] {
                EdgeData::Control(0) => false_edge = edge,
                EdgeData::Control(1) => true_edge = edge,
                EdgeData::Control(_) => return None,
                _ => {},
            } 
        }
        if false_edge == EdgeIndex::end() || true_edge == EdgeIndex::end() {
            None
        } else {
            Some(ConditionInfo::new(true_edge, false_edge))
        }
    }

    fn unconditional_edge(&self, i: Self::ActionRef) -> Option<Self::CFEdgeRef> {
        let edges = self.outgoing_edges(i);
        for &(edge, ety) in &edges {
            if ety == 2 {
                return Some(edge);
            }
        }
        None
    }

    fn incoming_edges(&self, i: NodeIndex) -> Vec<(EdgeIndex, u8)> {
        let mut edges = Vec::new();
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Incoming).detach();
        while let Some((edge, _)) = walk.next(&self.g) {
            if let EdgeData::Control(i) = self.g[edge] {
                edges.push((edge, i));
            }
        }
        edges
    }

    fn outgoing_edges(&self, i: NodeIndex) -> Vec<(EdgeIndex, u8)> {
        let mut edges = Vec::new();
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, _)) = walk.next(&self.g) {
            if let EdgeData::Control(i) = self.g[edge] {
                edges.push((edge, i));
            }
        }
        edges
    }

    fn starting_address(&self, si: Self::ActionRef) -> Option<MAddress> {
        if let NodeData::BasicBlock(ref addr) = self.g[si] {
            Some(*addr)
        } else if let NodeData::DynamicAction = self.g[si] {
            Some(MAddress::new(u64::MAX, 0))
        } else {
            None
        }
    }

    fn invalid_edge(&self) -> Option<Self::CFEdgeRef> {
        Some(EdgeIndex::end())
    }
}

impl CFGMod for SSAStorage {

    type BBInfo = MAddress;

    fn set_entry_node(&mut self, si: Self::ActionRef) {
        self.entry_node = si;
    }

    fn set_exit_node(&mut self, ei: Self::ActionRef) {
        self.exit_node = ei;
    }

    fn insert_block(&mut self, info: Self::BBInfo) -> Option<Self::ActionRef> {
        let bb = self.insert_node(NodeData::BasicBlock(info)).expect("Cannot insert new nodes");
        let rs = self.insert_node(NodeData::RegisterState).expect("Cannot insert new nodes");
        self.insert_edge(bb, rs, EdgeData::RegisterState);
        self.insert_edge(rs, bb, EdgeData::ContainedInBB(info));
        Some(bb)
    }

    fn insert_dynamic(&mut self) -> Option<Self::ActionRef> {
        let a = self.insert_node(NodeData::DynamicAction).expect("Cannot insert new nodes");
        let rs = self.insert_node(NodeData::RegisterState).expect("Cannot insert new nodes");
        self.insert_edge(a, rs, EdgeData::RegisterState);
        self.insert_edge(rs, a, EdgeData::ContainedInBB(MAddress::invalid_address()));
        Some(a)
    }

    fn insert_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8) -> Option<Self::CFEdgeRef> {
        self.insert_edge(source, target, EdgeData::Control(index))
    }

    fn remove_block(&mut self, exi: Self::ActionRef) {
        assert!(self.is_block(exi));

        let regstate = self.registers_in(exi)
                                .expect("No register state node found");
        self.remove_value(regstate);

        let node = exi;
        let mut expressions = Vec::<NodeIndex>::new();
        let mut walk = self.g.neighbors_directed(node, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::ContainedInBB(_) = self.g[edge] {
                expressions.push(othernode);
            }
        }

        for expr in expressions {
            self.remove_value(expr);
        }

        let preds = self.preds_of(exi);
        self.remove_value(exi);

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

    fn is_expr(&self, exi: Self::ValueRef) -> bool {
        let i = exi;
        match self.g[i] {
            NodeData::Op(_, _) => true,
            _ => false,
        }
    }

    fn is_phi(&self, exi: Self::ValueRef) -> bool {
        let i = exi;
        match self.g[i] {
            NodeData::Phi(_, _) => true,
            _ => false,
        }
    }

    fn is_selector(&self, exi: Self::ValueRef) -> bool {
        let i = exi;
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Incoming).detach();
        while let Some((edge, _)) = walk.next(&self.g) {
            if let EdgeData::Selector = self.g[edge] {
                return true;
            }
        }
        false
    }

    fn address(&self, ni: Self::ValueRef) -> Option<MAddress> {
        for edge in self.g.edges(ni) {
            if let EdgeData::ContainedInBB(addr) = *edge.weight() {
                return Some(addr);
            }
        }
        None
    }

    fn values(&self) -> Vec<Self::ValueRef> {
        let mut exprs = self.nodes();
        exprs.retain(|x| !self.is_action(*x));
        exprs
    }

    fn exprs_in(&self, i: Self::ActionRef) -> Vec<Self::ValueRef> {
        let mut expressions = Vec::new();
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Incoming).detach();
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

    fn phis_in(&self, exi: Self::ActionRef) -> Vec<Self::ValueRef> {
        if self.invalid_value() == Some(exi) {
            return Vec::new();
        }
        let i = exi;
        let mut phis = Vec::<NodeIndex>::new();
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::ContainedInBB(_) = self.g[edge] {
                if let NodeData::Phi(_, _) = self.g[othernode] {
                    phis.push(othernode);
                }
            }
        }
        phis
    }

    fn registers_in(&self, exi: Self::ActionRef) -> Option<Self::ValueRef> {
        assert!(self.is_action(exi));
        let i = exi;
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::RegisterState = self.g[edge] {
                return Some(othernode);
            }
        }
        None
    }

    fn selector_in(&self, i: Self::ActionRef) -> Option<Self::ValueRef> {
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Selector = self.g[edge] {
                return Some(othernode);
            }
        }
        None
    }

    fn selector_for(&self, i: Self::ValueRef) -> Option<Self::ActionRef> {
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Incoming).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Selector = self.g[edge] {
                return Some(othernode);
            }
        }
        None
    }

    fn uses_of(&self, node: Self::ValueRef) -> Vec<Self::ValueRef> {
        self.gather_adjacences(node, EdgeDirection::Incoming, true)
    }

    fn operands_of(&self, exi: Self::ValueRef) -> Vec<Self::ValueRef> {
        let mut args = self.sparse_operands_of(exi);
        args.sort_by(|a, b| a.0.cmp(&b.0));
        args.iter().map(|a| a.1).collect()
    }

    fn sparse_operands_of(&self, exi: Self::ValueRef) -> Vec<(u8, Self::ValueRef)> {
        let i = exi;
        let mut args = Vec::new();
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::Data(index) = self.g[edge] {
                args.push((index, othernode));
            }
        }
        args
    }

    fn block_for(&self, i: Self::ValueRef) -> Option<Self::ActionRef> {
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            if let EdgeData::ContainedInBB(_) = self.g[edge] {
                return Some(othernode);
            }
        }
        None
    }

    fn node_data(&self, i: Self::ValueRef) -> Result<TNodeData, Box<Debug>> {

        match self.g.node_weight(i) {
            Some(&NodeData::Op(ref opc, vt)) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Op(opc.clone()),
            }),
            Some(&NodeData::Phi(vt, _)) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Phi,
            }),
            Some(&NodeData::Comment(vt, ref s)) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Comment(s.clone()),
            }),
            Some(&NodeData::Undefined(vt)) => Ok(TNodeData {
                vt: vt,
                nt: TNodeType::Undefined,
            }),
            Some(&NodeData::Removed) |
            Some(&NodeData::Unreachable) |
            Some(&NodeData::BasicBlock(_)) |
            Some(&NodeData::DynamicAction) |
            Some(&NodeData::RegisterState) => Err(Box::new(self.g[i].clone())),
            None => Err(Box::new("Invalid Node Index")),
        }
    }

    fn constant(&self, ni: Self::ValueRef) -> Option<u64> {
        if let NodeData::Op(MOpcode::OpConst(n), _) = self.g[ni] {
            Some(n)
        } else {
            None
        }
    }

    fn comment(&self, i: Self::ValueRef) -> Option<String> {
        if let Ok(ndata) = self.node_data(i) {
            if let TNodeType::Comment(s) = ndata.nt {
                Some(s.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn registers(&self, i: Self::ValueRef) -> Vec<String> {
        let mut regs = Vec::new();
        // Self-loop in RadecoIL is not welcomed ;D
        // Thus, Comment Node in entry_node will not have a RegisterInfo edge pointing to itself.
        if let Some(s) = self.comment(i) {
            if self.block_for(i) == self.entry_node() {
                regs.push(s);
            }
        } 
        let mut walk = self.g.neighbors_directed(i, EdgeDirection::Outgoing).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            match self.g[edge] {
                EdgeData::RegisterInfo => {
                    if let Some(regname) = self.comment(othernode) {
                        regs.push(regname);
                    }
                }
                _ => {  }
            }
        }
        regs
    }

    fn opcode(&self, i: Self::ValueRef) -> Option<MOpcode> {
        if let Ok(ndata) = self.node_data(i) {
            if let TNodeType::Op(opc) = ndata.nt {
                Some(opc.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn invalid_value(&self) -> Option<Self::ValueRef> {
        Some(NodeIndex::end())
    }
}

impl SSAMod for SSAStorage {
    fn set_address(&mut self, i: Self::ValueRef, addr: MAddress) {
        let mut e = None;
        for edge in self.g.edges(i) {
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

    fn set_register(&mut self, i: Self::ValueRef, regname: String) {
        let reg_state = self.registers_in(self.entry_node)
                                .expect("No register state node found");
        let operands = self.operands_of(reg_state);
        for op in operands {
            if Some(regname.clone()) == self.comment(op) {
                self.insert_edge(i, op, EdgeData::RegisterInfo);
                break;
            }
        }
    }

    fn set_selector(&mut self, node: Self::ValueRef, block: Self::ActionRef) {
        self.insert_edge(block, node, EdgeData::Selector);
    }

    fn insert_op(&mut self, opc: MOpcode, vt: ValueInfo, _: Option<u64>) ->
        Option<Self::ValueRef> {
        Some(self.insert_node(NodeData::Op(opc, vt)).expect("Cannot insert new nodes!"))
    }

    fn insert_const(&mut self, value: u64) -> Option<Self::ValueRef> {
        if self.constants.contains_key(&value) {
            Some(self.constants.get(&value).unwrap().clone())
        } else {
            let data = NodeData::Op(MOpcode::OpConst(value),
                                    scalar!(64));
            let id = self.insert_node(data).expect("Cannot insert new nodes");
            self.constants.insert(value, id);
            Some(id)
        }
    }

    fn insert_phi(&mut self, vt: ValueInfo) -> Option<Self::ValueRef> {
        self.insert_node(NodeData::Phi(vt, "".to_owned()))
    }

    fn insert_undefined(&mut self, vt: ValueInfo) -> Option<Self::ValueRef> {
        self.insert_node(NodeData::Undefined(vt))
    }

    fn insert_comment(&mut self, vt: ValueInfo, msg: String) -> Option<Self::ValueRef> {
        self.insert_node(NodeData::Comment(vt, msg))
    }

    fn insert_into_block(&mut self, node: Self::ValueRef, block: Self::ActionRef, at: MAddress) {
        self.insert_edge(node, block, EdgeData::ContainedInBB(at));
    }

    fn phi_use(&mut self, phi: Self::ValueRef, node: Self::ValueRef) {
        self.insert_edge(phi, node, EdgeData::Data(0));
    }

    fn phi_unuse(&mut self, phi: Self::ValueRef, node: Self::ValueRef) {
        self.remove_edges_between(phi, node);
    }

    fn op_use(&mut self, node: Self::ValueRef, index: u8, argument: Self::ValueRef) {
        if argument == NodeIndex::end() {
            return;
        }
        // A node cannot use itself.
        assert_ne!(node, argument);
        self.insert_edge(node, argument, EdgeData::Data(index));
    }

    fn op_unuse(&mut self, op: Self::ValueRef, operand: Self::ValueRef) {
        self.remove_edges_between(op, operand);
    }

    fn replace_value(&mut self, node: Self::ValueRef, replacement: Self::ValueRef) {
        //self.replace_node(node, replacement);
        if let Some(adata) = self.assoc_data.remove(&node) {
            self.assoc_data.insert(replacement, adata);
        }
        self.replace_node(node, replacement);
    }

    fn remove_value(&mut self, node: Self::ValueRef) {
        self.assoc_data.remove(&node);
        // We should remove the edges which are associated with node.
        let mut walk = self.g.neighbors_undirected(node).detach();
        while let Some((edge, _)) = walk.next(&self.g) {
            self.g.remove_edge(edge);
        }
        self.remove_node(node);
    }

    fn remove_data_edge(&mut self, i: Self::CFEdgeRef) {
        let src_node = self.edge_info(i).expect("Less-endpoints edge").source;
        if let Some(selector) = self.selector_in(src_node) {
            self.remove_value(selector);
        }

        let invalid_edge = self.invalid_edge().expect("Invalid Edge is not defined");
        let conditional_branches = 
            if let Some(branches) = self.conditional_edges(src_node) {
                branches
            } else {
                ConditionInfo::new(invalid_edge, invalid_edge)
            };
        let other_edge = match self.g[i] {
            EdgeData::Control(j) if j <= 2 => {
                match j {
                    0 => Some(conditional_branches.true_side),
                    1 => Some(conditional_branches.false_side),
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

        self.g.remove_edge(i);
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
            explorer.push_back(self.entry_node().expect("Incomplete CFG graph"));
            let nodes = &mut walker.nodes;
            while let Some(ref block) = explorer.pop_front() {
                if visited.contains(block) {
                    continue;
                }
                visited.insert(*block);
                nodes.push_back(*block);
                let mut exprs = self.exprs_in(*block)
                                    .iter()
                                    .chain(self.phis_in(*block).iter())
                                    .cloned()
                                    .collect::<Vec<NodeIndex>>();

                exprs.sort_by(|x, y| {
                    let addr_x = self.address(*x)
                                        .expect("No address information found");
                    let addr_y = self.address(*y)
                                        .expect("No address information found");
                    addr_x.cmp(&addr_y)
                });

                for expr in &exprs {
                    nodes.push_back(*expr);
                }
                let mut outgoing = self.outgoing_edges(*block);
                outgoing.sort_by(|a, b| (a.1).cmp(&b.1));
                explorer.extend(outgoing.iter().map(|x| self.edge_info(x.0).expect("Less-endpoints edge").target));
            }
        }
        walker
    }

    fn inorder_walk(&self) -> Walker {
        let mut walker = Walker { nodes: VecDeque::new() };
        {
            let mut visited = HashSet::new();
            let mut explorer = BinaryHeap::<InorderKey>::new();
            explorer.push(InorderKey::new(MAddress::new(0, 0), self.entry_node().expect("Incomplete CFG graph")));
            let nodes = &mut walker.nodes;
            while let Some(ref key) = explorer.pop() {
                let block = &key.value;
                if visited.contains(block) {
                    continue;
                }
                visited.insert(*block);
                nodes.push_back(*block);
                let mut exprs = self.exprs_in(*block)
                                    .iter()
                                    .chain(self.phis_in(*block).iter())
                                    .cloned()
                                    .collect::<Vec<NodeIndex>>();

                exprs.sort_by(|x, y| {
                    let addr_x = self.address(*x)
                                        .expect("No address information found");
                    let addr_y = self.address(*y)
                                        .expect("No address information found");
                    addr_x.cmp(&addr_y)
                });
                for expr in &exprs {
                    nodes.push_back(*expr);
                }
                for outedge in self.outgoing_edges(*block) {
                    let target = self.edge_info(outedge.0)
                        .expect("Less-endpoints edge").target;
                    let addr = self.starting_address(target)
                        .expect("Losing starting address of an action");
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

impl DoubleEndedIterator for Walker {
    fn next_back(&mut self) -> Option<NodeIndex> {
        self.nodes.pop_back()
    }
}
