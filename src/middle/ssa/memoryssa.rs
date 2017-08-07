// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.


//! Module that implements memory SSA generation.
//! 
//! This module implements the algorithm mentioned in 
//! "Memory SSA - A Unified Approach for Fparsely Representing Memory Operations"
//!
//! Like what LLVM has done, we use the Dynamic Memory Partitions. In futher 
//! work, it may be improved to Hybrid Partitioning.
//! For more details, please refer:
//!     * http://www.airs.com/dnovillo/Papers/mem-ssa.pdf


use std::collections::{BTreeMap, HashMap, HashSet};
use std::cmp::Ordering;
use std::marker::PhantomData;
use petgraph::visit::{IntoEdgeReferences, EdgeRef};
use petgraph::EdgeDirection;
use petgraph::stable_graph::StableDiGraph;
use petgraph::graph::{EdgeIndex,  NodeIndex};

use r2api::structs::LVarInfo;

use super::ssa_traits::{SSA, SSAMod, SSAWalk};
use super::ssa_traits::{NodeType, NodeData, ValueType};

use middle::ir::MAddress;
use middle::regfile::SubRegisterFile;
use middle::ir::MOpcode;

// NOTE: Until now, this file is only used to implement raw Memory SSA, in 
// future work, it may be modified to support API for more accurate memory
// SSA, as the Value Set Analysis and Type Interface Analysis have been done.


// TODO: Add this file into ssastorage
// TODO: Add a trait for MemorySSA
// TODO: Combine the SSA implement with phiplacement

pub enum VariableType {
    Local(LVarInfo),
    Global(u64),
    Extra(NodeIndex),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MemOpcode {
    VDef,
    VUse,
    Phi,
    MemoryAccess,
    Undefined,
}

type VarId = usize;
type ArgOrd = u16;

pub struct MemorySSA<'a, I, T>
    where I: Iterator<Item = T::ValueRef>,
          T: 'a + SSA + SSAMod + SSAWalk<I>
{
    ssa: &'a T,
    pub g: StableDiGraph<MemOpcode, ArgOrd>,
    blocks: HashMap<NodeIndex, T::ActionRef>,
    links: HashMap<NodeIndex, T::ValueRef>,
    variables: Vec<VariableType>,
    may_aliases: HashMap<T::ValueRef, HashSet<VarId>>,
    sealed_blocks: HashSet<T::ActionRef>,
    local_nodes: HashSet<T::ValueRef>,
    global_nodes: HashSet<T::ValueRef>,
    current_def: Vec<HashMap<T::ActionRef, NodeIndex>>,
    incomplete_phis: Vec<HashMap<T::ActionRef, NodeIndex>>, 
    foo: PhantomData<I>,
}

impl<'a, I, T> MemorySSA<'a, I, T>
    where I: Iterator<Item = T::ValueRef>,
          T: 'a + SSA + SSAMod + SSAWalk<I>
{
    pub fn new(ssa: &'a T) -> MemorySSA<'a, I, T> {
        MemorySSA {
            ssa: ssa,
            g: StableDiGraph::new(),
            links: HashMap::new(),
            blocks: HashMap::new(),
            variables: Vec::new(),
            may_aliases: HashMap::new(),
            local_nodes: HashSet::new(),
            global_nodes: HashSet::new(),
            sealed_blocks: HashSet::new(),
            current_def: Vec::new(),
            incomplete_phis: Vec::new(),
            foo: PhantomData,
        } 
    } 

    // Gather variables in following rules:
    //      local: from r2api to get local;
    //      global:from r2api to get datafers;
    //      extra: for every call statement generate an extra variable.
    fn gather_variables(&mut self,
                        datafers: &Option<Vec<u64>>,
                        locals: &Option<Vec<LVarInfo>>,
                        callrefs: &Option<Vec<NodeIndex>>) {
        if !datafers.is_none() {
            let mut gvars: Vec<VariableType> 
                = datafers.unwrap()
                            .iter()
                            .map(|x| VariableType::Global(x.clone()))
                            .collect();
            self.variables.append(&mut gvars);
        }
        if !locals.is_none() {
            let mut lvars: Vec<VariableType>
                = locals.unwrap()
                            .iter()
                            .map(|x| VariableType::Local(x.clone()))
                            .collect();
            self.variables.append(&mut lvars);
        }
        if !callrefs.is_none() {
            let mut evars: Vec<VariableType>
                = callrefs.unwrap()
                            .iter()
                            .map(|x| VariableType::Extra(x.clone()))
                            .collect();
            self.variables.append(&mut evars);
        }
    }

    // Check this addr is a global variable's address or not
    fn check_global(&self, addr: u64) -> bool {
        for var in &self.variables {
            if let VariableType::Global(target) = *var {
                if addr == target {
                    return true;
                }
            }
        }
        return false;
    } 

    // Check this reg is a BP/SP register or not
    fn check_local(&self, reg: &String) -> bool {
        for var in &self.variables {
            if let VariableType::Local(lvarinfo) = *var {
                if Some(*reg) == lvarinfo.reference.unwrap().base {
                    return true;
                }
            } 
        }
        return false;
    }

    // Initialize the nodes which could be regard as variables' addresses.
    //      For locals, it should be based on the BP/SP registers;
    //      For globals, it should be equal to the globals' addresses;
    fn init_nodes_type(&mut self) {
        for node in self.ssa.nodes() {
            if let Ok(ndata) = self.ssa.get_node_data(&node) {
                match ndata.nt {
                    NodeType::Op(MOpcode::OpConst(addr)) => {
                        if self.check_global(addr) {
                            self.global_nodes.insert(node);
                        }
                    }
                    NodeType::Comment(reg) => {
                        if self.check_local(&reg) {
                            self.local_nodes.insert(node);
                        }
                    } 
                    _ => {}
                }
            }
        }
    }
    
    // If idx's operands include local addresses or global addresses, it could
    // be regared as corresponding address.
    fn propagate_nodes_type(&mut self, idx: &T::ValueRef, args: &[T::ValueRef]) {
        let involve_local = false;
        let involve_global = false;
        for arg in args {
            if self.local_nodes.contains(arg) {
                involve_local = true;
            }
            if self.global_nodes.contains(arg) {
                involve_global = true;
            }
        }
        match (involve_local, involve_global) {
            (true, false) => { self.local_nodes.insert(*idx); }
            (false, true) => { self.global_nodes.insert(*idx); }
            _ => {}
        }
    }

    // This function calculate the may_alias, using a simple rule:
    //      if mem is a local address, then the may_alias set will include all
    // local variables;
    //      if mem is a global address, the the may_alias set will include all 
    // global variables;
    //      Otherwise, it will include all the variables.
    fn calculate_may_alias(&mut self, idx: &T::ValueRef, mem: &T::ValueRef) {
        let involve_local = self.local_nodes.contains(mem);
        let involve_global = self.global_nodes.contains(mem);
        let mut may_alias = HashSet::new();
        for i in 0..self.variables.len() {
            match self.variables[i] {
                VariableType::Extra(_) => {
                    may_alias.insert(i);
                }
                VariableType::Local(_) => {
                    if involve_local {
                        may_alias.insert(i);
                    }
                }
                VariableType::Global(_) => {
                    if involve_global {
                        may_alias.insert(i);
                    }
                }
                _ => {}
            } 
        }
        self.may_aliases.entry(*idx).or_insert(may_alias);
    }

    // This function gather may_alias set for every LOAD/STORE,
    // Meanwhile, calculate out nodes which could be regarded as variables'
    // addresses.
    fn gather_may_aliases(&mut self) {
        for expr in self.ssa.inorder_walk() {
            if let Ok(ndata) = self.ssa.get_node_data(&expr) {
                let operands = self.ssa.get_operands(&expr);
                match ndata.nt {
                    NodeType::Op(opc) => {
                        match opc {
                            MOpcode::OpLoad | MOpcode::OpStore => {
                                self.calculate_may_alias(&expr, &operands[0]);
                            }
                            _ => {
                                self.propagate_nodes_type(&expr, &operands);
                            }
                        }
                    }
                    NodeType::Phi => {
                        self.propagate_nodes_type(&expr, &operands)
                    }
                    _ => {}
                }
            } 
        } 
    }

    fn link(&mut self, idx: &T::ValueRef, node: NodeIndex) {
        self.links.entry(node).or_insert(idx.clone());
    }

    fn link_block(&mut self, idx: &T::ActionRef, node: NodeIndex) {
        self.blocks.entry(node).or_insert(idx.clone());
    }

    fn write_variable(&mut self, var: &VarId, block: &T::ActionRef, node: NodeIndex) {
        self.current_def[var.clone()].insert(block.clone(), node.clone());
    }

    fn read_variable(&mut self, var: &VarId, block: &T::ActionRef) -> NodeIndex {
        match self.current_def[var.clone()].get(block) {
            None => self.read_variable_recursive(var, block),
            Some(node) => node.clone(),
        }
    }

    fn read_variable_recursive(&mut self, var: &VarId, block: &T::ActionRef) -> NodeIndex {
        let val: NodeIndex;
        if !self.sealed_blocks.contains(block) {
            val = self.add_phi(block);
            self.incomplete_phis[var.clone()].insert(block.clone(), val.clone());
        } else {
            if self.ssa.preds_of(block.clone()).len() == 1 {
                val = self.read_variable(var, block);
            } else {
                val = self.add_phi(block);
                self.write_variable(var, block, val);
                val = self.add_phi_operands(var, val);
            }
        }
        self.write_variable(var, block, val);
        return val;
    } 

    fn add_node(&mut self, idx: &T::ValueRef, data: MemOpcode) -> NodeIndex {
        let phi = self.g.add_node(data);
        let block = self.ssa.get_block(idx);
        self.link(idx, phi);
        self.link_block(&block, phi);
        return phi;
    }

    fn add_phi(&mut self, block: &T::ActionRef) -> NodeIndex {
        let phi = self.g.add_node(MemOpcode::Phi);
        self.link_block(block, phi);
        return phi;
    }

    fn add_phi_operands(&mut self, var: &VarId, phi: NodeIndex) -> NodeIndex {
        let block = self.blocks.get(&phi).expect("Phi not found!");
        let preds = self.ssa.preds_of(*block);
        for pred in &preds {
            let target = self.read_variable(var, pred);
            self.add_use(phi, target);
        }
        return self.try_remove_trivial_phi(phi);
    }

    fn add_use(&mut self, phi: NodeIndex, arg: NodeIndex) {
        self.g.add_edge(phi, arg, 0);
    }

    fn add_undefined(&mut self) -> NodeIndex {
        return self.g.add_node(MemOpcode::Undefined);
    }

    fn try_remove_trivial_phi(&mut self, phi: NodeIndex) -> NodeIndex {
        let mut same = NodeIndex::end();
        for op in self.get_operands(phi) {
            if op == same || op == phi {
                continue;
            }
            if same != NodeIndex::end() {
                return phi;
            }
            same = op;
        }
        if same == NodeIndex::end() {
            same = self.add_undefined();
        }
        let users = self.get_uses(phi);
        self.replace(phi, same);

        for user in users {
            if user == phi {
                continue;
            }
            if self.is_phi(user) {
                self.try_remove_trivial_phi(user);
            }
        }
        return same;
    }

    fn seal_block(&mut self, block: &T::ActionRef) {
        for i in 0..self.variables.len() {
            let contain = self.incomplete_phis[i].contains_key(block);
            if contain {
                let val = self.incomplete_phis[i].get(block).unwrap();
                self.add_phi_operands(&i, *val);
            }
        }
        self.sealed_blocks.insert(block.clone());
    }

    fn replace(&mut self, src: NodeIndex, dst: NodeIndex) {
        let users = self.get_uses(src);
        self.g.remove_node(src);
        // Seems the edges will be deleted meanwhile.
        for user in users {
            self.add_use(user, dst);
        }
    }

    fn gather_adjacent(&self,
                       node: NodeIndex,
                       direction: EdgeDirection)
                       -> Vec<NodeIndex> {
        let mut adjacent: Vec<NodeIndex> = Vec::new();
        let mut walk = self.g.neighbors_directed(node, direction).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            adjacent.push(othernode);
        }
        adjacent
    }

    fn get_operands(&mut self, node: NodeIndex) -> Vec<NodeIndex> {
       return self.gather_adjacent(node, EdgeDirection::Outgoing);
    }

    fn get_uses(&mut self, node: NodeIndex) -> Vec<NodeIndex> {
       return self.gather_adjacent(node, EdgeDirection::Incoming);
    }

    fn is_phi(&self, node: NodeIndex) -> bool {
        match self.g[node] {
            MemOpcode::Phi => true,
            _ => false,
        }
    }

    // Main function of MemorySSA, generate Memory SSA using the similar way of 
    // phiplacement.rs
    fn generate(&mut self) {
        let start_node = self.ssa.start_node();
        let mem_start_node = self.g.add_node(MemOpcode::MemoryAccess);
        self.link_block(&start_node, mem_start_node);
        for i in 0..self.variables.len() {
            self.write_variable(&i, &start_node, mem_start_node);
        }
        for expr in self.ssa.inorder_walk() {
            if let Ok(ndata) = self.ssa.get_node_data(&expr) {
                match ndata.nt {
                    NodeType::Op(MOpcode::OpLoad) => {
                        let vuse = self.add_node(&expr, MemOpcode::VUse);
                        let set: Vec<VarId> = self.may_aliases.get(&expr).unwrap().iter().map(|x| x.clone()).collect();
                        for i in set {
                            let block = self.ssa.get_block(&expr);
                            let arg = self.read_variable(&i, &block);
                            self.add_use(vuse, arg);
                        }
                    }
                    NodeType::Op(MOpcode::OpStore) => {  
                        let vdef = self.add_node(&expr, MemOpcode::VDef);
                        let set: Vec<VarId> = self.may_aliases.get(&expr).unwrap().iter().map(|x| x.clone()).collect();
                        for i in set {
                            let block = self.ssa.get_block(&expr);
                            let arg = self.read_variable(&i, &block);
                            self.add_use(vdef, arg);
                            // TODO: this may not cover all the may_alias Set 
                            self.write_variable(&i, &block, vdef);
                        }
                    }
                    _ => {}
                }
            } 
        } 
        for block in self.ssa.blocks() {
            self.seal_block(&block);
        }
    }
}
