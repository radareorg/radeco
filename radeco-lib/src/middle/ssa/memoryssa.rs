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

use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::stable_graph::StableDiGraph;
use petgraph::EdgeDirection;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use r2api::structs::LVarInfo;

use super::ssa_traits::NodeType;
use super::ssa_traits::{SSAMod, SSAWalk, SSA};
use crate::middle::ir::MOpcode;

// NOTE: Until now, this file is only used to implement raw Memory SSA, in
// future work, it may be modified to support API for more accurate memory
// SSA, as the Value Set Analysis and Type Interface Analysis have been done.
//
// Memory SSA generation algorithm is similar with phiplacement.rs, but a little
// different. Also, considering different data structures, it's maybe not an
// easy work to combine these two files together.

// TODO: Add Memory SSA into SSA_Extra.
// TODO: Add a trait for MemorySSA as APIs for future work.
// TODO: Split this file into two files, one for may_alias set, another for MemorySSA.
// Above tasks should be done after Value Set Analysis finished.

#[derive(Clone, Debug)]
pub enum VariableType {
    /// Local variable type.
    Local(LVarInfo),
    /// Global variable type.
    Global(u64),
    /// Extra variable, or heap variable, type.
    Extra(NodeIndex),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MemOpcode {
    /// Used for OpStore.
    VDef,
    /// Used for OpLoad.
    VUse,
    /// Phi node for Memory SSA.
    Phi,
    /// Initial Memory Station.
    MemoryAccess,
    /// Undefined node.
    Undefined,
}

// TODO: Now the edge weight in MemorySSA graph is all zero, it may help
// if the operands are in order.
type VarId = usize;
type ArgOrd = u16;

pub struct MemorySSA<'a, I, T>
where
    I: Iterator<Item = T::ValueRef>,
    T: 'a + SSA + SSAMod + SSAWalk<I>,
{
    ssa: &'a T,
    pub g: StableDiGraph<MemOpcode, ArgOrd>,
    /// Transform a Memory SSA Node into its associated basic block.
    pub associated_blocks: HashMap<NodeIndex, T::ActionRef>,
    /// Transform a VDef/VUse node into its associated opcode node.
    pub associated_nodes: HashMap<NodeIndex, T::ValueRef>,
    /// All variables used in this function.
    // TODO: Create an API for variables to renew its station, for VSA may change it
    pub variables: Vec<VariableType>,
    /// May-alias Set for every LOAD/STORE.
    pub may_aliases: HashMap<T::ValueRef, HashSet<VarId>>,
    /// Nodes which could be ragared as a local variable address.
    pub local_nodes: HashSet<T::ValueRef>,
    /// Nodes which could be regared as a global variable address.
    pub global_nodes: HashSet<T::ValueRef>,
    // TODO: Above two members(local_nodes & global_nodes) may be combined with
    // each other and expand to indicated every special variable's address separately
    // after VSA finish.
    /// Different variables' Phi nodes for every basic block.
    pub phi_nodes: HashMap<NodeIndex, VarId>,
    // phi_nodes is used to map every phi node to its variable, which will be useful
    // in future work.
    sealed_blocks: HashSet<T::ActionRef>,
    current_def: Vec<HashMap<T::ActionRef, NodeIndex>>,
    incomplete_phis: Vec<HashMap<T::ActionRef, NodeIndex>>,
    // Same usage in phiplacement.rs
    foo: PhantomData<I>,
}

impl<'a, I, T> MemorySSA<'a, I, T>
where
    I: Iterator<Item = T::ValueRef>,
    T: 'a + SSA + SSAMod + SSAWalk<I>,
{
    pub fn new(ssa: &'a T) -> MemorySSA<'a, I, T> {
        MemorySSA {
            ssa: ssa,
            g: StableDiGraph::new(),
            associated_blocks: HashMap::new(),
            associated_nodes: HashMap::new(),
            variables: Vec::new(),
            may_aliases: HashMap::new(),
            local_nodes: HashSet::new(),
            global_nodes: HashSet::new(),
            sealed_blocks: HashSet::new(),
            phi_nodes: HashMap::new(),
            current_def: Vec::new(),
            incomplete_phis: Vec::new(),
            foo: PhantomData,
        }
    }

    /// Run to generate MemorySSA.
    pub fn run(&mut self) {
        self.gather_may_aliases();
        radeco_trace!("MemorrySSA|May_alias Set: {:?}", self.may_aliases);
        // Gather may_alias sets.
        radeco_trace!("MemorrySSA|Gather may_alias done!");

        self.generate();
        // Calculate MemorySSA.
        radeco_trace!("MemorrySSA|generate done!");
    }

    // Gather variables in following rules:
    //      local: from r2api to get local;
    //      global:from r2api to get datafers;
    //      extra: for every call statement generate an extra variable.
    /// Use information from RadecoFunctoin to gather basic variables' information.
    pub fn gather_variables(
        &mut self,
        datafers: &Vec<u64>,
        locals: &Vec<LVarInfo>,
        callrefs: &Vec<NodeIndex>,
    ) {
        radeco_trace!("MemorrySSA|Get datafers: {:?}", datafers);
        // datafers is coming from RadecoFunctoin::datafers
        let mut gvars: Vec<VariableType> = datafers
            .clone()
            .iter()
            .map(|x| VariableType::Global(x.clone()))
            .collect();
        self.variables.append(&mut gvars);

        radeco_trace!("MemorrySSA|Get locals: {:?}", locals);
        // locals is coming from RadecoFunctoin::locals
        let mut lvars: Vec<VariableType> = locals
            .clone()
            .iter()
            .map(|x| VariableType::Local(x.clone()))
            .collect();
        self.variables.append(&mut lvars);

        radeco_trace!("MemorrySSA|Get callrefs: {:?}", callrefs);
        // callrefs is coming from RadecoFunctoin::call_ctx::ssa_ref
        let mut evars: Vec<VariableType> = callrefs
            .clone()
            .into_iter()
            .map(|x| VariableType::Extra(x))
            .collect();
        self.variables.append(&mut evars);

        radeco_trace!("MemorrySSA|Gather variables: {:?}", self.variables);

        // Resize associated data structures
        for _ in 0..self.variables.len() {
            self.current_def.push(HashMap::new());
            self.incomplete_phis.push(HashMap::new());
        }
    }

    // Check addr could be regared as a global variable's address or not
    fn check_global(&self, addr: u64) -> bool {
        radeco_trace!("MemorrySSA|New const added into test: {:?}", addr);
        for var in &self.variables {
            if let VariableType::Global(target) = *var {
                if addr == target {
                    radeco_trace!("MemorrySSA|New global address found: {:?}", addr);
                    return true;
                }
            }
        }
        return false;
    }

    // Check reg is a BP/SP register or not
    fn check_local(&self, mut comment: String) -> bool {
        radeco_trace!("MemorrySSA|New comment added into test: {:?}", &comment);
        for var in &self.variables {
            if let VariableType::Local(lvarinfo) = var.clone() {
                let reg = {
                    let b_opt = lvarinfo.reference.and_then(|r| r.base);
                    if let Some(b) = b_opt {
                        b
                    } else {
                        radeco_err!("Invalid reference");
                        continue;
                    }
                };
                let reg_len: usize = reg.len();
                if comment.len() < reg_len {
                    return false;
                }
                comment.split_off(reg_len);
                // Attition: Comment may be in format as: reg@address.offset

                if comment == reg {
                    radeco_trace!("MemorrySSA|New BP/SP register found: {:?}", comment);
                    return true;
                }
            }
        }
        return false;
    }
    // TODO: Above two functions should be combined into one named check_variables,
    // which could use to check every different variable after VSA.
    // OR: It's only used for raw Memory SSA to gather may_alias set, after raw
    // MemorySSA, the later may_alias set analysis should be done by VSA.

    // Initialize the nodes which could be regard as variables' addresses.
    //      For locals, it should be based on the BP/SP registers;      -> CommentNode
    //      For globals, it should be equal to the globals' addresses;  -> OpConse
    //
    // NOTE: It maybe become unnecessary after VSA finish, because VSA could make
    // may_alias set, rather than in MemorySSA.
    fn init_nodes_type(&mut self) {
        for node in &self.ssa.values() {
            if let Ok(ndata) = self.ssa.node_data(*node) {
                match ndata.nt {
                    NodeType::Op(MOpcode::OpConst(addr)) => {
                        if self.check_global(addr) {
                            self.global_nodes.insert(*node);
                        }
                    }

                    NodeType::Comment(reg) => {
                        if self.check_local(reg.clone()) {
                            self.local_nodes.insert(*node);
                        }
                    }
                    _ => {}
                }
            }
        }
        radeco_trace!(
            "MemorrySSA|Find local variable address: {:?}",
            self.local_nodes
        );
        radeco_trace!(
            "MemorrySSA|Find global variable address: {:?}",
            self.global_nodes
        );
    }

    // If idx's operands include local addresses or global addresses, it could
    // be regared as corresponding address.
    //
    // TODO: Treat different OpCodes in different ways may help increasing accuracy.
    fn propagate_nodes_type(&mut self, idx: &T::ValueRef, args: &[T::ValueRef]) {
        let mut involve_local = false;
        let mut involve_global = false;
        for arg in args {
            if self.local_nodes.contains(arg) {
                involve_local = true;
            }
            if self.global_nodes.contains(arg) {
                involve_global = true;
            }
        }

        match (involve_local, involve_global) {
            (true, false) => {
                radeco_trace!("MemorrySSA|Find new local variable address node: {:?}", idx);
                self.local_nodes.insert(*idx);
                radeco_trace!("MemorrySSA|Local variable address: {:?}", self.local_nodes);
            }
            (false, true) => {
                radeco_trace!(
                    "MemorrySSA|Find new global variable address node: {:?}",
                    idx
                );
                radeco_trace!(
                    "MemorrySSA|Global variable address: {:?}",
                    self.global_nodes
                );
                self.global_nodes.insert(*idx);
            }
            _ => {}
        }
    }

    // This function calculate the may_alias, using a simple rule:
    //      if mem is a local address, then the may_alias set will include all
    // local variables;
    //      if mem is a global address, the the may_alias set will include all
    // global variables;
    //      Otherwise, it will include all the variables.
    //
    // NOTE: This function is only used in raw MemorySSA, and may be replaces by VSA.
    fn calculate_may_alias(&mut self, idx: &T::ValueRef, mem: &T::ValueRef) {
        let involve_local = self.local_nodes.contains(mem);
        let involve_global = self.global_nodes.contains(mem);
        let mut may_alias = HashSet::new();

        let involve_all = !(involve_local ^ involve_global);
        // if involve_local and involve_global is both true or both false,
        // it should involve_all.

        for i in 0..self.variables.len() {
            match self.variables[i] {
                VariableType::Extra(_) => {
                    if involve_all {
                        may_alias.insert(i);
                    }
                }
                // TODO: Add function to check the call statement could reach mem node
                // or not.
                VariableType::Local(_) => {
                    if involve_local || involve_all {
                        may_alias.insert(i);
                    }
                }

                VariableType::Global(_) => {
                    if involve_global || involve_all {
                        may_alias.insert(i);
                    }
                }
            }
        }

        radeco_trace!("MemorrySSA|New may_alias set {:?} for {:?}", may_alias, idx);
        self.may_aliases.entry(*idx).or_insert(may_alias);
    }

    // This function gather may_alias set for every LOAD/STORE,
    // Meanwhile, calculate out nodes which could be regarded as variables'
    // addresses.
    fn gather_may_aliases(&mut self) {
        self.init_nodes_type();
        // Calculate nodes' type first.

        // TODO: There is a problem in this implement:
        //      if a phi node uses some node behind it, this will fail in
        //      propagating node type. But it's not urgent, because there must
        //      be at least one argument of phi node having been visited.
        //      My idea is using bfs from node which have been regared as
        //      variable address to propagate node type.
        //      I will try to do it later.
        for expr in self.ssa.inorder_walk() {
            if let Ok(ndata) = self.ssa.node_data(expr) {
                let operands = self.ssa.operands_of(expr);
                match ndata.nt {
                    NodeType::Op(opc) => {
                        match opc {
                            MOpcode::OpLoad | MOpcode::OpStore => {
                                // TODO: Token::EEq will cause OpStore uses the first
                                // argument as memory address, although it's impossible
                                // to happen in normal situation.
                                self.calculate_may_alias(&expr, &operands[1]);
                            }

                            _ => {
                                self.propagate_nodes_type(&expr, &operands);
                            }
                        }
                    }
                    NodeType::Phi => self.propagate_nodes_type(&expr, &operands),
                    _ => {}
                }
            }
        }
    }

    // Above code are used in raw MemorySSA to gather may_alias set.
    // Following code will concentrate on MemorySSA generation, which
    // will cut into two parts. Part One concentrates on the basic
    // operations on MemorySSA graph (self.g), while Part Two concentrates
    // on the MemorySSA generation, especially phi placement.

    // PART ONE: Basic Graph Operations
    fn add_node(&mut self, ssa_node: &T::ValueRef, data: MemOpcode) -> NodeIndex {
        let mem_node = self.g.add_node(data);
        self.associated_nodes.insert(mem_node, *ssa_node);

        let block = self.ssa.block_for(*ssa_node).unwrap_or_else(|| {
            radeco_err!("Value node doesn't belong to any block");
            self.ssa.invalid_action().unwrap()
        });
        self.associated_blocks.insert(mem_node, block.clone());
        return mem_node;
    }

    fn add_phi(&mut self, var: VarId, block: &T::ActionRef) -> NodeIndex {
        let phi = self.g.add_node(MemOpcode::Phi);
        self.associated_blocks.insert(phi, block.clone());
        self.phi_nodes.insert(phi.clone(), var);
        // Map the new phi node with its variable.
        return phi;
    }

    fn add_undefined(&mut self) -> NodeIndex {
        return self.g.add_node(MemOpcode::Undefined);
    }

    fn is_phi(&self, mem_node: &NodeIndex) -> bool {
        match self.g[*mem_node] {
            MemOpcode::Phi => true,
            _ => false,
        }
    }

    fn add_use(&mut self, expr: &NodeIndex, arg: &NodeIndex) {
        if !self.g.find_edge(*expr, *arg).is_none() {
            return;
            // In MemorySSA, it's common that add an edge between two nodes
            // which have been connected before.
        }
        self.g.add_edge(*expr, *arg, 0);
    }

    fn get_uses(&self, mem_node: &NodeIndex) -> Vec<NodeIndex> {
        let adjacents = self.gather_adjacent(mem_node, EdgeDirection::Incoming);
        adjacents.1
    }

    // NOTE: Now, we do not concentrate on the order of operands.
    fn get_operands(&self, mem_node: &NodeIndex) -> Vec<NodeIndex> {
        let adjacents = self.gather_adjacent(mem_node, EdgeDirection::Outgoing);
        adjacents.1
    }

    fn gather_adjacent(
        &self,
        node: &NodeIndex,
        direction: EdgeDirection,
    ) -> (Vec<EdgeIndex>, Vec<NodeIndex>) {
        let mut edges: Vec<EdgeIndex> = Vec::new();
        let mut adjacent: Vec<NodeIndex> = Vec::new();
        let mut walk = self.g.neighbors_directed(*node, direction).detach();
        while let Some((edge, othernode)) = walk.next(&self.g) {
            edges.push(edge);
            adjacent.push(othernode);
        }
        (edges, adjacent)
    }

    fn disconnect(&mut self, src: &NodeIndex, dst: &NodeIndex) {
        let edge = self.g.find_edge(*src, *dst);
        if let Some(ei) = edge {
            self.g.remove_edge(ei);
        }
    }

    fn replace(&mut self, origin: &NodeIndex, replacement: &NodeIndex) {
        radeco_trace!("MemorrySSA|Replace {:?} with {:?}", origin, replacement);
        let users = self.get_uses(origin);
        let operands = self.get_operands(origin);

        // Remove edges associated with origin node.
        for user in &users {
            self.disconnect(user, origin);
        }
        for operand in &operands {
            self.disconnect(origin, operand);
        }

        // Connect user with replacement node.
        for user in &users {
            self.add_use(user, replacement);
        }

        // Delete the association with ssa graph
        // NOTE: It must be phi now, but maybe change in future work.
        if !self.is_phi(origin) {
            self.associated_nodes.remove(origin);
        }
        self.associated_blocks.remove(origin);

        // Remove origin node.
        self.g.remove_node(*origin);
    }

    // PART TWO: MemorySSA Generation

    // Recode the current define of variable var.
    fn write_variable(&mut self, var: VarId, block: &T::ActionRef, mem_node: &NodeIndex) {
        radeco_trace!(
            "MemorrySSA|Write variable ({:?}) in block ({:?}) at mem node ({:?})",
            var,
            block,
            mem_node
        );
        self.current_def[var].insert(block.clone(), mem_node.clone());
    }

    // Get the reachable define for var, if it doesn't exist in this block, call
    // read_variable_recursive to visit block's predecessors.
    fn read_variable(&mut self, var: VarId, block: &T::ActionRef) -> NodeIndex {
        radeco_trace!(
            "MemorrySSA|Read variable ({:?}) in block ({:?})",
            var,
            block
        );
        if let Some(node) = self.current_def[var].get(block) {
            return node.clone();
        }
        return self.read_variable_recursive(var, block);
    }

    // Add phi nodes in the basic block, and visit its predecessors.
    // This function will only be called when the current block doesn't have var's define.
    fn read_variable_recursive(&mut self, var: VarId, block: &T::ActionRef) -> NodeIndex {
        radeco_trace!(
            "MemorrySSA|Call read_variable_recursive: {:?} {:?}",
            var,
            block
        );
        radeco_trace!(
            "MemorrySSA|Pred Block information {:?}",
            self.ssa.preds_of(block.clone())
        );
        let mut val: NodeIndex;

        if !self.sealed_blocks.contains(block) {
            val = self.add_phi(var, block);
            self.incomplete_phis[var].insert(block.clone(), val);
        } else {
            let preds = self.ssa.preds_of(block.clone());
            if preds.len() == 1 {
                val = self.read_variable(var, &preds[0]);
            } else {
                val = self.add_phi(var, block);
                self.write_variable(var, block, &val);
                val = self.add_phi_operands(var, &val);
            }
        }

        self.write_variable(var, block, &val);
        return val;
    }

    // Add phi operands, and try to use try_remove_trivial_phi to delete trivial phi node.
    fn add_phi_operands(&mut self, var: VarId, phi: &NodeIndex) -> NodeIndex {
        let preds = self.associated_blocks.get(phi).map_or_else(
            || {
                radeco_err!("Phi not found!");
                vec![self.ssa.invalid_action().unwrap().clone()]
            },
            |block| self.ssa.preds_of(block.clone()),
        );
        for pred in &preds {
            let target = self.read_variable(var, pred);
            self.add_use(phi, &target);
        }
        return self.try_remove_trivial_phi(phi);
    }

    // Remove trivial phi node, trying to make a minimal SSA.
    // However, if the function's CFG is not reductive, it will not be minimal.
    // Btw, the same problem also exist in the phiplacement.rs.
    fn try_remove_trivial_phi(&mut self, phi: &NodeIndex) -> NodeIndex {
        let mut same = NodeIndex::end(); // Invalid_node

        // Check if this phi is trivial
        for op in self.get_operands(phi) {
            if op == same || op == *phi {
                continue;
            }
            if same != NodeIndex::end() {
                return phi.clone();
                // Is not trivial, then return phi itself.
            }
            same = op;
        }

        if same == NodeIndex::end() {
            same = self.add_undefined();
            // This phi node only uses itself.
        }

        radeco_trace!("MemorrySSA|Remove phi node: {:?}", phi);
        let mut users: Vec<NodeIndex> = Vec::new();
        for user in self.get_uses(phi) {
            if !users.contains(&user) {
                users.push(user);
            }
        }
        self.replace(phi, &same);
        for variable in 0..self.variables.len() {
            let tmp: HashMap<T::ActionRef, NodeIndex> = self.current_def[variable]
                .iter()
                .map(|(block, target)| {
                    if target == phi {
                        (*block, same)
                    } else {
                        (*block, *target)
                    }
                })
                .collect();
            self.current_def[variable] = tmp;
        }
        self.phi_nodes.remove(&phi);
        // Disconnect the phi node with var.

        for user in &users {
            if user == phi {
                continue;
            }
            if self.is_phi(user) {
                self.try_remove_trivial_phi(user);
            }
        }
        return same;
    }

    // Called when block's predecessors finish.
    fn seal_block(&mut self, block: &T::ActionRef) {
        for i in 0..self.variables.len() {
            if self.incomplete_phis[i].contains_key(block) {
                let val = self.incomplete_phis[i].get(block).unwrap().clone();
                self.add_phi_operands(i.clone(), &val);
            }
        }
        self.sealed_blocks.insert(block.clone());
    }

    // Main function of MemorySSA, generate Memory SSA using the similar way of
    // phiplacement.rs
    fn generate(&mut self) {
        let entry_node = entry_node_err!(self.ssa);
        let mem_entry_node = self.g.add_node(MemOpcode::MemoryAccess);
        self.associated_blocks.insert(mem_entry_node, entry_node);
        for i in 0..self.variables.len() {
            self.write_variable(i, &entry_node, &mem_entry_node);
        }
        // Init MemorySSA, making MemoryAccess as all variables' define.

        for expr in self.ssa.inorder_walk() {
            let dummy = HashSet::new();
            if let Ok(ndata) = self.ssa.node_data(expr) {
                radeco_trace!("MemorrySSA|Deal with node: {:?}", expr);

                match ndata.nt {
                    NodeType::Op(MOpcode::OpLoad) => {
                        let set: Vec<VarId> = self
                            .may_aliases
                            .get(&expr)
                            .unwrap_or_else(|| {
                                radeco_err!("Cannot find may_alias set!");
                                &dummy
                            })
                            .iter()
                            .cloned()
                            .collect();
                        let vuse = self.add_node(&expr, MemOpcode::VUse);
                        for i in set {
                            let block = self.ssa.block_for(expr).unwrap_or_else(|| {
                                radeco_err!("Value node doesn't belong to any block");
                                self.ssa.invalid_action().unwrap()
                            });
                            let arg = self.read_variable(i, &block);
                            self.add_use(&vuse, &arg);
                        }
                    }

                    NodeType::Op(MOpcode::OpStore) => {
                        let set: Vec<VarId> = self
                            .may_aliases
                            .get(&expr)
                            .unwrap_or_else(|| {
                                radeco_err!("Cannot find may_alias set!");
                                &dummy
                            })
                            .iter()
                            .cloned()
                            .collect();
                        let vdef = self.add_node(&expr, MemOpcode::VDef);
                        for i in set {
                            let block = self.ssa.block_for(expr).unwrap_or_else(|| {
                                radeco_err!("Value node doesn't belong to any block");
                                self.ssa.invalid_action().unwrap()
                            });
                            let arg = self.read_variable(i, &block);
                            self.add_use(&vdef, &arg);
                            self.write_variable(i, &block, &vdef);
                        }
                    }
                    // TODO: Combine above code chunk
                    _ => {}
                }
            }
        }

        // Seal blocks
        for block in self.ssa.blocks() {
            self.seal_block(&block);
        }

        radeco_trace!("MemorrySSA|Memory SSA Graph: {:?}", self.g);
    }
}
