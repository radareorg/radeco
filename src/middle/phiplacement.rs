// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements the SSA construction algorithm described in
//! "Simple and Efficient Construction of Static Single Assignment Form"

use std::collections::{BTreeMap, HashMap, HashSet};
use std::cmp::Ordering;
use std::u64;

use middle::ssa::ssa_traits::{SSAMod, SSAExtra, ValueInfo};
use middle::ssa::graph_traits::{Graph, ConditionInfo};
use middle::ir::{self, MAddress, MOpcode};

use middle::ssa::ssa_traits::{NodeType, NodeData};
use middle::regfile::SubRegisterFile;

pub type VarId = u64;

const UNCOND_EDGE: u8 = 2;

pub struct PhiPlacer<'a, T> 
    where T: 'a + SSAExtra +
        SSAMod<BBInfo = MAddress,
               ActionRef = <T as Graph>::GraphNodeRef,
               CFEdgeRef = <T as Graph>::GraphEdgeRef>
{
    current_def: Vec<BTreeMap<MAddress, T::ValueRef>>,
    incomplete_phis: HashMap<MAddress, HashMap<VarId, T::ValueRef>>,
    incomplete_propagation: HashSet<T::ValueRef>,
    outputs: HashMap<T::ValueRef, VarId>,
    pub blocks: BTreeMap<MAddress, T::ActionRef>,
    pub index_to_addr: HashMap<T::ValueRef, MAddress>,
    pub variable_types: Vec<ValueInfo>,
    regfile: SubRegisterFile,
    sealed_blocks: HashSet<T::ActionRef>,
    ssa: &'a mut T,
    unexplored_addr: u64,
}

impl<'a, T> PhiPlacer<'a, T> 
    where T: 'a + SSAExtra +
        SSAMod<BBInfo = MAddress,
               ActionRef = <T as Graph>::GraphNodeRef,
               CFEdgeRef = <T as Graph>::GraphEdgeRef>
{

    /// Create a new instance of phiplacer
    pub fn new(ssa: &'a mut T, regfile: SubRegisterFile) -> PhiPlacer<'a, T> {
        PhiPlacer {
            blocks: BTreeMap::new(),
            current_def: Vec::new(),
            incomplete_phis: HashMap::new(),
            incomplete_propagation: HashSet::new(),
            index_to_addr: HashMap::new(),
            outputs: HashMap::new(),
            regfile: regfile,
            sealed_blocks: HashSet::new(),
            ssa: ssa,
            unexplored_addr: u64::max_value() - 1,
            variable_types: Vec::new(),
        }
    }

    /// Add a new variable that the phiplacer should know of.
    /// This information is required to place phi-s. Note that the 
    /// phis are generated only for variabels defined in this list.
    ///
    /// This list typically should include all the registers that a part of the
    /// arch and a memory instance.
    pub fn add_variables(&mut self, variable_types: Vec<ValueInfo>) {
        for _ in &variable_types {
            self.current_def.push(BTreeMap::new());
        }
        self.variable_types.extend(variable_types);
    }

    /// Write/associate a value/node with a defined variable. Usually called when the 
    /// expression has an assignment.
    ///
    /// It is up to the caller function to ensure that the sizes are compatible, and insert
    /// appropriate width operations if necessary.
    pub fn write_variable(&mut self, address: MAddress, variable: VarId, value: T::ValueRef) {
        radeco_trace!("phip_write_var|{:?}|{} ({:?})|{}",
                      value,
                      variable,
                      self.regfile.whole_names.get(variable as usize),
                      address);

        if let Some(rname) = self.regfile.whole_names.get(variable as usize) {
            self.ssa.set_register(value, rname.clone());
        }

        self.current_def[variable as usize].insert(address, value);
        self.outputs.insert(value, variable);
    }

    // Returns the address that provides this definition and the corresponding
    // ValueRef.
    // This method is different from current_def_in_block as this will return a
    // ValueRef even if
    // the definition is not within the same block.
    fn current_def_at(&self,
                      variable: VarId,
                      address: MAddress)
                      -> Option<(&MAddress, &T::ValueRef)> {
        for (addr, idx) in self.current_def[variable as usize].iter().rev() {
            if self.block_of(*addr) != self.block_of(address) && *addr > address {
                continue;
            }
            return Some((addr, idx));
        }
        None
    }

    fn current_def_in_block(&self, variable: VarId, address: MAddress) -> Option<&T::ValueRef> {
        if let Some(v) = self.current_def_at(variable, address) {
            if self.block_of(*v.0) == self.block_of(address) {
                Some(v.1)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn read_variable(&mut self, address: &mut MAddress, variable: VarId) -> T::ValueRef {
        match self.current_def_in_block(variable, *address).cloned() {
            Some(var) => var,
            None => self.read_variable_recursive(variable, address),
        }
    }

    fn read_variable_recursive(&mut self, variable: VarId, address: &mut MAddress) -> T::ValueRef {
        let block = self.block_of(*address).unwrap();
        let valtype = self.variable_types[variable as usize];
        let val = if self.sealed_blocks.contains(&block) {
            let preds = self.ssa.preds_of(block);
            //assert!(preds.len() > 0);
            if preds.len() == 1 {
                // Optimize the common case of one predecessor: No phi needed
                let mut p_address = self.addr_of(&preds[0]);
                self.read_variable(&mut p_address, variable)
            } else {
                // Break potential cycles with operandless phi
                let val = self.add_phi(address, valtype);
                self.write_variable(*address, variable, val);
                self.add_phi_operands(block, variable, val)
            }
        } else {
            // Incomplete CFG
            let val_ = self.add_phi(address, valtype);
            let block_addr = self.addr_of(&block);
            if let Some(hash) = self.incomplete_phis.get_mut(&block_addr) {
                match hash.get(&variable).cloned() {
                    Some(v) => v,
                    None => {
                        radeco_trace!("phip_rvr|phi({:?})|{}({:?})|{}|{}",
                                      val_,
                                      variable,
                                      self.regfile.whole_names.get(variable as usize),
                                      address,
                                      block_addr);

                        hash.insert(variable, val_);
                        val_
                    }
                }
            } else {
                panic!()
            }

        };
        self.write_variable(*address, variable, val);
        val
    }

    // In this new implementation, this method is a bit tricky.
    // Let us break down this functionality into multiple steps.
    // - add_block takes an address to add a block at.
    // - It then determines if this address is already "seen".
    // - If it has already seen this address, it means that we have a backwards
    //   edge and we need to split the existing basic block at the target address.
    //   Consequently, this method calls split_block to add references, phis etc.
    // - If it is unseen, then a new basic block is created at this address and
    //   kept ready to be used when instructions beloging to this basic block are
    //   parsed for addition into the SSA.
    //
    // current_addr is the current address at which we are parsing. This is
    // important to determine
    // if the address we are trying to create a block is seen or not.
    // - edge_type is the type of edge to be added between the block which contains
    // the
    // current_addr and the new block being added into the CFG.
    pub fn add_block(&mut self,
                     at: MAddress,
                     current_addr: Option<MAddress>,
                     edge_type: Option<u8>)
                     -> T::ActionRef {
        let seen = current_addr.map_or(false, |cur| {
            match cur.cmp(&at) {
                Ordering::Less | Ordering::Equal => false,
                // Check if `at` is before the first block.
                Ordering::Greater => self.block_of(at).is_some(),
            }
        });

        let upper_block = if seen {
            self.block_of(at).unwrap()
        } else {
            self.ssa.invalid_action().expect("Invalid Action is not defined")
        };

        // Create a new block and add the required type of edge.
        let lower_block = self.new_block(at);
        if let Some(e) = edge_type {
            if let Some(addr) = current_addr {
                let current_block = self.block_of(addr).unwrap();
                radeco_trace!("ADD BLOCK: phip_add_edge|{:?} --{}--> {:?}", 
                              current_block, e, lower_block);
                self.ssa.insert_control_edge(current_block, lower_block, e);
            }
        }
        
        if upper_block == lower_block {
            return upper_block;
        }

        if seen {
            // Details on performing the split up.
            //   - Add an unconditional flow edge from the upper to the lower block.
            // - We first find the defs for every expr in the `lower` block arising out
            // of a
            //   split (lower block is the block that has aatat`).
            // - If these defs are in the `upper` block, then we add a phi (incomplete
            // phi) and
            //   this is used to provide the def for this use.


            // Copy all the outgoing CF edges.
            let invalid_edge = self.ssa.invalid_edge()
                                        .expect("Invalid Edge is not defined");
            let conditional_branches = 
                if let Some(branches) = self.ssa.conditional_edges(upper_block) {
                    branches
                } else {
                    ConditionInfo::new(invalid_edge, invalid_edge)
                };
            let outgoing = [conditional_branches.false_side,
                            conditional_branches.true_side,
                            self.ssa.unconditional_edge(upper_block).unwrap_or(invalid_edge)
                            ];
            for (i, edge) in outgoing.iter().enumerate() {
                if *edge != self.ssa.invalid_edge()
                                        .expect("Invalid Edge is not defined") {
                    let target = self.ssa.edge_info(*edge).expect("Less-endpoints edge").target;
                    if lower_block != target {
                        radeco_trace!("ADD BLOCK: phip_add_edge|{:?} --{}--> {:?}", 
                                      lower_block, i, target);
                        self.ssa.insert_control_edge(lower_block, target, i as u8);
                        self.ssa.remove_control_edge(*edge);
                    }
                }
            }

            radeco_trace!("ADD BLOCK: phip_add_edge|{:?} --{}--> {:?}", upper_block, 
                          UNCOND_EDGE, lower_block);
            self.ssa.insert_control_edge(upper_block, lower_block, UNCOND_EDGE);
            self.blocks.insert(at, lower_block);

            // for (addr, ni) in self.addr_to_index.clone() {
            // For there is no assignment to addr_to_index, the original code will
            // skip the loop below, causing losing necessary phi functions
            for (ni, addr) in self.index_to_addr.clone() {
                if addr < at {
                    continue;
                }
                if let Some(block) = self.block_of(addr) {
                    if block != lower_block {
                        break;
                    }
                }
                // Now, this node index belongs to the lower part of the split block.
                let operands = self.ssa.sparse_operands_of(ni);
                // Check if the operands belong to the upper block and if they do, remove the
                // operands edge, add a new phi and connect the edge from phi to ni.
                for operand_ in &operands {
                    let (i, operand) = *operand_;
                    // If operand is const, it cannot belong to any block.
                    match self.ssa.opcode(operand) {
                        Some(MOpcode::OpConst(_)) => { continue; }
                        _ => {  }
                    }
                    let operand_addr = self.index_to_addr[&operand];
                    let operand_block = self.block_of(operand_addr).unwrap();
                    if operand_block == upper_block {
                        // Since the current block is not sealed, we can add an incomplete phi and
                        // modify the constructed SSA to take this phi as operand rather than the
                        // node.
                        // This can be summarized in the following operations:
                        //   - Remove the edge from the operand to ni.
                        //   - Add a new "incomplete" phi associated with the lower block.
                        //   - Connect the edge.
                        // The above two steps can easily be accomplished by doing a read_variable
                        // and adding an operand edge.
                        // BUG: Not all operands are the register residences.
                        // For example, OpZeroExt for 32-bit register. If we want to add a 32-bit register
                        // with a constant, our algorithm will add an Opwiden(64) node for the register,
                        // then add the OpZeroExt node with an Opconst. In this situation, OpZeroExt
                        // node isn't a residence for any register. 
                        if !self.outputs.contains_key(&operand) {
                            continue;
                        }
                        self.ssa.op_unuse(ni, operand);
                        let output_varid = self.outputs[&operand];
                        let mut at_ = at;
                        // BUG: if current define is ni itself, this code may cause ni use itself
                        // as operand. So we have to do some check.
                        // A tricky way to solve the bug is to call read_variable_recursive, rather
                        // than read_variable, this will force the code generate a phi node.
                        let replacement_phi = self.read_variable_recursive(output_varid, &mut at_);
                        self.ssa.op_use(ni, i, replacement_phi);
                    }
                }
            }
        } else {
            self.blocks.insert(at, lower_block);
        }
        lower_block
    }

    // Function to add an indirect control flow transfer
    pub fn add_indirect_cf(&mut self, selector: &T::ValueRef, current_addr: &mut MAddress, edge_type: u8) {
        let source_block = self.block_of(*current_addr).unwrap();
        let unexplored_addr = MAddress::new(self.unexplored_addr, 0);

        // Decrement the unexplored addr so that we have a new unexplored block for next use
        self.unexplored_addr = self.unexplored_addr - 1;

        let unexplored_block = self.new_block(unexplored_addr);

        self.blocks.insert(unexplored_addr, unexplored_block);
        self.ssa.insert_control_edge(source_block, unexplored_block, edge_type);

        // Add a dummy ITE to mark the selector.
        let op_node = self.add_op(&MOpcode::OpITE,
                                  current_addr,
                                  ValueInfo::new_scalar(ir::WidthSpec::Known(1)));

        self.op_use(&op_node, 0, selector);
    }

    pub fn add_edge(&mut self, source: MAddress, target: MAddress, cftype: u8) {
        let source_block = self.block_of(source).unwrap();
        let target_block = self.block_of(target).unwrap();
        radeco_trace!("phip_add_edge|{:?} --{}--> {:?}", source_block, cftype, target_block);
        radeco_trace!("phip_add_edge|{} --{}--> {}", source, cftype, target);
        self.ssa.insert_control_edge(source_block, target_block, cftype);
    }

    // Adds an unconditional edge the the next block if there are no edges for the current block.
    pub fn maybe_add_edge(&mut self, source: MAddress, target: MAddress) {
        let source_block = self.block_of(source).unwrap();
        if self.ssa.outgoing_edges(source_block).is_empty() {
            let target_block = self.block_of(target).unwrap();
            if target_block != source_block {
                radeco_trace!("phip_add_edge|{} --{}--> {}", source, UNCOND_EDGE, target);
                self.ssa.insert_control_edge(source_block, target_block, UNCOND_EDGE);
            }
        }
    }

    pub fn seal_block(&mut self, block: T::ActionRef) {
        let block_addr = self.addr_of(&block);
        let inc = self.incomplete_phis[&block_addr].clone();
        for (variable, node) in inc {
            let z = node;//.clone();
            let nx = self.add_phi_operands(block, variable, node);


            if let Some(t) = self.incomplete_phis.get_mut(&block_addr) {
                t.insert(variable, nx);
            }

            {
                let tmp_incomplete_phis = self.incomplete_phis.clone();
                for (blkaddr, var) in tmp_incomplete_phis {
                    for (variable, tmp_node) in var {
                        if tmp_node.eq(&z) {
                            if let Some(t) = self.incomplete_phis.get_mut(&blkaddr) {
                                t.insert(variable, nx);
                            }
                        }
                    }
                }

                let zz = self.current_def[variable as usize].clone();
                for (key, value) in zz {
                    if value.eq(&node) {
                        self.current_def[variable as usize].insert(key, nx);
                    }
                }
            }
        }
        self.sealed_blocks.insert(block);
    }

    fn add_phi_operands(&mut self,
                        block: T::ActionRef,
                        variable: VarId,
                        phi: T::ValueRef)
                        -> T::ValueRef {
        // Determine operands from predecessors
        let baddr = self.addr_of(&block);
        for pred in self.ssa.preds_of(block) {
            let mut p_addr = self.addr_of(&pred);

            radeco_trace!("phip_add_phi_operands|cur:{}|pred:{}", baddr, p_addr);

            let datasource = self.read_variable(&mut p_addr, variable);
            self.ssa.phi_use(phi, datasource);
            if self.ssa.registers(phi).is_empty() {
                self.propagate_reginfo(&phi);
            }
        }
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: T::ValueRef) -> T::ValueRef {
        let undef = self.ssa.invalid_value().expect("Invalid Value is not defined");
        // The phi is unreachable or in the start block
        let mut same: T::ValueRef = undef;
        for op in self.ssa.operands_of(phi) {
            if op == same || op == phi {
                // Unique value or self−reference
                continue;
            }
            if same != undef {
                // The phi merges at least two values: not trivial
                return phi;
            }
            same = op
        }

        if same == undef {
            let phi_addr = self.index_to_addr.get(&phi).cloned().unwrap();
            let block = self.block_of(phi_addr).unwrap();
            let valtype = self.ssa
                              .node_data(phi)
                              .expect("No Data associated with this node!")
                              .vt;
            let block_addr = self.addr_of(&block);
            same = self.add_undefined(block_addr, valtype);
        }

        //let users = self.ssa.uses_of(phi); //This works too instead of the code below.
        let mut users: Vec<T::ValueRef> = Vec::new(); 
        //I feel this is better is there is no comparison with phi value that is being replaced.
        // Better not, when node_data called for the phi node, it has been removed, so raise
        // an error.
        for uses in self.ssa.uses_of(phi) {
            if !users.contains(&uses) {
                users.push(uses);
            }
        }
        // Reroute all uses of phi to same and remove phi
        self.index_to_addr.remove(&phi);
        self.ssa.replace_value(phi, same);
        // TODO: There is a deep-hidden bug: when phi is some variables' residence, we should replace 
        // these variables' residences from phi to same. Otherwise, later when we call read_variable,
        // the function will return a deleted node, and cause panic in future work.
        if let Some(VarId) = self.outputs.remove(&phi) {
            self.outputs.insert(same, VarId);
        }
        for variable in 0..self.variable_types.len() {
            let cur_def = self.current_def[variable].clone();
            let mut pairs = cur_def.iter();
            while let Some(pair) = pairs.next() {
                if pair.1 == &phi {
                    self.current_def[variable].insert(*pair.0, same);
                }
            }
        }
        // Try to recursively remove all phi users, which might have become trivial
        for use_ in users {
            if use_ == phi {
                continue;
            }
            if true || false { //TODO: Need suggestion
                match self.ssa.node_data(use_) {
                    Ok(NodeData {vt: _, nt: NodeType::Phi}) => {
                        self.try_remove_trivial_phi(use_);
                    },
                    _ => {}
                }
            } else { //XXX: Remove this?
                if self.ssa.node_data(use_).is_ok() {
                    self.try_remove_trivial_phi(use_);
                }
            }
        }
        same
    }

    pub fn add_dynamic(&mut self) -> T::ActionRef {
        let action = self.ssa.insert_dynamic().expect("Cannot insert new actions");
        let dyn_addr = MAddress::new(u64::MAX, 0);
        self.blocks.insert(dyn_addr, action);
        self.incomplete_phis.insert(dyn_addr, HashMap::new());
        self.sync_register_state(action);
        action
    }

    pub fn sync_register_state(&mut self, block: T::ActionRef) {
        let rs = self.ssa.registers_in(block).expect("No register state node found");
        for var in 0..self.variable_types.len() {
            let mut addr = self.addr_of(&block);
            let val = self.read_variable(&mut addr, var as u64);
            self.ssa.op_use(rs, var as u8, val);
        }
    }

    pub fn mark_entry_node(&mut self, block: &T::ActionRef) {
        self.ssa.set_entry_node(*block);
    }

    pub fn mark_exit_node(&mut self, block: &T::ActionRef) {
        self.ssa.set_exit_node(*block);
    }

    // These functions take in address and automatically determine the block that
    // they should
    // belong to and make the necessary mappings internally. This is useful as the
    // outer struct
    // never has to keep track of the current_block or determine which block a
    // particular
    // instruction has to go into.

    fn add_phi(&mut self, address: &mut MAddress, vt: ValueInfo) -> T::ValueRef {
        let i = self.ssa.insert_phi(vt).expect("Cannot insert new phi nodes");
        self.index_to_addr.insert(i, *address);
        address.offset += 1;
        i
    }


    // Some constants are generated to operate with other OpCode, which shoud be careful
    // about its width. Because we treat all consts are 64 bit.
    // Constants need not belong to any block. They are stored as a separate table.
    pub fn add_const(&mut self, address: &mut MAddress, value: u64, vt_option: Option<ValueInfo>) -> T::ValueRef {
        if vt_option.is_none() {
            return self.ssa.insert_const(value).expect("Cannot insert new constants");
        }
        let vt = vt_option.unwrap();
        let width = vt.width().get_width().unwrap_or(64);
        if width < 64 {
            let val: u64 = value & (1 << (width) - 1);
            let const_node = self.ssa.insert_const(val)
                                        .expect("Cannot insert new constants");
            let opcode = MOpcode::OpNarrow(width as u16);
            let narrow_node = self.add_op(&opcode, address, vt);
            self.op_use(&narrow_node, 0, &const_node);
            narrow_node
        } else {
            let const_node = self.ssa.insert_const(value)
                                        .expect("Cannot insert new constants");
            const_node
        }
    }

    pub fn add_undefined(&mut self, address: MAddress, vt: ValueInfo) -> T::ValueRef {
        let i = self.ssa.insert_undefined(vt).expect("Cannot insert new undefined nodes");
        self.index_to_addr.insert(i, address);
        i
    }

    pub fn add_comment(&mut self, address: MAddress, vt: ValueInfo, msg: String) -> T::ValueRef {
        let i = self.ssa.insert_comment(vt, msg.clone())
                        .expect("Cannot insert new comments");
        
        // Add register information into comment;
        for id in 0..self.regfile.whole_names.len() {
            let reg_name = self.regfile.get_name(id).unwrap();
            let name = reg_name.as_str();
            if msg.starts_with(name) {
                self.ssa.set_register(i, reg_name.clone());
            }
        }

        self.index_to_addr.insert(i, address);
        i
    }

    // TODO: Add a more convenient method to add an opcode and operands to it.
    // Something like the previous verified_add_op.

    pub fn add_op(&mut self, op: &MOpcode, address: &mut MAddress, vt: ValueInfo) -> T::ValueRef {
        let i = self.ssa.insert_op(op.clone(), vt, None).expect("Cannot insert new values");
        self.index_to_addr.insert(i, *address); 
        address.offset += 1;
        i
    }

    pub fn set_address(&mut self, node: &T::ValueRef, address: MAddress) {
        self.index_to_addr.insert(*node, address);
    }

    pub fn read_register(&mut self, address: &mut MAddress, var: &str) -> T::ValueRef {
        radeco_trace!("phip_read_reg|{}", var);

        let info = match self.regfile.get_subregister(var) {
            Some(reg) => reg,
            None => {
                radeco_warn!("Float operations are not supported (yet)");
                let vi = ValueInfo::new_scalar(ir::WidthSpec::Unknown);
                let node = self.add_undefined(*address, vi);
                return node;
            }
        };
        let id = info.base;
        let mut value = self.read_variable(address, id);

        let width = self.operand_width(&value);

        // BUG: If width is not 64, every operation with OpConst will make
        // unbalanced width.

        if info.shift > 0 {
            let vtype = ValueInfo::new_unresolved(ir::WidthSpec::from(width));
            let shift_amount_node = self.add_const(address, info.shift as u64, Some(vtype));
            let opcode = MOpcode::OpLsr;
            let op_node = self.add_op(&opcode, address, vtype);
            self.op_use(&op_node, 0, &value);
            self.op_use(&op_node, 1, &shift_amount_node);
            value = op_node;
            self.propagate_reginfo(&value);
        }

        if info.width < width as u64 {
            let opcode = MOpcode::OpNarrow(info.width as u16);
            let vtype = ValueInfo::new_unresolved(ir::WidthSpec::from(info.width as u16));
            let op_node = self.add_op(&opcode, address, vtype);
            self.op_use(&op_node, 0, &value);
            value = op_node;
            self.propagate_reginfo(&value);
        }

        radeco_trace!("phip_read_reg|{:?}", value);
        value
    }

    pub fn write_register(&mut self, address: &mut MAddress, var: &str, mut value: T::ValueRef) {

        radeco_trace!("phip_write_reg|{}<-{:?}", var, value);

        let info = match self.regfile.get_subregister(var) {
            Some(reg) => reg,
            None => {
                radeco_warn!("Nowadays, radeco could not support float operation");
                return;
            }
        };
        let id = info.base;

        let vt = self.variable_types[id as usize];
        let width = vt.width().get_width().unwrap_or(64);

        if info.width >= width as u64 {
            // Register width should be corresponding with its residence's width.
            value = match width.cmp(&self.operand_width(&value)) {
                Ordering::Equal => value,
                Ordering::Less => {
                    let opcode = MOpcode::OpNarrow(width);
                    let narrow_node = self.add_op(&opcode, address, vt);
                    self.op_use(&narrow_node, 0, &value);
                    narrow_node
                }
                Ordering::Greater => {
                    let opcode = MOpcode::OpZeroExt(width);
                    let width_node = self.add_op(&opcode, address, vt);
                    self.op_use(&width_node, 0, &value);
                    width_node
                }
            };
            self.write_variable(*address, id, value);
            self.ssa.set_register(value, self.regfile
                                              .get_name(id as usize)
                                              .unwrap_or(String::new()));
            return;
        }

        // BUG: If width is not 64, every operation with OpConst will make
        // unbalanced width.
        let opcode = MOpcode::OpZeroExt(width as u16);

        if self.operand_width(&value) < width {
            let opcode_node = self.add_op(&opcode, address, vt);
            self.op_use(&opcode_node, 0, &value);
            value = opcode_node;
            self.propagate_reginfo(&value);
        }

        if info.shift > 0 {
            let shift_amount_node = self.add_const(address, info.shift as u64, Some(vt));
            let opcode_node = self.add_op(&MOpcode::OpLsl, address, vt);
            self.op_use(&opcode_node, 0, &value);
            self.op_use(&opcode_node, 1, &shift_amount_node);
            value = shift_amount_node;
            self.propagate_reginfo(&value);
        }

        let fullval: u64 = !((!1u64) << (width - 1));
        let maskval: u64 = ((!((!1u64) << (info.width - 1))) << info.shift) ^ fullval;

        if maskval == 0 {
            self.write_variable(*address, id, value);
            return;
        }

        let mut ov = self.read_variable(address, id);
        let maskvalue_node = self.add_const(address, maskval, Some(vt));

        let op_and = self.add_op(&MOpcode::OpAnd, address, vt);
        self.op_use(&op_and, 0, &ov);
        self.op_use(&op_and, 1, &maskvalue_node);
        self.propagate_reginfo(&op_and);

        ov = op_and;
        let op_or = self.add_op(&MOpcode::OpOr, address, vt);
        self.op_use(&op_or, 0, &value);
        self.op_use(&op_or, 1, &ov);
        value = op_or;
        self.write_variable(*address, id, value);
        self.propagate_reginfo(&value);
    }

    pub fn op_use(&mut self, op: &T::ValueRef, index: u8, arg: &T::ValueRef) {
        self.ssa.op_use(*op, index, *arg)
    }

    pub fn operand_width(&self, node: &T::ValueRef) -> u16 {
        self.ssa.node_data(*node).unwrap().vt.width().get_width().unwrap_or(64)
    }

    fn new_block(&mut self, bb: MAddress) -> T::ActionRef {
        if let Some(b) = self.blocks.get(&bb) {
            *b
        } else {
            let block = self.ssa.insert_block(bb).expect("Cannot insert new blocks");
            self.incomplete_phis.insert(bb, HashMap::new());
            block
        }
    }

    // For constant, we should narrow it rather widening another OpCode
    pub fn narrow_const_operand(&mut self, 
                                address: &mut MAddress, 
                                lhs: &mut Option<T::ValueRef>, 
                                rhs: &mut Option<T::ValueRef>) {
        if lhs.is_none() || rhs.is_none() {
            return
        }

        let lhs_size = lhs.map_or(0, |i| self.operand_width(&i));
        let rhs_size = rhs.map_or(0, |i| self.operand_width(&i));

        // Narrowing will happen only if there is one constant and one normal opcode.
        if lhs.map_or(false, |i| self.ssa.constant(i).is_some()) ^
            rhs.map_or(false, |i| self.ssa.constant(i).is_some()) {
                let (victim, victim_size) = if lhs_size < 64 {
                    (rhs, lhs_size)
                } else {
                    (lhs, rhs_size)
                };
                if victim_size < 64 {
                    let victim_node = victim.unwrap();
                    let vtype = ValueInfo::new_unresolved(ir::WidthSpec::from(victim_size));
                    let opc = MOpcode::OpNarrow(victim_size as u16);
                    let node = self.add_op(&opc, address, vtype);
                    self.op_use(&node, 0, &victim_node);
                    *victim = Some(node);
                }
        }
    }  

    // Determine which block as address should belong to.
    // This basically translates to finding the greatest address that is less that
    // `address` in self.blocks. This is fairly simple as self.blocks is sorted.
    pub fn block_of(&self, address: MAddress) -> Option<T::ActionRef> {
        let mut last = None;
        let start_address = {
            let start = self.ssa.entry_node().expect("Incomplete CFG graph");
            self.addr_of(&start)
        };
        for (baddr, index) in self.blocks.iter().rev() {
            // TODO: Better way to detect start block by using self.ssa.start_block
            // If this is the start block.
            if *baddr == start_address && *baddr != address {
                last = None;
            } else {
                last = Some(*index);
            }
            if *baddr <= address {
                break;
            }
        }
        last
    }

    // Return the address corresponding to the block.
    fn addr_of(&self, block: &T::ActionRef) -> MAddress {
        self.ssa.starting_address(*block).expect("Losing start address of an action")
    }

    pub fn associate_block(&mut self, node: &T::ValueRef, addr: MAddress) {
        let block = self.block_of(addr);
        self.ssa.insert_into_block(*node, block.unwrap(), addr);
    }

    // Copy the reginfo from operand into expr, especially for OpWiden/OpNarrow,
    // OpAnd/OpOr, OpLsl, which are used in width change, also, for Phi nodes.
    pub fn propagate_reginfo(&mut self, node: &T::ValueRef) {
        let args = self.ssa.operands_of(*node);
        // For OpWiden/OpNarrow, they only have one operation;
        // For OpAnd/OpOr, OpLsl, only their first operand coule be register;
        // For Phi node, their operations have the same reginfo;
        // Thus, choosing the first operand is enough. 

        // No arguments, nothing to do.
        if args.len() == 0 {
            return;
        }

        let regnames = self.ssa.registers(args[0]);
        if !regnames.is_empty() {
            for regname in &regnames {
                self.ssa.set_register(node.clone(), regname.clone());
                // Check whether its child nodes are incomplete 
            }
            for user in self.ssa.uses_of(*node) {
                if let Some(victim) = self.incomplete_propagation.take(&user) {
                    self.propagate_reginfo(&victim);
                }            
            }
        } else {
            // Wait its parent node to be propagated.
            self.incomplete_propagation.insert(*node);
            radeco_trace!("Fail in propagate_reginfo: {:?} with {:?}"
                          , node, self.ssa.node_data(*node));
            radeco_trace!("First operand is {:?} with {:?}", args[0]
                          , self.ssa.node_data(args[0]));
        }
    }


    // Visit all the blocks, find the exits of this function, and link these basic
    // with exit_node
    pub fn gather_exits(&mut self) {
        let blocks = self.ssa.blocks();
        let exit_node = self.ssa.exit_node().expect("Incomplete CFG graph");
        for block in blocks {
            if self.ssa.succs_of(block).len() == 0 {
                self.ssa.insert_control_edge(block, exit_node, UNCOND_EDGE);
            }
        } 
    }

    // Performs SSA finish operation such as assigning the blocks in the final
    // graph, sealing blocks, running basic dead code elimination etc.
    pub fn finish(&mut self) {
        // Iterate through blocks and seal them. Also associate nodes with their
        // respective blocks.
        let blocks = self.blocks.clone();
        for (addr, block) in blocks.iter().rev() {
            radeco_trace!("phip_seal_block|{:?}|{}", block, addr);
            self.seal_block(*block);
        }

        for node in &self.ssa.values() {
            if let Some(addr) = self.index_to_addr.get(node).cloned() {
                self.associate_block(node, addr);
                // Mark selector.
                if let Ok(ndata) = self.ssa.node_data(*node) {
                    if let NodeType::Op(MOpcode::OpITE) = ndata.nt {
                        let block = self.block_of(addr);
                        let cond_node = self.ssa.operands_of(*node)[0];
                        self.ssa.set_selector(cond_node, block.unwrap());
                        self.ssa.remove_value(*node);
                    }
                }
            }
        }

        self.ssa.map_registers(self.regfile.whole_names.clone());
    }
}
