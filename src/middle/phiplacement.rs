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
use std::io::{self, Write};
use std::process;

use middle::ssa::ssa_traits::{SSAMod, ValueType};
use middle::ir::MAddress;

use middle::ssa::ssa_traits::{NodeType, NodeData};
use middle::regfile::SubRegisterFile;
use middle::ir::MOpcode;

pub type VarId = usize;

const UNCOND_EDGE: u8 = 2;

pub struct PhiPlacer<'a, T: SSAMod<BBInfo = MAddress> + 'a> {
    ssa: &'a mut T,
    pub variable_types: Vec<ValueType>,
    sealed_blocks: HashSet<T::ActionRef>,
    current_def: Vec<BTreeMap<MAddress, T::ValueRef>>,
    incomplete_phis: HashMap<MAddress, HashMap<VarId, T::ValueRef>>,
    regfile: SubRegisterFile,
    pub blocks: BTreeMap<MAddress, T::ActionRef>,
    pub index_to_addr: HashMap<T::ValueRef, MAddress>,
    // addr_to_index: BTreeMap<MAddress, T::ValueRef>,
    outputs: HashMap<T::ValueRef, VarId>,
}

impl<'a, T: SSAMod<BBInfo=MAddress> + 'a> PhiPlacer<'a, T> {
    pub fn new(ssa: &'a mut T, regfile: SubRegisterFile) -> PhiPlacer<'a, T> {
        PhiPlacer {
            ssa: ssa,
            variable_types: Vec::new(),
            sealed_blocks: HashSet::new(),
            current_def: Vec::new(),
            incomplete_phis: HashMap::new(),
            regfile: regfile,
            blocks: BTreeMap::new(),
            index_to_addr: HashMap::new(),
            // No need for addr_to_index
            // addr_to_index: BTreeMap::new(),
            outputs: HashMap::new(),
        }
    }

    pub fn add_variables(&mut self, variable_types: Vec<ValueType>) {
        for _ in &variable_types {
            self.current_def.push(BTreeMap::new());
        }
        self.variable_types.extend(variable_types);
    }

    pub fn write_variable(&mut self, address: MAddress, variable: VarId, value: T::ValueRef) {
        radeco_trace!("phip_write_var|{:?}|{} ({:?})|{}",
                      value,
                      variable,
                      self.regfile.whole_names.get(variable),
                      address);

        self.current_def[variable].insert(address, value);
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
        for (addr, idx) in self.current_def[variable].iter().rev() {
            // if *addr > address {
            // continue;
            // }
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
        let valtype = self.variable_types[variable];
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
                                      self.regfile.whole_names.get(variable),
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

    // TODO: Add methods to expose addition of edges between the basic blocks.
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
            self.ssa.invalid_action()
        };

        // Create a new block and add the required type of edge.
        let lower_block = self.new_block(at);
        if let Some(e) = edge_type {
            if let Some(addr) = current_addr {
                let current_block = self.block_of(addr).unwrap();
                self.ssa.add_control_edge(current_block, lower_block, e);
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
            let outgoing = [self.ssa.false_edge_of(&upper_block),
                            self.ssa.true_edge_of(&upper_block),
                            self.ssa.next_edge_of(&upper_block)
                            ];
            for (i, edge) in outgoing.iter().enumerate() {
                if *edge != self.ssa.invalid_edge() {
                    let target = self.ssa.target_of(edge);
                    if lower_block != target {
                        self.ssa.add_control_edge(lower_block, target, i as u8);
                        self.ssa.remove_control_edge(*edge);
                    }
                }
            }

            self.ssa.add_control_edge(upper_block, lower_block, UNCOND_EDGE);
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
                let operands = self.ssa.get_sparse_operands(&ni);
                // Check if the operands belong to the upper block and if they do, remove the
                // operands edge, add a new phi and connect the edge from phi to ni.
                for operand_ in &operands {
                    let (i, operand) = *operand_;
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
                        self.ssa.disconnect(&ni, &operand);
                        let output_varid = self.outputs[&operand];
                        let mut at_ = at;
                        let replacement_phi = self.read_variable(&mut at_, output_varid);
                        self.ssa.op_use(ni, i, replacement_phi);
                    }
                }
            }
        } else {
            self.blocks.insert(at, lower_block);
        }
        lower_block
    }

    pub fn add_edge(&mut self, source: MAddress, target: MAddress, cftype: u8) {
        let source_block = self.block_of(source).unwrap();
        let target_block = self.block_of(target).unwrap();
        radeco_trace!("phip_add_edge|{} --{}--> {}", source, cftype, target);
        self.ssa.add_control_edge(source_block, target_block, cftype);
    }

    // Adds an unconditional edge the the next block if there are no edges for the current block.
    pub fn maybe_add_edge(&mut self, source: MAddress, target: MAddress) {
        let source_block = self.block_of(source).unwrap();
        if self.ssa.edges_of(&source_block).is_empty() {
            let target_block = self.block_of(target).unwrap();
            if target_block != source_block {
                radeco_trace!("phip_add_edge|{} --{}--> {}", source, UNCOND_EDGE, target);
                self.ssa.add_control_edge(source_block, target_block, UNCOND_EDGE);
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

                let zz = self.current_def[variable].clone();
                for (key, value) in zz {
                    if value.eq(&node) {
                        self.current_def[variable].insert(key, nx);
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
            radeco_trace!("phip_add_phi_operands_src|{} ({:?})|{:?}",
                          variable,
                          self.regfile.whole_names.get(variable),
                          datasource);
            self.ssa.phi_use(phi, datasource)
        }
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: T::ValueRef) -> T::ValueRef {
        let undef = self.ssa.invalid_value();
        // The phi is unreachable or in the start block
        let mut same: T::ValueRef = undef;
        for op in self.ssa.args_of(phi) {
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
                              .get_node_data(&phi)
                              .expect("No Data associated with this node!")
                              .vt;
            let block_addr = self.addr_of(&block);
            same = self.add_undefined(block_addr, valtype);
        }

        //let users = self.ssa.uses_of(phi); //This works too instead of the code below.
        let mut users: Vec<T::ValueRef> = Vec::new(); 
        //I feel this is better is there is no comparison with phi value that is being replaced.
        // Better not, when get_node_data called for the phi node, it has been removed, so raise
        // an error.
        for uses in self.ssa.uses_of(phi) {
            if !users.contains(&uses) {
                users.push(uses);
            }
        }
        // Reroute all uses of phi to same and remove phi
        self.ssa.replace(phi, same);
        // Try to recursively remove all phi users, which might have become trivial
        for use_ in users {
            if use_ == phi {
                continue;
            }
            if true || false { //TODO: Need suggestion
                match self.ssa.get_node_data(&use_) {
                    Ok(NodeData {vt: _, nt: NodeType::Phi}) => {
                        self.try_remove_trivial_phi(use_);
                    },
                    _ => {}
                }
            } else { //XXX: Remove this?
                if self.ssa.get_node_data(&use_).is_ok() {
                    self.try_remove_trivial_phi(use_);
                }
            }
        }
        same
    }

    pub fn add_dynamic(&mut self) -> T::ActionRef {
        let action = self.ssa.add_dynamic();
        let dyn_addr = MAddress::new(u64::MAX, 0);
        self.blocks.insert(dyn_addr, action);
        self.incomplete_phis.insert(dyn_addr, HashMap::new());
        self.sync_register_state(action);
        action
    }

    pub fn sync_register_state(&mut self, block: T::ActionRef) {
        let rs = self.ssa.registers_at(&block);
        for var in 0..self.variable_types.len() {
            let mut addr = self.addr_of(&block);
            let val = self.read_variable(&mut addr, var);
            self.ssa.op_use(rs, var as u8, val);
        }
    }

    pub fn mark_start_node(&mut self, block: &T::ActionRef) {
        self.ssa.mark_start_node(block);
    }

    pub fn mark_exit_node(&mut self, block: &T::ActionRef) {
        self.ssa.mark_exit_node(block);
    }

    // These functions take in address and automatically determine the block that
    // they should
    // belong to and make the necessary mappings internally. This is useful as the
    // outer struct
    // never has to keep track of the current_block or determine which block a
    // particular
    // instruction has to go into.

    fn add_phi(&mut self, address: &mut MAddress, vt: ValueType) -> T::ValueRef {
        let i = self.ssa.add_phi(vt);
        self.index_to_addr.insert(i, *address);
        address.offset += 1;
        i
    }

    // Constants need not belong to any block. They are stored as a separate table.
    pub fn add_const(&mut self, value: u64) -> T::ValueRef {
        // All consts are assumed to be 64 bit wide.
        self.ssa.add_const(value)
    }

    pub fn add_undefined(&mut self, address: MAddress, vt: ValueType) -> T::ValueRef {
        let i = self.ssa.add_undefined(vt);
        self.index_to_addr.insert(i, address);
        i
    }

    pub fn add_comment(&mut self, address: MAddress, vt: ValueType, msg: String) -> T::ValueRef {
        let i = self.ssa.add_comment(vt, msg);
        self.index_to_addr.insert(i, address);
        i
    }

    // TODO: Add a more convenient method to add an opcode and operands to it.
    // Something like the previous verified_add_op.

    pub fn add_op(&mut self, op: &MOpcode, address: &mut MAddress, vt: ValueType) -> T::ValueRef {
        let i = self.ssa.add_op(*op, vt, None);
        match *op {
            MOpcode::OpConst(_) => { },
            _ => { self.index_to_addr.insert(i, *address); },
        }
        address.offset += 1;
        i
    }

    pub fn set_address(&mut self, node: &T::ValueRef, address: MAddress) {
        self.index_to_addr.insert(*node, address);
    }

    pub fn read_register(&mut self, address: &mut MAddress, var: &str) -> T::ValueRef {
        radeco_trace!("phip_read_reg|{}", var);

        let info = match self.regfile.get_info(var) {
            Some(reg) => reg,
            None => {
                writeln!(io::stderr(), "Error: Radeco haven't supported float nowadays");
                process::exit(1);
            }
        };
        let id = info.base;
        let mut value = self.read_variable(address, id);

        let width = self.operand_width(&value);

        if info.shift > 0 {
            let shift_amount_node = self.add_const(info.shift as u64);
            let opcode = MOpcode::OpLsr;
            let vtype = From::from(width);
            let op_node = self.add_op(&opcode, address, vtype);
            self.op_use(&op_node, 0, &value);
            self.op_use(&op_node, 1, &shift_amount_node);
            value = op_node;
        }

        if info.width < width as usize {
            let opcode = MOpcode::OpNarrow(info.width as u16);
            let vtype = From::from(info.width);
            let op_node = self.add_op(&opcode, address, vtype);
            self.op_use(&op_node, 0, &value);
            value = op_node;
        }

        radeco_trace!("phip_read_reg|{:?}", value);
        value
    }

    pub fn write_register(&mut self, address: &mut MAddress, var: &str, mut value: T::ValueRef) {

        radeco_trace!("phip_write_reg|{}<-{:?}", var, value);

        let info = self.regfile.get_info(var).unwrap();
        let id = info.base;

        let width = match self.variable_types[id] {
            ValueType::Integer { width } => width,
        };

        if info.width >= width as usize {
            self.write_variable(*address, id, value);
            return;
        }

        let vt = From::from(width);
        let opcode = MOpcode::OpWiden(width as u16);

        if self.operand_width(&value) < width {
            let opcode_node = self.add_op(&opcode, address, vt);
            self.op_use(&opcode_node, 0, &value);
            value = opcode_node;
        }

        if info.shift > 0 {
            let shift_amount_node = self.add_const(info.shift as u64);
            let opcode_node = self.add_op(&MOpcode::OpLsl, address, vt);
            self.op_use(&opcode_node, 0, &value);
            self.op_use(&opcode_node, 1, &shift_amount_node);
            value = shift_amount_node;
        }

        let fullval: u64 = !((!1u64) << (width - 1));
        let maskval: u64 = ((!((!1u64) << (info.width - 1))) << info.shift) ^ fullval;

        if maskval == 0 {
            self.write_variable(*address, id, value);
            return;
        }

        let mut ov = self.read_variable(address, id);
        let maskvalue_node = self.add_const(maskval);

        let op_and = self.add_op(&MOpcode::OpAnd, address, vt);
        self.op_use(&op_and, 0, &ov);
        self.op_use(&op_and, 1, &maskvalue_node);

        ov = op_and;
        let op_or = self.add_op(&MOpcode::OpOr, address, vt);
        self.op_use(&op_or, 0, &value);
        self.op_use(&op_or, 1, &ov);
        value = op_or;
        self.write_variable(*address, id, value);
    }

    pub fn op_use(&mut self, op: &T::ValueRef, index: u8, arg: &T::ValueRef) {
        self.ssa.op_use(*op, index, *arg)
    }

    pub fn operand_width(&self, node: &T::ValueRef) -> u16 {
        match self.ssa.get_node_data(node).unwrap().vt {
            ValueType::Integer{ref width} => *width,
        }
    }

    fn new_block(&mut self, bb: MAddress) -> T::ActionRef {
        if let Some(b) = self.blocks.get(&bb) {
            *b
        } else {
            let block = self.ssa.add_block(bb);
            self.incomplete_phis.insert(bb, HashMap::new());
            block
        }
    }

    // Determine which block as address should belong to.
    // This basically translates to finding the greatest address that is less that
    // `address` in self.blocks. This is fairly simple as self.blocks is sorted.
    pub fn block_of(&self, address: MAddress) -> Option<T::ActionRef> {
        let mut last = None;
        let start_address = {
            let start = self.ssa.start_node();
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
        self.ssa.address(block).unwrap()
    }

    pub fn associate_block(&mut self, node: &T::ValueRef, addr: MAddress) {
        let block = self.block_of(addr);
        self.ssa.add_to_block(*node, block.unwrap(), addr);
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

        for node in self.ssa.nodes() {
            if let Some(addr) = self.index_to_addr.get(&node).cloned() {
                self.associate_block(&node, addr);
                // Mark selector.
                if let Ok(ndata) = self.ssa.get_node_data(&node) {
                    if let NodeType::Op(MOpcode::OpITE) = ndata.nt {
                        let block = self.block_of(addr);
                        let cond_node = self.ssa.get_operands(&node)[0];
                        self.ssa.mark_selector(cond_node, block.unwrap());
                        self.ssa.remove(node);
                    }
                }
            }
        }

        self.ssa.map_registers(self.regfile.whole_names.clone());
    }
}
