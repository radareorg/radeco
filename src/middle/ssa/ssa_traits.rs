// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Defines the traits to be implemented by SSA data structures.
//!
//! These traits extend upon the ones provided in cfg_traits.
//!
//! # Design
//!
//!  * `SSA` - Analogous to `CFG` this trait provides __accessors__ to
//!  the data: Methods to enumerate operations, discovered connected operations,
//!  determine which operation a basic block is in. etc.
//!
//!  * `SSAMod` - This trait provides methods to __manipulate__ operation nodes.
//!
//!  * `SSAExtras` - TODO
//!
//! The associated type `SSA::ValueRef` is used by the methods to refer to
//! nodes.
//!  ValueRefs are invalidated by removal or replacement of the target node.
//!  You can revalidate a ValueRef by calling `SSA::refresh` on it.
//!  Currently `SSAStorage` automatically uses `refresh` on arguments of its
//!  `*_use`, `get_node_data` and `replace` methods.

use std::hash::Hash;
use std::fmt::Debug;
use middle::ir;
use super::cfg_traits::{CFG, CFGMod};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueType {
    Integer {
        width: ir::WidthSpec,
    },
}

impl<'a> From<&'a str> for ValueType {
    fn from(v: &'a str) -> ValueType {
        let t = v.to_owned().chars().nth(0).unwrap();

        let w = match t {
            'i' => v[1..].parse::<u16>().unwrap(),
            _ => unimplemented!(),
        };

        ValueType::Integer { width: w }
    }
}

impl From<String> for ValueType {
    fn from(v: String) -> ValueType {
        From::from(&*v)
    }
}

impl From<u16> for ValueType {
    fn from(v: u16) -> ValueType {
        ValueType::Integer { width: v as ir::WidthSpec }
    }
}

impl From<usize> for ValueType {
    fn from(v: usize) -> ValueType {
        ValueType::Integer { width: v as ir::WidthSpec }
    }
}

/// Data associated with a basic block. Use is optional.
#[derive(Clone, Debug)]
pub struct BBInfo {
    pub addr: u64,
}

/// Value node without operands
#[derive(Clone, Debug)]
pub enum NodeType {
    /// An operation node with the specified opcode.
    Op(ir::MOpcode),
    /// A phi node.
    Phi,
    /// A node with unknown value.
    Undefined,
}

/// Value node without operands with `ValueType`
#[derive(Clone, Debug)]
pub struct NodeData {
    pub vt: ValueType,
    pub nt: NodeType,
}

/// Trait for the SSA Form implementation.
// This trait ensures that any other ssa form will be compatible with our
// implementations provided
// the SSA form implements the following traits.
pub trait SSA: CFG {
	type ValueRef: Eq + Hash + Clone + Copy + Debug; // We could drop the Copy trait later and insert .clone()

    /// ////////////////////////////////////////////////////////////////////////
    /// / Node accessors and helpers
    /// ////////////////////////////////////////////////////////////////////////

    /// Get all the NodeIndex of all operations/expressions in the BasicBlock with index 'i'.
    fn exprs_in(&self, i: &Self::ActionRef) -> Vec<Self::ValueRef>;

    /// Check if the node at the given index is a expression or not.
    fn is_expr(&self, i: &Self::ValueRef) -> bool;

    /// Get all phis in the BasicBlock with index 'i'.
    fn get_phis(&self, i: &Self::ActionRef) -> Vec<Self::ValueRef>;

    /// Get all the uses of the node with index 'i'.
    fn get_uses(&self, i: &Self::ValueRef) -> Vec<Self::ValueRef>;

    /// Get the NodeIndex of the BasicBlock to which node with index 'i' belongs to.
    fn get_block(&self, i: &Self::ValueRef) -> Self::ActionRef;

    /// Get the operands for the operation with NodeIndex 'i'.
    fn get_operands(&self, i: &Self::ValueRef) -> Vec<Self::ValueRef>;

    /// Get the operands for the operation with NodeIndex 'i' as tuples.
    fn get_sparse_operands(&self, i: &Self::ValueRef) -> Vec<(u8, Self::ValueRef)>;

    /// Get the lhs() of the Operation with NodeIndex 'i'.
    fn lhs(&self, i: &Self::ValueRef) -> Self::ValueRef {
        self.get_operands(i)[0].clone()
    }

    /// Get the rhs() of the Operation with NodeIndex 'i'.
    fn rhs(&self, i: &Self::ValueRef) -> Self::ValueRef {
        self.get_operands(i)[1].clone()
    }

    /// Get the actual NodeData.
    fn get_node_data(&self, i: &Self::ValueRef) -> Result<NodeData, Box<Debug>>;

    /// Returns true if the expression acts as a `Selector` for control flow.
    fn is_selector(&self, i: &Self::ValueRef) -> bool;

    /// Returns the selector for the Block.
    fn selector_of(&self, i: &Self::ActionRef) -> Option<Self::ValueRef>;

    /// Get the Block for which i acts as a selector.
    fn selects_for(&self, i: &Self::ValueRef) -> Self::ActionRef;

    /// Get Jump target of a call or an unconditional jump.
    fn get_target(&self, i: &Self::ValueRef) -> Self::ActionRef;

    /// Get branches of a selector (false_branch, true_branch).
    fn get_branches(&self, i: &Self::ValueRef) -> (Self::ActionRef, Self::ActionRef);

    /// Helper method that gets only the true branch.
    fn get_true_branch(&self, i: &Self::ValueRef) -> Self::ActionRef {
        self.get_branches(i).1
    }

    /// Helper method that gets only the false branch.
    fn get_false_branch(&self, i: &Self::ValueRef) -> Self::ActionRef {
        self.get_branches(i).0
    }

    /// Gets the data dependencies of a value node in any order.
    /// (See get_operands for ordered return value)
    fn args_of(&self, node: Self::ValueRef) -> Vec<Self::ValueRef>;

    /// Gets uses dependents of a value node.
    /// (Equivalent as `SSAMod::get_uses`)
    fn uses_of(&self, node: Self::ValueRef) -> Vec<Self::ValueRef>;

    /// Get the NodeIndex of the BasicBlock to which node with index 'i' belongs to.
    /// (Alias for get_block)
    fn block_of(&self, i: &Self::ValueRef) -> Self::ActionRef {
        self.get_block(i)
    }

    /// Get a node that has all register values at the beginning of the specified basic block as args
    fn registers_at(&self, i: &Self::ActionRef) -> Self::ValueRef;

    fn invalid_value(&self) -> Self::ValueRef;

    fn to_value(&self, Self::ActionRef) -> Self::ValueRef;
    fn to_action(&self, Self::ValueRef) -> Self::ActionRef;

    fn node_count(&self) -> usize;
    fn edge_count(&self) -> usize;
    fn nodes(&self) -> Vec<Self::ValueRef>;
}

/// Trait for modifying SSA data
pub trait SSAMod: SSA + CFGMod {

    /// Add a new operation node.
    fn add_op(&mut self,
              block: Self::ActionRef,
              opc: ir::MOpcode,
              vt: ValueType,
              addr: Option<u64>)
              -> Self::ValueRef;

    /// Add a new constant node.
    fn add_const(&mut self, block: Self::ActionRef, value: u64) -> Self::ValueRef;

    /// Add a new phi node.
    fn add_phi(&mut self, block: Self::ActionRef, vt: ValueType) -> Self::ValueRef;

    /// Add a new undefined node
    fn add_undefined(&mut self, block: Self::ActionRef, vt: ValueType) -> Self::ValueRef;

    /// Add a new comment node
    fn add_comment(&mut self,
                   block: Self::ActionRef,
                   vt: ValueType,
                   msg: String)
                   -> Self::ValueRef;

    /// Mark the node as selector for the control edges away from the specified basic block
    fn mark_selector(&mut self, node: Self::ValueRef, block: Self::ActionRef);

    /// Add a data source to a phi node.
    fn phi_use(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

    /// Remove a data source from a phi node.
    fn phi_unuse(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

    /// Set the index-th argument of the node.
    fn op_use(&mut self, node: Self::ValueRef, index: u8, argument: Self::ValueRef);

    /// Replace one node by another within one basic block.
    fn replace(&mut self, node: Self::ValueRef, replacement: Self::ValueRef);

    /// Remove a node without replacement
    fn remove(&mut self, node: Self::ValueRef);

    /// Remove control flow edge. This is a part of SSAMod as this potentially modifies the ssa.
    fn remove_edge(&mut self, i: &Self::CFEdgeRef);
}

/// Extras. TODO
///
/// Design requirement - For a method to be a part of SSAExtras, it __has__ to have a default
/// implementation that would work out of the box. Since these methods are only extras and do not
/// add any major functionality, but rather just convinence or display glitter, the user must not
/// be burdened with implementing this. All methods must return `Option<T>` to ensure this.
pub trait SSAExtra: SSA {
    fn mark(&mut self, _: &Self::ValueRef) {
        ();
    }

    fn clear_mark(&mut self, &Self::ValueRef) {
        ();
    }

    fn set_color(&mut self, _: &Self::ValueRef, _: u8) {
        ();
    }

    fn set_comment(&mut self, _: &Self::ValueRef, _: String) {
        ();
    }

    fn set_addr(&mut self, _: &Self::ValueRef, _: String) {
        ();
    }

    fn add_flag(&mut self, _: &Self::ValueRef, _: String) {
        ();
    }

    fn is_marked(&self, _: &Self::ValueRef) -> bool {
        false
    }

    fn color(&self, _: &Self::ValueRef) -> Option<u8> {
        None
    }

    fn comments(&self, _: &Self::ValueRef) -> Option<String> {
        None
    }

    fn addr(&self, _: &Self::ValueRef) -> Option<String> {
        None
    }

    fn flags(&self, _: &Self::ValueRef) -> Option<String> {
        None
    }
}
