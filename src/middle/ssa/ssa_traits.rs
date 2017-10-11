// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Defines the traits to be implemented by SSA data structures.
//!
//! These traits extend upon the ones provided in `cfg_traits`
//!
//! # Design
//!
//!  * `SSA` - Analogous to `CFG` this trait provides __accessors__ to
//!  the data: Methods to enumerate operations, discovered connected operations,
//!  determine which operation a basic block is in. etc.
//!
//!  * `SSAMod` - This trait provides methods to __manipulate__ operation nodes.
//!
//!  * `SSAExtras` - Provides additional methods that allow associated information to be stored
//!
//! The associated type `SSA::ValueRef` is used by the methods to refer to
//! nodes.

use std::hash::Hash;
use std::fmt::{self, Debug};

use middle::ir;
use super::cfg_traits::{CFG, CFGMod};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Defines the high level `type` of value for a node. It is only used to differentiate between
/// references and non-reference (scalar) types.
pub enum ValueType {
    /// Pointer or reference to code or memory
    Reference,
    /// Not a pointer type
    Scalar,
    /// Not (yet) resolved to be a reference or a constant
    Unresolved,
    /// Invalid/Unconsistent
    Invalid,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Information for a `ValueRef`
pub struct ValueInfo {
    /// `Type` of value
    vty: ValueType,
    /// `width` of the operation, should be in 1, 2, 4, 8, 16, 32, 64, 128
    width: ir::WidthSpec,
}

macro_rules! scalar {
    ($w:expr) => {
        ValueInfo::new($crate::middle::ssa::ssa_traits::ValueType::Scalar, ir::WidthSpec::new_known($w))
    }
}

impl ValueInfo {
    pub fn new(vty: ValueType, width: ir::WidthSpec) -> ValueInfo {
        ValueInfo {
            vty: vty,
            width: width,
        }
    }

    pub fn width(&self) -> &ir::WidthSpec {
        &self.width
    }

    pub fn new_unresolved(width: ir::WidthSpec) -> ValueInfo {
        ValueInfo::new(ValueType::Unresolved, width)
    }

    pub fn new_scalar(width: ir::WidthSpec) -> ValueInfo {
        ValueInfo::new(ValueType::Scalar, width)
    }

    pub fn new_reference(width: ir::WidthSpec) -> ValueInfo {
        ValueInfo::new(ValueType::Reference, width)
    }

    pub fn mark_as_scalar(&mut self) {
        self.vty = ValueType::Scalar;
    }

    pub fn mark_as_reference(&mut self) {
        self.vty = ValueType::Reference;
    }

    pub fn mark_as_invalid(&mut self) {
        self.vty = ValueType::Invalid;
    }

    pub fn value_type(&self) -> &ValueType {
        &self.vty
    }

    pub fn is_scalar(&self) -> bool {
        self.vty == ValueType::Scalar
    }

    pub fn is_reference(&self) -> bool {
        self.vty == ValueType::Reference
    }
}

pub struct BBInfo;

/// Value node without operands
#[derive(Clone, Debug)]
pub enum NodeType {
    /// An operation node with the specified opcode.
    Op(ir::MOpcode),
    /// A phi node.
    Phi,
    /// A node with unknown value.
    Undefined,
    /// Generic comment, used to represent other data
    Comment(String),
}

// Implement display helper for NodeData to make it a little nicer to read prefix notation.
impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            &NodeType::Op(ref op) => format!("{}", op),
            &NodeType::Phi => "Phi".to_owned(),
            &NodeType::Comment(ref s) => s.clone(),
            // Don't care about these
            _ => String::new(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug)]
/// Value node without operands with `ValueInfo`
pub struct NodeData {
    pub vt: ValueInfo,
    pub nt: NodeType,
}

/// Trait for the SSA Form implementation.
// This trait ensures that any other ssa form will be compatible with our
// implementations provided
// the SSA form implements the following traits.
pub trait SSA: CFG {
    type ValueRef: Eq + Hash + Clone + Copy + Debug; // We could drop the Copy trait later and insert .clone()

    ///////////////////////////////////////////////////////////////////////////
    //// Node accessors and helpers
    ///////////////////////////////////////////////////////////////////////////

    /// Check if the node at the given index is a expression or not.
    fn is_expr(&self, i: Self::ValueRef) -> bool;

    /// Check if the node at the given index is a phi node or not.
    fn is_phi(&self, i: Self::ValueRef) -> bool;

    /// Returns true if the expression acts as a `Selector` for control flow.
    fn is_selector(&self, i: Self::ValueRef) -> bool;

    /// Get all value nodes in the whole graph.
    fn values(&self) -> Vec<Self::ValueRef>;

    /// Get expr/phi node address
    fn address(&self, ni: Self::ValueRef) -> Option<ir::MAddress>;

    /// Get all the NodeIndex of all operations/expressions in the BasicBlock with index 'i'.
    fn exprs_in(&self, i: Self::ActionRef) -> Vec<Self::ValueRef>;

    /// Get all phis in the BasicBlock with index 'i'.
    fn phis_in(&self, i: Self::ActionRef) -> Vec<Self::ValueRef>;

    /// Get a node that has all register values at the beginning of the specified basic block as args
    fn registers_in(&self, i: Self::ActionRef) -> Option<Self::ValueRef>;

    /// Returns the selector for the Block.
    fn selector_in(&self, i: Self::ActionRef) -> Option<Self::ValueRef>;

    /// Get the Block for which i acts as a selector.
    fn selector_for(&self, i: Self::ValueRef) -> Option<Self::ActionRef>;

    /// Get all the uses of the node with index 'i'.
    fn uses_of(&self, i: Self::ValueRef) -> Vec<Self::ValueRef>;

    /// Get the operands for the operation with NodeIndex 'i'.
    fn operands_of(&self, i: Self::ValueRef) -> Vec<Self::ValueRef>;

    /// Get the operands for the operation with NodeIndex 'i' as tuples.
    fn sparse_operands_of(&self, i: Self::ValueRef) -> Vec<(u8, Self::ValueRef)>;

    /// Get the NodeIndex of the BasicBlock to which node with index 'i' belongs to.
    fn block_for(&self, i: Self::ValueRef) -> Option<Self::ActionRef>;

    /// Get the actual NodeData.
    fn node_data(&self, i: Self::ValueRef) -> Result<NodeData, Box<Debug>>;

    /// Get const information, as a pack of get_node_data on a OpConst node.
    fn constant(&self, i: Self::ValueRef) -> Option<u64>;

    /// Get comment information, as a pack of get_node_data on a Comment data.
    fn comment(&self, i: Self::ValueRef) -> Option<String>;

    /// Get OpCode information, as a pack of get_node_data on a Comment data.
    fn opcode(&self, i: Self::ValueRef) -> Option<ir::MOpcode>;

    /// Get information of the register which belongs to the node.
    fn registers(&self, _: Self::ValueRef) -> Vec<String>;

    /// Return invalid value
    fn invalid_value(&self) -> Option<Self::ValueRef>;
}

/// Trait for modifying SSA data
pub trait SSAMod: SSA + CFGMod {

    /// Set the address of a value
    fn set_address(&mut self, i: Self::ValueRef, addr: ir::MAddress);

    /// Set the register information for a value
    fn set_register(&mut self, i: Self::ValueRef, regname: String);

    /// Set the node as selector for the control edges away from the specified basic block
    fn set_selector(&mut self, node: Self::ValueRef, block: Self::ActionRef);

    /// Insert a new operation node.
    fn insert_op(&mut self, opc: ir::MOpcode, vt: ValueInfo, addr: Option<u64>) -> Option<Self::ValueRef>;

    /// Add a new constant node.
    fn insert_const(&mut self, value: u64) -> Option<Self::ValueRef>;

    /// Add a new phi node.
    fn insert_phi(&mut self, vt: ValueInfo) -> Option<Self::ValueRef>;

    /// Add a new undefined node
    fn insert_undefined(&mut self, vt: ValueInfo) -> Option<Self::ValueRef>;

    /// Add a new comment node
    fn insert_comment(&mut self, vt: ValueInfo, msg: String) -> Option<Self::ValueRef>;
    
    /// Associate a node with index n with a block
    fn insert_into_block(&mut self, node: Self::ValueRef, block: Self::ActionRef, ir::MAddress);

    /// Add a data source to a phi node.
    fn phi_use(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

    /// Remove a data source from a phi node.
    fn phi_unuse(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

    /// Set the index-th argument of the node.
    fn op_use(&mut self, node: Self::ValueRef, index: u8, argument: Self::ValueRef);

    /// Remove a data source from a expr node.
    fn op_unuse(&mut self, op: Self::ValueRef, operand: Self::ValueRef);

    /// Replace one node by another within one basic block.
    fn replace_value(&mut self, node: Self::ValueRef, replacement: Self::ValueRef);

    /// Remove a node without replacement
    fn remove_value(&mut self, node: Self::ValueRef);

    /// Remove control flow edge. This is a part of SSAMod as this potentially modifies the ssa.
    fn remove_data_edge(&mut self, i: Self::CFEdgeRef);

    /// Map register names into SSA Graph
    fn map_registers(&mut self, regs: Vec<String>);
}

/// Extras. TODO
///
/// Design requirement - For a method to be a part of `SSAExtras`, it __has__ to have a default
/// implementation that would work out of the box. Since these methods are only extras and do not
/// add any major functionality, but rather just convinence or display glitter, the user must not
/// be burdened with implementing this. All methods must return `Option<T>` to ensure this.

pub trait SSAExtra: SSA {
    fn mark(&mut self, _: &Self::ValueRef) { }
    fn clear_mark(&mut self, &Self::ValueRef) { }
    fn set_color(&mut self, _: &Self::ValueRef, _: u8) { }
    fn set_comment(&mut self, _: &Self::ValueRef, _: String) { }
    fn add_flag(&mut self, _: &Self::ValueRef, _: String) { }
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

pub trait SSAWalk<I: Iterator<Item=<Self as SSA>::ValueRef>>: SSA {
    fn bfs_walk(&self) -> I;
    fn inorder_walk(&self) -> I;
    fn dfs_walk(&self) -> I;
}
