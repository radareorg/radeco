// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements operands' sort for SSA, which could help futher
//! analysis.

use crate::middle::ssa::ssa_traits::{NodeData, NodeType};

use crate::middle::ir::{MAddress, MOpcode};
use crate::middle::ssa::ssa_traits::{SSAMod, SSAWalk, SSA};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::marker::PhantomData;

// NOTE: I am not sure where to put this file, in analysis or in middle End.
// If we add this into middle/ssa/ssastorage.rs, it seems there are too much
// trivial code. Thus, I make this as a single file.

/// Represents the priority for SSA elements.
#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum SortPriority {
    /// Priority for undefined SSA elements.
    Undefined = 0,
    /// Priority for SSA comments.
    Comment = 1,
    /// Priority for SSA PHI elements.
    Phi = 2,
    /// Represents the priority for SSA opcodes.
    Opcode(u16) = 3,
}

impl SortPriority {
    /// Represents the priority for undefined SSA elements.
    pub const UNDEFINED: u16 = 0;
    /// Represents the priority for SSA comments.
    pub const COMMENT: u16 = 1;
    /// Represents the priority for SSA PHI elements.
    pub const PHI: u16 = 2;
    /// Represents the starting priority for opcodes.
    pub const OPCODE: u16 = 3;

    /// Converts a [`u16`] into a [SortPriority].
    #[inline]
    pub const fn from_u16(val: u16) -> Self {
        match val {
            Self::UNDEFINED => Self::Undefined,
            Self::COMMENT => Self::Comment,
            Self::PHI => Self::Phi,
            op => Self::Opcode(op),
        }
    }

    /// Converts the [SortPriority] into a [`u16`].
    #[inline]
    pub const fn to_u16(self) -> u16 {
        match self {
            Self::Undefined => Self::UNDEFINED,
            Self::Comment => Self::COMMENT,
            Self::Phi => Self::PHI,
            Self::Opcode(op) => Self::OPCODE.saturating_add(op),
        }
    }
}

impl From<u16> for SortPriority {
    fn from(val: u16) -> Self {
        Self::from_u16(val)
    }
}

impl From<SortPriority> for u16 {
    fn from(val: SortPriority) -> Self {
        val.to_u16()
    }
}

pub struct Sorter<'a, I, T>
where
    I: Iterator<Item = T::ValueRef>,
    T: 'a + SSAMod + SSA + SSAWalk<I>,
{
    sorted: HashMap<T::ValueRef, bool>,
    record: HashMap<(T::ValueRef, T::ValueRef), Ordering>,
    ssa: &'a mut T,
    foo: PhantomData<I>,
}

impl<'a, I, T> Sorter<'a, I, T>
where
    I: Iterator<Item = T::ValueRef>,
    T: 'a + SSA + SSAMod + SSAWalk<I>,
{
    pub fn new(ssa: &'a mut T) -> Sorter<'a, I, T> {
        Sorter {
            sorted: HashMap::new(),
            record: HashMap::new(),
            ssa,
            foo: PhantomData,
        }
    }

    fn get_priority(&self, node_data: NodeData) -> (SortPriority, u64) {
        match node_data.nt {
            NodeType::Undefined => (SortPriority::Undefined, 0),
            NodeType::Comment(_) => (SortPriority::Comment, 0),
            NodeType::Phi => (SortPriority::Phi, 0),
            NodeType::Op(opc) => (
                SortPriority::Opcode(opc.idx()),
                match opc {
                    MOpcode::OpConst(num) => num,
                    MOpcode::OpNarrow(num) | MOpcode::OpZeroExt(num) | MOpcode::OpSignExt(num) => {
                        num as u64
                    }
                    _ => 0,
                },
            ),
        }
    }

    // op1 and op2 are the same kind of opcode.
    // NOTE: The key point for this recursion is that it's impossible that there is a loop made by
    // use-def chain withoud any phi node. Because the recursion will stop at phi node, it's
    // impossible to become an infinite loop.
    fn compare_operands(&mut self, op1: T::ValueRef, op2: T::ValueRef) -> Ordering {
        let operands1 = self.ssa.operands_of(op1);
        let operands2 = self.ssa.operands_of(op2);
        for i in 0..operands1.len() {
            let result = self.compare(operands1[i], operands2[i]);
            if result != Ordering::Equal {
                return result;
            }
        }
        Ordering::Equal
    }

    fn return_value(&mut self, order: Ordering, op1: T::ValueRef, op2: T::ValueRef) -> Ordering {
        self.record.entry((op1, op2)).or_insert(order);
        self.record.entry((op2, op1)).or_insert(order.reverse());
        order
    }

    // TODO: make compare function more concise
    fn compare(&mut self, op1: T::ValueRef, op2: T::ValueRef) -> Ordering {
        if op1 == op2 {
            return Ordering::Equal;
        }
        if self.record.contains_key(&(op1, op2)) {
            return self
                .record
                .get(&(op1, op2))
                .copied()
                .unwrap_or(Ordering::Equal);
        }
        if !self.sorted.contains_key(&op1) {
            self.sort_operands(op1);
        }
        if !self.sorted.contains_key(&op2) {
            self.sort_operands(op2);
        }
        if self.ssa.node_data(op1).is_err() || self.ssa.node_data(op2).is_err() {
            radeco_err!("Operand node not found");
            return Ordering::Equal;
        };
        let node_data1 = self.ssa.node_data(op1).unwrap();
        let node_data2 = self.ssa.node_data(op2).unwrap();
        let (priority1, priority1_val) = self.get_priority(node_data1);
        let (priority2, priority2_val) = self.get_priority(node_data2);

        if !priority1.cmp(&priority2).is_eq() {
            return self.return_value(priority1.cmp(&priority2), op1, op2);
        }

        // Equal and has an opcode.
        if let Some(opcode) = self.ssa.opcode(op1) {
            match opcode {
                MOpcode::OpNop => {
                    return self.return_value(Ordering::Equal, op1, op2);
                }
                MOpcode::OpCall | MOpcode::OpLoad | MOpcode::OpStore | MOpcode::OpITE => {
                    let addr1 = self.ssa.address(op1).expect("No address information found");
                    let addr2 = self.ssa.address(op2).expect("No address information found");
                    return self.return_value(addr1.cmp(&addr2), op1, op2);
                }
                MOpcode::OpConst(_) => {
                    return self.return_value(priority1_val.cmp(&priority2_val), op1, op2);
                }
                MOpcode::OpSignExt(_) | MOpcode::OpZeroExt(_) | MOpcode::OpNarrow(_) => {
                    if !priority1_val.cmp(&priority2_val).is_eq() {
                        return self.return_value(priority1_val.cmp(&priority2_val), op1, op2);
                    } else {
                        let order = self.compare_operands(op1, op2);
                        return self.return_value(order, op1, op2);
                    }
                }
                _ => {}
            }
        }

        // Equal and not opcode node
        match priority1 {
            SortPriority::Undefined => self.return_value(Ordering::Equal, op1, op2),
            SortPriority::Phi | SortPriority::Comment => {
                let addr1 = self.ssa.address(op1).unwrap_or_else(|| {
                    radeco_err!("No address information found");
                    MAddress::new(0, 0)
                });
                let addr2 = self.ssa.address(op2).unwrap_or_else(|| {
                    radeco_err!("No address information found");
                    MAddress::new(0, 0)
                });
                self.return_value(addr1.cmp(&addr2), op1, op2)
            }
            _ => {
                let order = self.compare_operands(op1, op2);
                self.return_value(order, op1, op2)
            } // Opcode:
              //  For zero, opc could only be OpConst or OpInvalid.
              //  For Unary and Binary, we should consider their operands. Because
              //      they must be sorted before, we could compare them in order.
              //  For ternary, opc could only be OpITE. Is there any posibility
              //      that OpITE becomes an operand?
        }
    }

    fn sort_operands(&mut self, idx: T::ValueRef) {
        if self.sorted.contains_key(&idx) {
            return;
        }
        if let Ok(node_data) = self.ssa.node_data(idx) {
            match node_data.nt {
                NodeType::Op(ref opc) if opc.is_commutative() => {
                    let operands = self.ssa.operands_of(idx);
                    // Operands' length must be 2, for only commutative opcode
                    // could get in this function, while commutative opcodes
                    // always have two operands.
                    // TODO: replace this assert with an Err, and make this function fallible
                    assert_eq!(operands.len(), 2);
                    if self.compare(operands[0], operands[1]) == Ordering::Less {
                        self.ssa.op_unuse(idx, operands[0]);
                        self.ssa.op_unuse(idx, operands[1]);
                        self.ssa.op_use(idx, 0, operands[1]);
                        self.ssa.op_use(idx, 1, operands[0]);
                    }
                }
                _ => {}
            }
        }

        self.sorted.entry(idx).or_insert(true);
    }

    pub fn run(&mut self) {
        for idx in self.ssa.inorder_walk() {
            self.sort_operands(idx);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_priority() {
        assert_eq!(
            SortPriority::Undefined.cmp(&SortPriority::Undefined),
            Ordering::Equal
        );
        assert_eq!(
            SortPriority::Undefined.cmp(&SortPriority::Comment),
            Ordering::Less
        );
        assert_eq!(
            SortPriority::Undefined.cmp(&SortPriority::Phi),
            Ordering::Less
        );
        assert_eq!(
            SortPriority::Undefined.cmp(&SortPriority::Opcode(0)),
            Ordering::Less
        );

        assert_eq!(
            SortPriority::Comment.cmp(&SortPriority::Comment),
            Ordering::Equal
        );
        assert_eq!(
            SortPriority::Comment.cmp(&SortPriority::Phi),
            Ordering::Less
        );
        assert_eq!(
            SortPriority::Comment.cmp(&SortPriority::Opcode(0)),
            Ordering::Less
        );

        assert_eq!(SortPriority::Phi.cmp(&SortPriority::Phi), Ordering::Equal);
        assert_eq!(
            SortPriority::Phi.cmp(&SortPriority::Opcode(0)),
            Ordering::Less
        );

        assert_eq!(
            SortPriority::Opcode(0).cmp(&SortPriority::Opcode(0)),
            Ordering::Equal
        );
        assert_eq!(
            SortPriority::Opcode(0).cmp(&SortPriority::Opcode(1)),
            Ordering::Less
        );
    }
}
