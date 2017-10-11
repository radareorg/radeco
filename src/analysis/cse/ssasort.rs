// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements operands' sort for SSA, which could help futher
//! analysis.

use middle::ir::MOpcode;
use middle::ssa::ssa_traits::{NodeType, NodeData};

use middle::ssa::ssa_traits::{SSA, SSAMod, SSAWalk};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::marker::PhantomData;


// NOTE: I am not sure where to put this file, in analysis or in middle End.
// If we add this into middle/ssa/ssastorage.rs, it seems there are too much
// trivial code. Thus, I make this as a single file.

const PUNDEFINED: u16 = 0;
const PCOMMENT: u16 = 1;
const PPHI: u16 = 2;
// Starting priority for opcodes.
const OP_BASE: u16 = 3;

pub struct Sorter<'a, I, T>
    where I: Iterator<Item = T::ValueRef>,
          T: 'a + SSAMod + SSA + SSAWalk<I>
{
    sorted: HashMap<T::ValueRef, bool>,
    record: HashMap<(T::ValueRef, T::ValueRef), Ordering>,
    ssa: &'a mut T,
    foo: PhantomData<I>,
}

impl<'a, I, T> Sorter<'a, I, T>
    where I: Iterator<Item = T::ValueRef>,
          T: 'a + SSA + SSAMod + SSAWalk<I>
{
    pub fn new(ssa: &'a mut T) -> Sorter<'a, I, T> {
        Sorter {
            sorted: HashMap::new(),
            record: HashMap::new(),
            ssa: ssa,
            foo: PhantomData,
        }
    }

    fn get_priority(&self, node_data: NodeData) -> (u16, u64) {
        match node_data.nt {
            NodeType::Undefined => (PUNDEFINED, 0),
            NodeType::Comment(_) => (PCOMMENT, 0),
            NodeType::Phi => (PPHI, 0),
            NodeType::Op(opc) => {
                (opc.idx() + OP_BASE,
                 match opc {
                     MOpcode::OpConst(num) => num,
                     MOpcode::OpNarrow(num) |
                     MOpcode::OpZeroExt(num) |
                     MOpcode::OpSignExt(num) => num as u64,
                     _ => 0,
                 })
            }
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
        return Ordering::Equal;
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
            return *self.record.get(&(op1, op2)).unwrap();
        }
        if !self.sorted.contains_key(&op1) {
            self.sort_operands(op1);
        }
        if !self.sorted.contains_key(&op2) {
            self.sort_operands(op2);
        }

        let node_data1 = self.ssa.node_data(op1).expect("Operand node not found!");
        let node_data2 = self.ssa.node_data(op2).expect("Operand node not found!");
        let priority1 = self.get_priority(node_data1);
        let priority2 = self.get_priority(node_data2);

        if priority1.0.cmp(&priority2.0) != Ordering::Equal {
            return self.return_value(priority1.0.cmp(&priority2.0), op1, op2);
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
                    return self.return_value(priority1.1.cmp(&priority2.1), op1, op2);
                }
                MOpcode::OpSignExt(_) |
                MOpcode::OpZeroExt(_) |
                MOpcode::OpNarrow(_) => {
                    if priority1.1.cmp(&priority2.1) != Ordering::Equal {
                        return self.return_value(priority1.1.cmp(&priority2.1), op1, op2);
                    } else {
                        let order = self.compare_operands(op1, op2);
                        return self.return_value(order, op1, op2);
                    }
                }
                _ => {}
            }
        }

        // Equal and not opcode node
        match priority1.0 {
            PUNDEFINED => {
                return self.return_value(Ordering::Equal, op1, op2);
            }
            PPHI | PCOMMENT => {
                let addr1 = self.ssa.address(op1).expect("No address information found");
                let addr2 = self.ssa.address(op2).expect("No address information found");
                return self.return_value(addr1.cmp(&addr2), op1, op2);
            }
            _ => {
                let order = self.compare_operands(op1, op2);
                return self.return_value(order, op1, op2);
            } 
            // Opcode:
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
