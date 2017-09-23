// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements operands' sort for SSA, which could help futher
//! analysis.

use std::collections::HashMap;
use std::cmp::Ordering;
use std::marker::PhantomData;

use middle::ssa::ssa_traits::{SSA, SSAMod, SSAWalk};
use middle::ssa::ssa_traits::{NodeType, NodeData};
use middle::ir::MOpcode;


// NOTE: I am not sure where to put this file, in analysis or in middle End.
// If we add this into middle/ssa/ssastorage.rs, it seems there are too much
// trivial code. Thus, I make this as a single file. 

const PUNDEFINED: u16 = 0;
const PCOMMENT  : u16 = 1;
const PPHI      : u16 = 2;
const POPADD    : u16 = 3;
const POPSUB    : u16 = 4;
const POPMUL    : u16 = 5;
const POPDIV    : u16 = 6;
const POPMOD    : u16 = 7;
const POPAND    : u16 = 8;
const POPOR     : u16 = 9;
const POPXOR    : u16 = 10;
const POPNOT    : u16 = 11;
const POPEQ     : u16 = 12;
const POPCMP    : u16 = 13;
const POPGT     : u16 = 14;
const POPLT     : u16 = 15;
const POPLSL    : u16 = 16;
const POPLSR    : u16 = 17;
const POPIF     : u16 = 18;
const POPJMP    : u16 = 19;
const POPCJMP   : u16 = 20;
const POPCALL   : u16 = 21;
const POPLOAD   : u16 = 22;
const POPSTORE  : u16 = 23;
const POPNARROW : u16 = 24;
const POPWIDEN  : u16 = 25;
const POPWIDEN1 : u16 = 26;
const POPCONST  : u16 = 27;
const POPNOP    : u16 = 28;
const POPINVALID: u16 = 29;
const POPITE    : u16 = 30;
const POPROL    : u16 = 31;
const POPROR    : u16 = 32;

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
                match opc {
                    MOpcode::OpAdd   => (POPADD, 0),
                    MOpcode::OpAnd   => (POPAND, 0),
                    MOpcode::OpCJmp  => (POPCJMP, 0),
                    MOpcode::OpCall  => (POPCALL, 0),
                    MOpcode::OpCmp   => (POPCMP, 0),
                    MOpcode::OpConst(num)  => (POPCONST, num),
                    MOpcode::OpDiv   => (POPDIV, 0),
                    MOpcode::OpEq    => (POPEQ, 0),
                    MOpcode::OpGt    => (POPGT, 0),
                    MOpcode::OpITE   => (POPITE, 0),
                    MOpcode::OpIf    => (POPIF, 0),
                    MOpcode::OpInvalid   => (POPINVALID, 0),
                    MOpcode::OpJmp   => (POPJMP, 0),
                    MOpcode::OpLoad  => (POPLOAD, 0),
                    MOpcode::OpLsl   => (POPLSL, 0),
                    MOpcode::OpLsr   => (POPLSR, 0),
                    MOpcode::OpLt    => (POPLT, 0),
                    MOpcode::OpMod   => (POPMOD, 0),
                    MOpcode::OpMul   => (POPMUL, 0),
                    MOpcode::OpNarrow(num) => (POPNARROW, num as u64),
                    MOpcode::OpNop   => (POPNOP, 0),
                    MOpcode::OpNot   => (POPNOT, 0),
                    MOpcode::OpOr    => (POPOR, 0),
                    MOpcode::OpRol   => (POPROL, 0),
                    MOpcode::OpRor   => (POPROR, 0),
                    MOpcode::OpStore => (POPSTORE, 0),
                    MOpcode::OpSub   => (POPSUB, 0),
                    MOpcode::OpZeroExt(num)  => (POPWIDEN, num as u64),
                    MOpcode::OpSignExt(num)  => (POPWIDEN1, num as u64),
                    MOpcode::OpXor   => (POPXOR, 0),
                }
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

        // consider the equal situation.
        match priority1.0 {
            PUNDEFINED | POPNOP => {
                return self.return_value(Ordering::Equal, op1, op2);    
            }
            PPHI | 
            PCOMMENT |
            POPCALL |
            POPIF |
            POPJMP |
            POPCJMP |
            POPLOAD |
            POPSTORE |
            POPITE => { 
                let addr1 = self.ssa.address(op1)
                                        .expect("No address information found");
                let addr2 = self.ssa.address(op2)
                                        .expect("No address information found");
                return self.return_value(addr1.cmp(&addr2), op1, op2);
            }
            POPCONST => {
                return self.return_value(priority1.1.cmp(&priority2.1), op1, op2);
            }
            POPNARROW | POPWIDEN => {
                if priority1.1.cmp(&priority2.1) != Ordering::Equal {
                    return self.return_value(priority1.1.cmp(&priority2.1), op1, op2);
                } else {
                    let order = self.compare_operands(op1, op2);
                    return self.return_value(order, op1, op2);
                }
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
                NodeType::Op(opc) if opc.is_commutative() => {
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

