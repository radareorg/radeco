// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

// Eventually we want to specifiy replacements like this
// Add(x, Neg(y)) -> Sub(x, y)
// Sub(x, x) -> 0
//
// This file contains the datastructures to hold fragments like "Add(x, Neg(y))"
//
// variables = ["x", "y"]
// nodes = [
//     /*0*/ varindex=0 pattern=Any
//     /*1*/ varindex=1 pattern=Any
//     /*2*/ varindex=_ pattern=Op1(Neg, 1)
//     /*3*/ varindex=_ pattern=Op2(Add, 0, 1)
// ]

use middle::ir;
use super::ssa_traits::{SSA, SSAMod};

struct Variable {
    name: String,
}

type Ref = u16;

enum Pattern {
    Any,
    Const(u8),
    ConstAny,
    Op0(ir::MOpcode),
    Op1(ir::MOpcode, Ref),
    Op2(ir::MOpcode, Ref, Ref),
}

struct Node {
    varindex: u8,
    pattern:  Pattern
}

struct SSAQuote {
    variables: Vec<Variable>,
    nodes:     Vec<Node>, // contains the quoted trees in postorder
}

trait Binding {
    fn get_valueref(&self, &SSA, &Variable) -> SSA::ValueRef;
    fn get_value(&self, &SSA, &Variable) -> u64;
    fn set_valueref(&mut self, &SSA, &Variable, SSA::ValueRef);
    fn set_value(&mut self, &SSA, &Variable, u64);
}

impl SSAQuote {
    fn insert_into(&self, ssa: &mut SSAMod, block: T::ActionRef, binding: &Binding) {
        let mut indices = Vec::with_capacity(self.nodes.len());
        for node in &self.nodes {
            let i = match node.pattern {
                Any      => binding.get_valueref(ssa, &self.variables[node.varindex]),
                Const(n) => ssa.add_const(block, n),
                ConstAny => ssa.add_const(block, binding.get_value(ssa, &self.variables[node.varindex])),
                Op0(op)  => ssa.add_op(block, op),
                Op1(op, op1r) => {
                    let n = ssa.add_op(block, op);
                    ssa.op_use(n, 0, indices[op1r as usize]);
                    n
                },
                Op2(op, op1r, op2r) => {
                    let n = ssa.add_op(block, op);
                    ssa.op_use(n, 0, indices[op1r as usize]);
                    ssa.op_use(n, 1, indices[op2r as usize]);
                    n
                },
            };
            indices.push(i);
        }
    }

    fn extract_from(&self, ssa: &SSA, binding: &mut Binding) {
        unimplemented!();
        //for node in reverse &self.nodes
    }
}
