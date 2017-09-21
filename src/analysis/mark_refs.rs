//! Simple data flow analysis to mark ssa nodes as references or scalars.
//!

use petgraph::graph::NodeIndex;
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::ssa_traits::{ValueInfo, ValueType, SSA};
use middle::ir::MOpcode;

use std::collections::{HashSet, VecDeque};

#[derive(Clone, Copy, Debug)]
pub struct ReferenceMarker { }

impl ReferenceMarker {

    fn compute_result(&self, op: &[ValueType]) -> ValueType {
        match (op[0], op[1]) {
            (ValueType::Invalid, _)
            | (_, ValueType::Invalid)
            | (ValueType::Reference, ValueType::Reference) => ValueType::Invalid,

            (ValueType::Reference, _) 
            | (_, ValueType::Reference) => ValueType::Reference,

            (ValueType::Scalar, ValueType::Scalar) => ValueType::Scalar,

            (_, _) => ValueType::Unresolved,
        }
    }

    fn compute_result_arr(&self, ops: &[ValueType]) -> ValueType {
        unimplemented!()
        //ops.fold(ValueType::Unresolved, |acc, &v| { compute_result(&[acc, x]) })
    }

    fn resolve_and_mark_subtree(&self, ssa: &mut SSAStorage, subtree: &NodeIndex) -> ValueType {
        let mut operands = ssa.get_operands(subtree);
        let rvt = self.compute_result_arr(operands.iter().map(|n| ssa.g[n].get_valueinfo()));
        match rvt {
            // All operands are scalar
            ValueType::Scalar => ValueType::Scalar,
            // Exactly one operand is a reference
            ValueType::Reference => ValueType::Reference,
            // More than one operand has been identified as a reference
            ValueType::Invalid => ValueType::Invalid,
            // Unresolved, need more information/analysis to determine which operand is a reference
            ValueType::Unresolved => {
                // Need to go deeper!
                let mut vtys = Vec::new();
                // Check the opcode to determine the next action.
                match opc {
                    // Either operand can be ref
                    &MOpcode::OpAdd |
                    &MOpcode::OpAnd |
                    &MOpcode::OpOr |
                    &MOpcode::OpXor => {
                        for op in &operands {
                            let v = self.resolve_and_mark_subtree(ssa, op);
                            vtys.push(vtys);
                        }
                    }

                    // Second operand can never be ref
                    &MOpcode::OpSub |
                    &MOpcode::OpMul |
                    &MOpcode::OpLsl |
                    &MOpcode::OpLsr  => { 
                        self.mark_subtreee_as_scalar(operands[1]);
                        let v = self.resolve_and_mark_subtree(ssa, operands[0]);
                        vtys.push(v);
                    },

                    // Unary operand that should copy current nodes's status
                    &MOpcode::OpNarrow(u16) |
                    &MOpcode::OpWiden(u16)  => {
                        let v = self.resolve_and_mark_subtree(ssa, operands[0]);
                        vtys.push(v);
                    },

                    // Special cased to do a section lookup
                    &MOpcode::OpConst(u64)  => {
                        // TODO. XXX.
                        vtys.push(ValueType::Scalar);
                    },

                    // Result can never be ref
                    &MOpcode::OpDiv |
                    &MOpcode::OpMod |
                    &MOpcode::OpNot |
                    &MOpcode::OpGt |
                    &MOpcode::OpLt => {
                        for op in &operands {
                            self.mark_subtree_as_scalar(ssa, op);
                        }
                        vtys.push(ValueType::Scalar);
                    },
                }

                // Check if exactly one subtree resolved to be a reference.
                // TODO.
                self.compute_result_arr(&vtys)
            },
        }
    }

    fn mark_subtree_as_scalar(&self, ssa: &mut SSAStorage, subtree: &NodeIndex) {
        for op in ssa.get_operands(subtree) {
            if let Some(ref mut nd) = ssa.node_weight_mut(op) {
                if let Some(ref mut vi) = nd.get_valueinfo_mut() {
                    vi.mark_as_scalar();
                }
            }
            self.mark_subtree_as_scalar(ssa, op);
        }
    }

    pub fn resolve_refs(ssa: &mut SSAStorage) {
        let mut wl: VecDeque<NodeIndex> =  VecDeque::new();
        let seen: HashSet<NodeIndex> = HashSet::new();

        // Start from sources where this information can originate.
        // For marking nodes as references, the source of this information
        // has to be one of:
        //   - Address to read/write (memory operations)
        //   - Target of a indirect CF-transfer (call/jump).
        //   - Arguments to current function
        //   - Arguments to  call
        //   - Return from functions

        while !wl.is_empty() {
            let current = wl.pop_front().expect("Cannot be `None`");
            let nd  = ssa.g[current].clone();

            match nd {
                NodeData::Op(ref opc, ref vi) => {
                    match opc {
                        &MOpcode::OpStore | &MOpcode::OpLoad => {
                            let addr = ssa.get_operands(&current)[1];
                            if let Some(ref mut ind) = ssa.g.node_weight_mut(addr) {
                                if let Some(ref mut vi) = ind.get_valueinfo_mut() {
                                    vi.mark_as_reference();
                                }
                            }
                            // Push in the next use of memory, if this is a store.
                            if opc == &MOpcode::OpStore {
                                let uses = ssa.uses_of(current);
                                wl.extend(&uses);
                            }
                            wl.push_back(addr);
                        }

                        &MOpcode::OpCall => unimplemented!(),


                        _ => continue,
                    }
                }
                NodeData::Phi(ref vi, ref c) => unimplemented!(),
                NodeData::Comment(ref vi, ref c) => unimplemented!(),
                _ => continue,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mark_refs_1() {
    }
}
