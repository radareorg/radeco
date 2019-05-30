//! Intra-function data flow analysis to propagate reference information within a function.
//! This is useful to distinguish between pointer (reference) and non-pointer (scalar) types.
//!
//! This analysis makes some assumptions about how references are forged/used:
//!
//!   1. Every subtree has exactly one leaf that is a reference,
//!      that is, two references are never used in an operation (such as add, sub etc.)
//!      Indeed, there is nothing that prevents someone from code that does this, but is generally
//!      not an accepted practice.
//!
//!   2. References are not used in operations such as mul, left shift, etc.
//!
//! We treat the following as sources of information:
//!
//!   - Address to read/write (memory operations)
//!   - Target of a indirect CF-transfer (call/jump).
//!   - Access to stack using rsp/rbp
//!   - Arguments to current function                   -- Implicitly get marked as reference
//!   - Arguments to call                               -- Added from inter-function propagation
//!   - Return from functions                           -- Added from inter-function propagation
//!     + Returns from 'well-known' functions that return references, such as malloc.

use crate::analysis::constraint_set::ConstraintSet;
use crate::frontend::radeco_containers::RadecoFunction;
use crate::middle::ir::MOpcode;
use crate::middle::regfile::SubRegisterFile;
use crate::middle::ssa::ssa_traits::{NodeType, SSAWalk, ValueType, SSA};
use crate::middle::ssa::ssastorage::SSAStorage;
use petgraph::graph::NodeIndex;
use r2api::structs::LSectionInfo;
use std::collections::HashSet;
use std::sync::Arc;

#[derive(Debug)]
pub struct ReferenceMarker {
    pub cs: ConstraintSet<NodeIndex>,
    regfile: Arc<SubRegisterFile>,
    sections: Arc<Vec<LSectionInfo>>,
}

impl ReferenceMarker {
    fn is_constant_scalar(&self, val: u64) -> bool {
        !self.sections.iter().any(|section| {
            let base = section.vaddr.unwrap();
            let size = section.vsize.unwrap();
            // XXX: Hack!
            if base > 0 && base <= val && val < base + size {
                true
            } else {
                false
            }
        })
    }

    fn add_constraints(&mut self, ssa: &SSAStorage) {
        let mut comment_nodes = HashSet::new();
        for idx in ssa.inorder_walk() {
            let nd = ssa.node_data(idx);
            if nd.is_err() {
                continue;
            }
            let nd = nd.unwrap();
            // TODO: Selector to an indirect CF transfer
            match nd.nt {
                NodeType::Op(ref opc) => {
                    // Generate constraints based on the type of opcode
                    match opc {
                        &MOpcode::OpAdd
                        | &MOpcode::OpGt
                        | &MOpcode::OpLt
                        | &MOpcode::OpNot
                        | &MOpcode::OpOr
                        | &MOpcode::OpNarrow(_)
                        | &MOpcode::OpSignExt(_)
                        | &MOpcode::OpZeroExt(_)
                        | &MOpcode::OpAnd
                        | &MOpcode::OpDiv
                        | &MOpcode::OpLsl
                        | &MOpcode::OpLsr
                        | &MOpcode::OpMod
                        | &MOpcode::OpMul
                        | &MOpcode::OpRol
                        | &MOpcode::OpRor
                        | &MOpcode::OpSub
                        | &MOpcode::OpXor => {
                            // All operands are allowed to be references
                            let operands = ssa.operands_of(idx);
                            // Setup a union constraint
                            self.cs.add_union(idx, operands.as_slice());
                            // Check if they of the operands are comments
                            comment_nodes.extend(operands.iter().filter(|&&x| ssa.is_comment(x)));
                            for (cidx, cval) in operands
                                .iter()
                                .filter_map(|&x| ssa.constant_value(x).map(|v| (x, v)))
                            {
                                if self.is_constant_scalar(cval) {
                                    self.cs.add_eq(cidx, ValueType::Scalar);
                                }
                            }
                        }
                        &MOpcode::OpConst(val) => {
                            // Special case handling for const opcodes
                            // Need to check for a couple of things here. These are mostly
                            // heuristics.
                            //  - Check if it is a valid address, i.e. the constant points to
                            //  one of the valid sections in the binary.
                            //  - If it does not, it certainly is not a reference.
                            //  XXX: Can be a bit more efficient/intelligent here
                            if self.is_constant_scalar(val) {
                                self.cs.add_eq(idx, ValueType::Scalar);
                            }
                        }
                        &MOpcode::OpCall => {
                            let operands = ssa.operands_of(idx);
                            if !operands.is_empty() {
                                self.cs.add_eq(operands[0], ValueType::Reference);
                            }
                        }
                        //&MOpcode::OpAnd | &MOpcode::OpDiv | &MOpcode::OpLsl | &MOpcode::OpLsr |
                        //&MOpcode::OpMod | &MOpcode::OpMul | &MOpcode::OpRol | &MOpcode::OpRor |
                        //&MOpcode::OpSub => {
                        //// op2 is not allowed to be a reference
                        //let operands = ssa.operands_of(idx);
                        //self.cs.add_union(idx, operands.as_slice());
                        //comment_nodes.extend(operands.iter().filter(|&&x| ssa.is_comment(x)));
                        //// Add additional constraint that the second operands is a scalar
                        //if let Some(op2) = operands.get(1) {
                        //self.cs.add_eq(*op2, ValueType::Scalar);
                        //}
                        //}
                        &MOpcode::OpLoad | &MOpcode::OpStore => {
                            // Special case for load/store
                            let operands = ssa.operands_of(idx);
                            // Operand 0 is "mem", this is not a reference
                            if let Some(op0) = operands.get(0) {
                                self.cs.add_eq(*op0, ValueType::Scalar);
                            }
                            // Operand 1 is a reference
                            if let Some(op1) = operands.get(1) {
                                self.cs.add_eq(*op1, ValueType::Reference);
                            }
                            // In case of store, nothing can be said about operand 2.
                            // However, opstore returns a new instance of memory. This can be
                            // inferred to not be a reference when it is used in a memory operation
                            // later on.
                            // In case of load, nothing can be said about the returned value.
                        }
                        &MOpcode::OpCustom(_) | &MOpcode::OpInvalid | &MOpcode::OpNop => {
                            // Can't say anything about these operands
                            let operands = ssa.operands_of(idx);
                            self.cs.add_union(idx, operands.as_slice());
                            comment_nodes.extend(operands.iter().filter(|&&x| ssa.is_comment(x)));
                        }
                        _ => {
                            // Nothing to do
                        }
                    }
                }
                NodeType::Phi => {
                    // Generate a union constraint over all operands of phi-node
                    // same type.
                    let mut operands = ssa.operands_of(idx);
                    // The phi node should also be equal to all its operands.
                    operands.push(idx);
                    self.cs.add_equivalence_assertion(operands.as_slice());
                    comment_nodes.extend(operands.iter().filter(|&&x| ssa.is_comment(x)));
                }
                _ => {
                    // This is unreachable!
                    unreachable!()
                }
            }
        }

        for idx in comment_nodes {
            let nd = ssa.node_data(idx);
            match nd.unwrap().nt {
                NodeType::Comment(ref comm) => {
                    // Generate equality constraint if the comment is the stack pointer
                    if let Some(sp_reg) = self.regfile.alias_info.get("SP") {
                        if comm == sp_reg {
                            self.cs.add_eq(idx, ValueType::Reference);
                        }
                    }
                    // If it is `rip`, then it can't be a reference
                    if let Some(ip_reg) = self.regfile.alias_info.get("PC") {
                        if comm == ip_reg {
                            self.cs.add_eq(idx, ValueType::Scalar);
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn resolve_references_iterative(&mut self, rfn: &mut RadecoFunction) -> bool {
        let progress = self.cs.solve();
        for (ni, vt) in self.cs.iter_bindings() {
            let ssa = rfn.ssa_mut();
            if let Some(ref mut nd) = ssa.g.node_weight_mut(*ni) {
                if let Some(vi) = nd.valueinfo_mut() {
                    vi.set_value_type(*vt);
                }
            }
        }

        // fixpoint = !progress
        !progress
    }

    // Used for calling to resolve references the first time. Future calls should call
    // `resolve_references_iterative`.
    pub fn resolve_references(
        rfn: &mut RadecoFunction,
        regfile: Arc<SubRegisterFile>,
        sections: Arc<Vec<LSectionInfo>>,
    ) -> ReferenceMarker {
        let mut refmarker = ReferenceMarker {
            regfile: regfile,
            sections: sections,
            cs: ConstraintSet::default(),
        };
        let _bn = refmarker.add_constraints(rfn.ssa());
        refmarker.resolve_references_iterative(rfn);
        refmarker
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn mark_refs_1() {}
}
