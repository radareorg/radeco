//! Data flow analysis to mark ssa nodes as references or scalars.
//!
//! This analysis makes some assumptions about how references are forged/used:
//!   1. Every subtree has exactly one leaf that is a reference,
//!   that is, two references are never used in an operation (such as add, sub etc.)
//!   Indeed, there is nothing that prevents someone from code that does this, but is generally
//!   not an accepted practice.
//!
//!   2. References are not used in operations such as mul, left shift, etc.


use frontend::radeco_containers::RadecoFunction;
use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{ValueInfo, ValueType, SSA, SSAWalk};

use analysis::constraint_set::{ConstraintSet, Constraint};

use middle::ssa::ssastorage::{NodeData, SSAStorage};
use petgraph::graph::NodeIndex;

use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;

#[derive(Default, Debug)]
pub struct ReferenceMarker {
    cs: Option<ConstraintSet<NodeIndex>>,
}

impl ReferenceMarker {
    fn compute_result(&self, op: &[ValueType]) -> ValueType {
        match (op[0], op[1]) {
            (ValueType::Invalid, _) |
            (_, ValueType::Invalid) |
            (ValueType::Reference, ValueType::Reference) => ValueType::Invalid,
            (ValueType::Reference, _) |
            (_, ValueType::Reference) => ValueType::Reference,
            (ValueType::Scalar, ValueType::Scalar) => ValueType::Scalar,
            (_, _) => ValueType::Unresolved,
        }
    }

    fn mark_node(&self, ssa: &mut SSAStorage, ni: NodeIndex, ty: ValueType) {

    }

    // 
    // Start from sources where this information can originate.
    // For marking nodes as references, the source of this information
    // has to be one of:
    //   - Address to read/write (memory operations)
    //   - Target of a indirect CF-transfer (call/jump).
    //   - Access to stack using rsp/rbp
    //   - Arguments to current function
    //   - Arguments to call                               -- Added from inter-function propagation
    //   - Return from functions                           -- Added from inter-function propagation
    //
    // Return the number of bindings to create
    fn add_constraints(&mut self, ssa: &SSAStorage) -> HashSet<NodeIndex> {
        for idx in ssa.inorder_walk() {
            println!("{:?}", idx);
        }
        HashSet::new()
    }

    pub fn resolve_refs(&mut self, rfn: &mut RadecoFunction) {
        // Only the first time. Consecutive calls will try to solve the remaining unsolved
        // constraints rather than adding in new ones.
        if self.cs.is_none() {
            self.cs = Some(ConstraintSet::default());
            let bn = self.add_constraints(rfn.ssa());
            self.cs.as_mut().unwrap().bind(bn.into_iter().collect::<Vec<_>>().as_slice());
        }

        let cs = self.cs.as_mut().unwrap();
        cs.solve();

        for (ni, vt) in cs.iter_bindings() {
            let ssa = rfn.ssa_mut();
            if let Some(ref mut nd) = ssa.g.node_weight_mut(*ni) {
                if let Some(vi) = nd.valueinfo_mut() {
                    vi.set_value_type(*vt);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mark_refs_1() {}
}
