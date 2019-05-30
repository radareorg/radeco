//! A few utility functions for working with an [`SSAStorage`].

use crate::middle::regfile::{RegisterId, RegisterMap};
use crate::middle::ssa::ssa_traits::ValueInfo;
use crate::middle::ssa::ssastorage::{EdgeData, SSAStorage};

use petgraph::prelude::*;

/// Structured information about a call.
pub struct CallInfo {
    /// Call target.
    pub target: NodeIndex,
    /// Value of every register that is passed as an argument.
    pub register_args: RegisterMap<NodeIndex>,
}

/// Extracts the call target and the value of all argument registers.
/// Returns `None` if the call doesn't have a target operand.
pub fn call_info(call_node: NodeIndex, ssa: &SSAStorage) -> Option<CallInfo> {
    let mut tgt_opt = None;
    let mut register_args = ssa.regfile.new_register_map();

    for edge_ref in ssa.g.edges_directed(call_node, Outgoing) {
        if let &EdgeData::Data(op_idx) = edge_ref.weight() {
            if op_idx == 0 {
                tgt_opt = Some(edge_ref.target());
            } else {
                register_args.insert(RegisterId::from_u8(op_idx - 1), edge_ref.target());
            }
        }
    }
    tgt_opt.map(|target| CallInfo {
        target,
        register_args,
    })
}

/// Extracts the values of all registers modified by a call.
pub fn call_rets(call_node: NodeIndex, ssa: &SSAStorage) -> RegisterMap<(NodeIndex, ValueInfo)> {
    let mut ret = ssa.regfile.new_register_map();
    for edge_ref in ssa.g.edges_directed(call_node, Incoming) {
        if let (&EdgeData::Data(idx), Some(&vt)) =
            (edge_ref.weight(), ssa.g[edge_ref.source()].valueinfo())
        {
            ret.insert(RegisterId::from_u8(idx), (edge_ref.source(), vt));
        }
    }
    ret
}

/// Extracts the value of all registers at a `RegisterState` SSA node.
pub fn register_state_info(
    regstate_node: NodeIndex,
    ssa: &SSAStorage,
) -> RegisterMap<(NodeIndex, ValueInfo)> {
    let mut ret = ssa.regfile.new_register_map();
    for edge_ref in ssa.g.edges_directed(regstate_node, Outgoing) {
        if let (&EdgeData::Data(op_idx), Some(&vt)) =
            (edge_ref.weight(), ssa.g[edge_ref.target()].valueinfo())
        {
            ret.insert(RegisterId::from_u8(op_idx), (edge_ref.target(), vt));
        }
    }
    ret
}
