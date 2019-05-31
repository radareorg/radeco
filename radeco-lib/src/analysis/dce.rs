// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Dead code elimination
//!
//! Removes SSA nodes that are not used by any other node.
//! The algorithm will not consider whether the uses keeping a node alive
//! are in code that is actually executed or not. For a better analysis
//! look at `analysis::constant_propagation`.

use crate::analysis::analyzer::{
    Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer, RemoveValue,
};
use crate::frontend::radeco_containers::RadecoFunction;
use crate::middle::ssa::cfg_traits::{CFGMod, CFG};
use crate::middle::ssa::graph_traits::Graph;
use crate::middle::ssa::ssa_traits::{NodeType, SSAExtra, SSAMod, SSA};
use crate::middle::ssa::ssastorage::SSAStorage;

use std::any::Any;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct DCE {}

const NAME: &str = "dce";
const REQUIRES: &[AnalyzerKind] = &[];

pub const INFO: AnalyzerInfo = AnalyzerInfo {
    name: NAME,
    kind: AnalyzerKind::DCE,
    requires: REQUIRES,
    uses_policy: true,
};

impl DCE {
    pub fn new() -> Self {
        DCE {}
    }

    // Marks node for removal. This method does not remove nodes.
    fn mark(&self, ssa: &mut SSAStorage) {
        let nodes = ssa.values();
        let roots = registers_in_err!(ssa, exit_node_err!(ssa), ssa.invalid_value().unwrap());
        let mut queue = VecDeque::<<SSAStorage as SSA>::ValueRef>::new();
        for node in &nodes {
            if let Ok(ref result) = ssa.node_data(*node) {
                if let NodeType::Op(ref op) = result.nt {
                    if op.has_sideeffects() || ssa.is_selector(*node) {
                        queue.push_back(*node);
                    }
                }
            } else {
                ssa.mark(node);
            }
        }
        ssa.clear_mark(&roots);
        queue.extend(&[roots]);
        while let Some(ni) = queue.pop_front() {
            if ssa.is_marked(&ni) {
                continue;
            }
            ssa.mark(&ni);
            queue.extend(ssa.operands_of(ni));
        }
    }

    // Sweeps away the un-marked nodes
    fn sweep<T: FnMut(Box<dyn Change>) -> Action>(&self, ssa: &mut SSAStorage, mut policy: T) {
        for node in &ssa.values() {
            if !ssa.is_marked(node) {
                match policy(Box::new(RemoveValue(*node))) {
                    Action::Apply => {
                        ssa.remove_value(*node);
                    }
                    Action::Skip => (),
                    Action::Abort => {
                        return;
                    }
                };
            }
            ssa.clear_mark(node);
        }

        // Remove empty blocks and redirect control flows.
        let blocks = ssa.blocks();
        for block in &blocks {
            // Do not touch start or exit nodes
            if *block == entry_node_err!(ssa) || *block == exit_node_err!(ssa) {
                continue;
            }

            let remove_block = if ssa.exprs_in(*block).is_empty() && ssa.phis_in(*block).is_empty()
            {
                let incoming = ssa.incoming_edges(*block);
                let outgoing = ssa.outgoing_edges(*block);
                // Two cases.
                if outgoing.len() == 1 {
                    let new_target = ssa
                        .edge_info(outgoing[0].0)
                        .expect("Less-endpoints edge")
                        .target;
                    for &(ie, ref i) in &incoming {
                        let new_src = ssa.edge_info(ie).expect("Less-endpoints edge").source;
                        ssa.remove_control_edge(ie);
                        ssa.insert_control_edge(new_src, new_target, *i);
                    }
                    ssa.remove_control_edge(outgoing[0].0);
                    true
                } else {
                    // Incoming edge has to be an unconditional,
                    // else we cannot re-route.
                    // TODO: This is currently unimplemented!
                    false
                }
            } else {
                false
            };
            if remove_block {
                radeco_trace!("dce_rm_empty|{:?}", block);
                ssa.remove_block(*block);
            }
        }
    }
}

impl Analyzer for DCE {
    fn info(&self) -> &'static AnalyzerInfo {
        &INFO
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl FuncAnalyzer for DCE {
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        rfn: &mut RadecoFunction,
        policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>> {
        self.mark(rfn.ssa_mut());
        self.sweep(
            rfn.ssa_mut(),
            policy.expect("A policy function must be provided"),
        );

        None
    }
}
