//! Implements some low-level analysis as a part of frontend

use frontend::radeco_containers::{RadecoModule, CallGraph, CGInfo, CallContextInfo, RadecoFunction};
use middle::ssa::ssa_traits::{SSAWalk, SSA, NodeType};
use middle::regfile::SubRegisterFile;
use middle::ir::MOpcode;
use r2api::structs::FunctionInfo;
use std::collections::HashMap;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Direction;

/// Converts call graph information from `Source`, represented in FunctionInfo,
/// into an actual graph with links.
pub fn load_call_graph(finfos: &[FunctionInfo], rmod: &RadecoModule) -> CallGraph {
    let mut cg = CallGraph::new();
    let node_map = finfos.iter()
        .map(|x| {
            let offset = x.offset.unwrap();
            (offset, cg.add_node(offset))
        })
        .collect::<HashMap<_, _>>();

    for x in finfos {
        let offset = x.offset.unwrap();
        let fnode = node_map.get(&offset);
        if let Some(ref callrefs) = x.callrefs {
            for cs in callrefs {
                match cs.call_type {
                    Some(ref c) if c != "C" => continue,
                    _ => {}
                }
                let mut cctx = CallContextInfo::default();
                cctx.csite = cs.source.expect("No source for call");
                let target = node_map.get(&cs.target.unwrap());
                match (fnode, target) {
                    (Some(cn), Some(tn)) => {
                        cg.add_edge(*cn, *tn, cctx);
                    }
                    (_, _) => {}
                }
            }
        }
    }

    // Initial callsite analysis to fix up CallContextInfo on the edges.
    for (offset, node) in &node_map {
        if let Some(rfn) = rmod.functions.get(offset) {
            let mut csites = analyze_callsite_initial(rfn);
            // Get callees of the node to update the cctx information that we just discovered
            let mut edges = cg.neighbors_directed(*node, Direction::Outgoing).detach();
            while let Some(call_edge) = edges.next_edge(&cg) {
                if let Some(cctx) = cg.edge_weight_mut(call_edge) {
                    if let Some(new_cctx) = csites.remove(&cctx.csite) {
                        *cctx = new_cctx;
                    }
                }
            }
        }
    }

    cg
}

/// Iterates through nodes in SSA for rfn and initializes the inital CallContextInfo
fn analyze_callsite_initial(rfn: &RadecoFunction) -> HashMap<u64, CallContextInfo> {
    let mut cctxs = HashMap::new();
    let ssa = rfn.ssa();
    for node in ssa.inorder_walk() {
        match ssa.node_data(node) {
            Ok(ref nd) => { 
                if let NodeType::Op(MOpcode::OpCall) = nd.nt {
                    let offset = ssa.address(node).expect("").address;
                    let mut cctx = CallContextInfo::default();
                    let args_to_call = ssa.operands_of(node);
                    // Map operands/arguments to call in the caller context to invalid nodes in
                    // the callee context. These will be resolved later (hopefully)
                    // to be the argument nodes in the callee.
                    cctx.map = args_to_call.iter().map(|&x| (x, NodeIndex::end())).collect();
                    cctx.csite_node = node;
                    cctx.csite = offset;
                    cctxs.insert(offset, cctx);
                }
            },
            _ => { /* Noop, we only analyze callsites */ }
        }
    }
    cctxs
}

pub fn init_call_ctx(rmod: &mut RadecoModule) {
    let cctxs: Vec<(u64, CallContextInfo)> = Vec::new();
    for wrapper in rmod.functions.iter() {
        let rfn = wrapper.1;
        let mut csites: HashMap<u64, CallContextInfo> = analyze_callsite_initial(rfn);
        // Iterate through callsites
        for (csite, callee) in rmod.callgraph.callees(rfn.cgid()) {
            // Get args of callee
            if let Some(calleefn) = rmod.functions.get(&rmod.callgraph[callee]) {
                let mut args = calleefn.bindings().into_iter().filter(|x| x.btype.is_argument());
                // Access the actual callsite in rfn.
                if let Some(cctx) = csites.get_mut(&csite) {
                    cctx.map = cctx.map
                        .iter()
                        .map(|&(k, v)| k)
                        .zip(args.into_iter().map(|v| v.idx))
                        .collect();
                }
            }
        }
        // TODO: Save the csites information in the callgraph
    }
}
