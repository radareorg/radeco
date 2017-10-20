//! Implements some low-level analysis as a part of frontend

use frontend::radeco_containers::{RadecoModule, CallGraph};
use r2api::structs::FunctionInfo;
use std::collections::HashMap;

/// Converts call graph information from `Source`, represented in FunctionInfo,
/// into an actual graph with links.
pub fn load_call_graph(finfos: &[FunctionInfo]) -> CallGraph {
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

                let csite = cs.source.unwrap();
                let target = node_map.get(&cs.target.unwrap());

                match (fnode, target) {
                    (Some(cn), Some(tn)) => {
                        cg.add_edge(*cn, *tn, csite);
                    }
                    (_, _) => {}
                }
            }
        }
    }

    cg
}
