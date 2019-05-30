// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements the `GraphDot` trait for `SSAStorage`

use petgraph::graph;
use petgraph::graph::NodeIndex;
use petgraph::visit::{EdgeRef, IntoEdgeReferences};

use super::ssa_traits::{SSAExtra, SSA};
use super::ssastorage::{EdgeData, NodeData, SSAStorage};
use crate::middle::dot::{DotAttrBlock, GraphDot};
use crate::middle::ir::MOpcode;
use crate::middle::ssa::cfg_traits::CFG;

///////////////////////////////////////////////////////////////////////////////
//// Implementation of GraphDot to emit Dot for SSAStorage.
///////////////////////////////////////////////////////////////////////////////

impl GraphDot for SSAStorage {
    type NodeIndex = graph::NodeIndex;
    type EdgeIndex = graph::EdgeIndex;

    fn configure(&self) -> String {
        "digraph cfg {\nsplines=\"ortho\"\nranksep=1\nnodesep=1;\ngraph [fontsize=12 \
         fontname=\"Verdana\" compound=true rankdir=TB;]\n"
            .to_owned()
    }

    fn nodes(&self) -> Vec<Self::NodeIndex> {
        self.g.node_indices().collect()
    }

    fn edges(&self) -> Vec<Self::EdgeIndex> {
        self.g.edge_references().map(|x| x.id()).collect()
    }

    fn node_count(&self) -> usize {
        self.g.node_count()
    }

    fn edge_count(&self) -> usize {
        self.g.edge_count()
    }

    fn node_index_new(i: usize) -> Self::NodeIndex {
        graph::NodeIndex::new(i)
    }

    fn edge_index_new(i: usize) -> Self::EdgeIndex {
        graph::EdgeIndex::new(i)
    }

    fn node_cluster(&self, i: &Self::NodeIndex) -> Option<usize> {
        let invalid_block = self
            .invalid_action()
            .expect("Invalid Action is not defined");
        match self.g.node_weight(*i) {
            Some(&NodeData::BasicBlock(_, _)) | Some(&NodeData::DynamicAction) => Some(i.index()),
            _ => Some(self.block_for(*i).unwrap_or(invalid_block).index()),
        }
    }

    fn edge_source(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
        let edge_opt = &self.g.edge_references().find(|x| x.id() == *i);
        if edge_opt.is_none() {
            radeco_err!("Invalid edge index");
            return NodeIndex::end();
        };
        let edge = edge_opt.unwrap();
        match *edge.weight() {
            EdgeData::Data(_) => edge.target(),
            _ => edge.source(),
        }
    }

    fn edge_target(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
        let edge_opt = &self.g.edge_references().find(|x| x.id() == *i);
        if edge_opt.is_none() {
            radeco_err!("Invalid edge index");
            return NodeIndex::end();
        };
        let edge = edge_opt.unwrap();
        match *edge.weight() {
            EdgeData::Data(_) => edge.source(),
            _ => edge.target(),
        }
    }

    fn edge_skip(&self, i: &Self::EdgeIndex) -> bool {
        let edge_opt = &self.g.edge_references().find(|x| x.id() == *i);
        if edge_opt.is_none() {
            radeco_err!("Invalid edge index");
            return false;
        };
        let edge = edge_opt.unwrap();
        match *edge.weight() {
            EdgeData::ContainedInBB(_) | EdgeData::RegisterState => true,
            EdgeData::Data(_) => {
                // Skip data edges from registers to registerstates.
                if let NodeData::RegisterState = self.g[edge.source()] {
                    self.comment(edge.target()).is_some()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    // TODO: Ordering of clusters for ssa is kind of hacky and may not run top to
    // bottom in some
    // cases.
    fn edge_attrs(&self, i: &Self::EdgeIndex) -> DotAttrBlock {
        let edge_opt = &self.g.edge_references().find(|x| x.id() == *i);
        if edge_opt.is_none() {
            radeco_err!("Invalid edge index");
            return DotAttrBlock::Raw(String::new());
        };
        let edge = edge_opt.unwrap();
        let mut prefix = String::new();
        let src = edge.source();
        let target = edge.target();

        prefix.push_str(&format!("n{} -> n{}", src.index(), target.index()));
        let target_is_bb = if let NodeData::BasicBlock(_, _) = self.g[edge.target()] {
            true
        } else {
            false
        };
        let attr = match *edge.weight() {
            EdgeData::Control(_) if !target_is_bb => vec![("color".to_string(), "red".to_string())],
            EdgeData::Control(i) => {
                // Determine the source and destination clusters.
                let source_cluster = edge.source().index();
                let dst_cluster = edge.target().index();
                let (color, label) = match i {
                    0 => ("red", "F"),
                    1 => ("green", "T"),
                    2 => ("blue", "U"),
                    _ => unreachable!(),
                };
                vec![
                    ("color".to_string(), color.to_string()),
                    ("xlabel".to_string(), label.to_string()),
                    ("ltail".to_string(), format!("cluster_{}", source_cluster)),
                    ("lhead".to_string(), format!("cluster_{}", dst_cluster)),
                    ("minlen".to_string(), "9".to_owned()),
                ]
            }
            EdgeData::Data(i) => vec![
                ("dir".to_string(), "back".to_string()),
                ("xlabel".to_string(), format!("{}", i)),
            ],
            EdgeData::ContainedInBB(_) => vec![("color".to_string(), "gray".to_string())],
            EdgeData::Selector => vec![("color".to_string(), "purple".to_string())],
            EdgeData::ReplacedBy => vec![("color".to_string(), "brown".to_string())],
            EdgeData::RegisterInfo => vec![("color".to_string(), "yellow".to_string())],
            EdgeData::RegisterState => unreachable!(),
        };

        DotAttrBlock::Hybrid(prefix, attr)
    }

    fn node_attrs(&self, i: &Self::NodeIndex) -> DotAttrBlock {
        let node = &self.g[*i];
        let mut prefix = String::new();
        prefix.push_str(&format!("n{}", i.index()));

        let attr = match *node {
            NodeData::Op(ref opc, vi) => {
                let w = vi.width().get_width().unwrap_or(64);
                let mut attrs = Vec::new();
                let mut r = String::new();
                let addr = self.addr(i);
                if addr.is_some() {
                    r.push_str(&format!(
                        "<<font color=\"grey50\">{}: </font>",
                        addr.as_ref().unwrap()
                    ))
                }
                r.push_str(&format!("\"[i{}] {:?}\"", w, opc));
                if addr.is_some() {
                    r.push_str(">");
                }

                attrs.push(("style".to_owned(), "filled".to_owned()));
                if let MOpcode::OpConst(c) = *opc {
                    attrs.push(("color".to_owned(), "black".to_owned()));
                    attrs.push(("fillcolor".to_owned(), "yellow".to_owned()));
                    attrs.push(("label".to_string(), format!("\"0x{:x}\"", c)));
                } else {
                    attrs.push(("color".to_owned(), "black".to_owned()));
                    attrs.push(("label".to_string(), r));

                    if self.is_marked(i) {
                        attrs.push(("fillcolor".to_owned(), "green".to_owned()));
                    } else {
                        attrs.push(("fillcolor".to_owned(), "white".to_owned()));
                    }
                }
                attrs
            }
            NodeData::BasicBlock(addr, _) => {
                let label_str = format!(
                    "<<font color=\"grey50\">Basic Block Information<br/>Start Address: {}</font>>",
                    addr
                );
                let mut attrs = Vec::new();
                if let Some(e) = self.entry_node() {
                    if *i == e {
                        attrs.push(("rank".to_string(), "min".to_string()));
                    }
                };

                attrs.extend(
                    [
                        ("style".to_string(), "filled".to_string()),
                        ("fillcolor".to_string(), "white".to_string()),
                        ("label".to_string(), label_str),
                        ("shape".to_string(), "box".to_string()),
                        ("color".to_string(), "\"grey\"".to_string()),
                    ]
                    .iter()
                    .cloned(),
                );
                attrs
            }
            NodeData::Comment(_, ref msg) => vec![
                (
                    "label".to_string(),
                    format!("\"{}\"", msg.replace("\"", "\\\"")),
                ),
                ("shape".to_owned(), "box".to_owned()),
                ("style".to_owned(), "filled".to_owned()),
                ("color".to_owned(), "black".to_owned()),
                ("fillcolor".to_owned(), "greenyellow".to_owned()),
            ],
            NodeData::Phi(_, _) => {
                let mut attrs = Vec::new();
                let mut label = format!("{:?}", node);
                label = format!("\"{}\"", label);
                if let Some(addr) = self.addr(i) {
                    label = format!("<<font color=\"black\">{}: </font> {}>", addr, label);
                }
                attrs.push(("label".to_string(), label));
                attrs.push(("style".to_owned(), "filled".to_owned()));
                attrs.push(("color".to_owned(), "black".to_owned()));
                if self.is_marked(i) {
                    attrs.push(("fillcolor".to_owned(), "green".to_owned()));
                } else {
                    attrs.push(("fillcolor".to_owned(), "orange".to_owned()));
                }
                attrs
            }
            _ => {
                let mut attrs = Vec::new();
                let mut label = format!("{:?}", node);
                label = label.replace("\"", "\\\"");
                label = format!("\"{}\"", label);
                if let Some(addr) = self.addr(i) {
                    label = format!("<<font color=\"grey50\">{}: </font> {}>", addr, label);
                }
                attrs.push(("label".to_string(), label));
                attrs.push(("style".to_owned(), "filled".to_owned()));
                attrs.push(("color".to_owned(), "black".to_owned()));
                if self.is_marked(i) {
                    attrs.push(("fillcolor".to_owned(), "green".to_owned()));
                } else {
                    attrs.push(("fillcolor".to_owned(), "grey90".to_owned()));
                }
                attrs
            }
        };
        DotAttrBlock::Hybrid(prefix, attr)
    }
}
