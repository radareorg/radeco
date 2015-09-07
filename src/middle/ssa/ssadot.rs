// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements the `GraphDot` trait for SSAStorage

use petgraph::graph::{Edge, NodeIndex};

use middle::dot::{GraphDot, DotAttrBlock};
use super::ssastorage::{NodeData, EdgeData, SSAStorage};
use super::ssa_traits::{SSA, SSAMod, ValueType, BBInfo};

///////////////////////////////////////////////////////////////////////////////
//// Implementation of GraphDot to emit Dot for SSAStorage.
///////////////////////////////////////////////////////////////////////////////

impl GraphDot for SSAStorage {
	type NodeType = NodeData;
	type EdgeType = Edge<EdgeData>;

	fn configure(&self) -> String {
		format!("digraph cfg {{\nsplines=\"true\";\ngraph [fontsize=12 fontname=\"Verdana\" compound=true rankdir=TB;]\n")
	}

	fn nodes(&self) -> Vec<Self::NodeType> {
		let res = self.g.raw_nodes().iter().map(|e| e.weight.clone()).collect();
		res
	}

	fn edges(&self) -> Vec<Self::EdgeType> {
		let res = self.g.raw_edges().to_vec();
		res
	}

	fn get_node(&self, n: usize) -> Option<&Self::NodeType> {
		self.g.node_weight(NodeIndex::new(n))
	}

	fn node_cluster(&self, n: usize) -> usize {
		let ni = NodeIndex::new(n);
		match self.g.node_weight(ni) {
			Some(&NodeData::BasicBlock(_)) => n,
			Some(&NodeData::DynamicAction) => n,
			_ => self.get_block(&ni).index()
		}
	}

	fn edge_source(&self, edge: &Edge<EdgeData>) -> usize {
		match edge.weight {
			EdgeData::Data(_) => edge.target().index(),
			_                 => edge.source().index()
		}
	}

	fn edge_target(&self, edge: &Edge<EdgeData>) -> usize {
		match edge.weight {
			EdgeData::Data(_) => edge.source().index(),
			_                 => edge.target().index()
		}
	}

	fn edge_skip(&self, edge: &Edge<EdgeData>) -> bool {
		match edge.weight {
			EdgeData::ContainedInBB => true,
			EdgeData::RegisterState => true,
			_ => false,
		}
	}

	// TODO: Ordering of clusters for ssa is kind of hacky and may not run top to bottom in some
	// cases.
	fn edge_attrs(&self, edge: &Edge<EdgeData>) -> DotAttrBlock {
		let target_is_bb = if let NodeData::BasicBlock(_) = self.g[edge.target()] { true } else { false };
		DotAttrBlock::Attributes(
			match edge.weight {
				EdgeData::Control(_) if !target_is_bb => {
					vec![("color".to_string(), "red".to_string())]
				},
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
					vec![("color".to_string(), color.to_string()),
					("label".to_string(), format!("{}", label.to_string())),
					("ltail".to_string(), format!("cluster_{}",source_cluster)),
					("lhead".to_string(), format!("cluster_{}",dst_cluster)),
					("minlen".to_string(), format!("9"))]
				},
				EdgeData::Data(i) => {
					vec![("dir".to_string(), "back".to_string()),
					("label".to_string(), format!("{}", i))]
				},
				EdgeData::ContainedInBB => {
					vec![("color".to_string(), "gray".to_string())]
				},
				EdgeData::Selector => {
					vec![("color".to_string(), "purple".to_string())]
				},
				EdgeData::ReplacedBy => {
					vec![("color".to_string(), "brown".to_string())]
				},
				EdgeData::RegisterState => unreachable!(),
			})
	}

	fn node_attrs(&self, node: &NodeData) -> DotAttrBlock {
		let l = match *node {
			NodeData::Op(opc, ValueType::Integer{width: w}) => {
				vec![("label".to_string(), format!("\"[i{}] {:?}\"", w, opc))]
			},
			NodeData::BasicBlock(BBInfo{addr}) => {
				let label_str = format!("<<font color=\"grey50\">Basic Block Information<br/>Start Address: 0x{:X}</font>>", addr);
				vec![("label".to_string(), label_str),
				("shape".to_string(), "box".to_string()),
				("color".to_string(), "\"grey\"".to_string())]
			},
			NodeData::Comment(_, ref msg) => {
				vec![("label".to_string(), format!("\"{}\"", msg.replace("\"", "\\\"")))]
			},
			_ => {
				vec![("label".to_string(), format!("\"{}\"", format!("\"{:?}\"", node).replace("\"", "\\\"")))]
			},
		};
		DotAttrBlock::Attributes(l)
	}
}
