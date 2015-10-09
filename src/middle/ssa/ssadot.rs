// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements the `GraphDot` trait for SSAStorage

use petgraph::graph;

use middle::dot::{GraphDot, DotAttrBlock};
use super::ssastorage::{NodeData, EdgeData, SSAStorage};
use super::ssa_traits::{SSA, SSAMod, SSAExtra, ValueType, BBInfo};

///////////////////////////////////////////////////////////////////////////////
//// Implementation of GraphDot to emit Dot for SSAStorage.
///////////////////////////////////////////////////////////////////////////////

impl GraphDot for SSAStorage {
	type NodeIndex = graph::NodeIndex;
	type EdgeIndex = graph::EdgeIndex;

	fn configure(&self) -> String {
		format!("digraph cfg {{\nsplines=\"true\";\ngraph [fontsize=12 fontname=\"Verdana\" compound=true rankdir=TB;]\n")
	}

	//fn nodes(&self) -> Vec<Self::NodeType> {
		//let res = self.g.raw_nodes().iter().map(|e| e.weight.clone()).collect();
		//res
	//}

	//fn edges(&self) -> Vec<Self::EdgeType> {
		//let res = self.g.raw_edges().to_vec();
		//res
	//}

	//fn get_node(&self, n: usize) -> Option<&Self::NodeType> {
		//self.g.node_weight(NodeIndex::new(n))
	//}
	
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
		match self.g.node_weight(*i) {
			Some(&NodeData::BasicBlock(_)) => Some(i.index()),
			Some(&NodeData::DynamicAction) => Some(i.index()),
			_ => Some(self.get_block(i).index()),
		}
	}

	fn edge_source(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
		let edge = &self.g.raw_edges()[i.index()];
		match edge.weight {
			EdgeData::Data(_) => edge.target(),
			_                 => edge.source(),
		}
	}

	fn edge_target(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
		let edge = &self.g.raw_edges()[i.index()];
		match edge.weight {
			EdgeData::Data(_) => edge.source(),
			_                 => edge.target(),
		}
	}

	fn edge_skip(&self, i: &Self::EdgeIndex) -> bool {
		let edge = &self.g.raw_edges()[i.index()];
		match edge.weight {
			EdgeData::ContainedInBB => true,
			EdgeData::RegisterState => true,
			_ => false,
		}
	}

	// TODO: Ordering of clusters for ssa is kind of hacky and may not run top to bottom in some
	// cases.
	fn edge_attrs(&self, i: &Self::EdgeIndex) -> DotAttrBlock {
		// TODO: Error Handling
		let edge = &self.g.raw_edges()[i.index()];
		let mut prefix = String::new();
		let src = edge.source().index();
		let target = edge.target().index();

		prefix.push_str(&format!("n{} -> n{}", src, target));
		let target_is_bb = if let NodeData::BasicBlock(_) = self.g[edge.target()] { true } else { false };
		let attr = match edge.weight {
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
		};

		DotAttrBlock::Hybrid(prefix, attr)
	}

	fn node_attrs(&self, i: &Self::NodeIndex) -> DotAttrBlock {
		let node = &self.g[*i];
		let mut prefix = String::new();
		prefix.push_str(&format!("n{}", i.index()));
		let attr = match *node {
			NodeData::Op(opc, ValueType::Integer{width: w}) => {
				// TODO
				//self.extras_dump();
				let addr = self.addr(i);
				let mut r = String::new();
				if addr.is_some() {
					r.push_str(&format!("<<font color=\"grey50\">0x{}: </font>"
										                     , addr.as_ref().unwrap()))
				}
				r.push_str(&format!("\"[i{}] {:?}\"", w, opc));
				if addr.is_some() {
					r.push_str(">");
				}
				vec![("label".to_string(), r)]
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
		DotAttrBlock::Hybrid(prefix, attr)
	}
}
