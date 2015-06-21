use std::io::{Cursor, Write};

use super::super::traits::{InstructionType, Navigation, NavigationInternal};
use super::indextype::IndexType;
use super::{DefaultInnerIndex, IRGraph, IRNode, IREdge};
use super::petgraph::graph::{DefIndex, NodeIndex};

pub fn dot<Index: IndexType, Instruction: InstructionType>(target: &mut Write, graph: &IRGraph<Index, Instruction>) {
	// find something more idiomatic than .as_bytes()
	target.write("digraph G {\n".as_bytes());
	target.write("  compound=true;\n".as_bytes());
	let node_count = graph.node_count();
	for i in 0..node_count {
		let node = &graph[NodeIndex::new(i)];
		match node {
			&IRNode::BasicBlock(ref bb) => {
				target.write(format!("  subgraph cluster{} {{\n", i).as_bytes());
				for (j, instr) in bb.inner_graph.iter().enumerate() {
					// TODO properly escape
					target.write(format!("    {}_{} [label=\"{:?}\"];\n", i, j, instr).as_bytes());
				}
				target.write("  }\n".as_bytes());
			}
			_ => {}
		}
	}
	target.write("}\n".as_bytes());
}
