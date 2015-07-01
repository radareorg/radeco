use std::io::{Cursor, Write};

use super::super::traits::{InstructionType, Navigation, NavigationInternal};
use super::indextype::IndexType;
use super::{IRGraph, IRNode, NodeRef};
use super::petgraph::graph::NodeIndex;

pub fn dot<Index: IndexType, Instruction: InstructionType>(graph: &IRGraph<Index, Instruction>) -> Vec<u8> {
	let mut buffer: Vec<u8> = Vec::new();
	// find something more idiomatic than .as_bytes()
	buffer.extend("digraph G {\n".as_bytes());
	buffer.extend("  compound=true;\n".as_bytes());
	let node_count = graph.node_count();
	for i in 0..node_count {
		let node = &graph[NodeIndex::new(i)];
		match node {
			&IRNode::BasicBlock(ref bbr) => {
				let bb = bbr.borrow();
				buffer.extend(format!("  subgraph cluster{} {{\n", i).as_bytes());
				for (j, instr) in bb.inner_graph.iter().enumerate() {
					// TODO properly escape
					buffer.extend(format!("    n{}_{} [label=\"{:?}\"];\n", i, j, instr).as_bytes());
				}
				buffer.extend("  }\n".as_bytes());
			}
			_ => {}
		}
	}
	for i in 0..node_count {
		let node = &graph[NodeIndex::new(i)];
		match node {
			&IRNode::BasicBlock(ref bbr) => {
				let instr_count = bbr.borrow().inner_graph.count();
				for j in 0..instr_count {
					for arg in graph.args_of(NodeRef(NodeIndex::new(i), Index::from_usize(j))) {
						//TODO properly escape
						buffer.extend(format!("  n{}_{} <- n{}_{} [dir=back label=\"\"];\n",
							i, j, arg.0.index(), arg.1.as_usize()).as_bytes());
					}
					/*for arg in bb.inner_graph.args_of(Index::from_usize(j)) {
						//TODO properly escape
						buffer.extend(format!("  n{}_{} <- n{}_{} [dir=back label=\"\"];\n",
							i, j, i, arg.as_usize()).as_bytes());
					}*/
				}
			}
			_ => {}
		}
	}
	buffer.extend("}\n".as_bytes());
	buffer
}
