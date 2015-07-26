//! Module to parse and write dot files for graphs.

use std::collections::HashMap;

macro_rules! add_strings {
	( $( $x: expr ),* ) => {
		{
			let mut s = String::new();
			$(
				s.push_str(&format!("{}", $x));
			 )*
				s
		}
	};
}

pub enum DotAttrBlock {
	Raw(String),
	Attributes(Vec<(String, String)>),
}

impl DotAttrBlock {
	fn bake(&mut self) -> &String {
		let r: String = match self {
			&mut DotAttrBlock::Raw(ref l) => return l,
			&mut DotAttrBlock::Attributes(ref attrs) if attrs.len() == 0 => "".to_string(),
			&mut DotAttrBlock::Attributes(ref attrs) => {
				let mut t = " [".to_string();
				for &(ref k, ref v) in attrs {
					t.push_str(&*format!(" {}={}", k, v));
				}
				t.push_str(" ]");
				t
			}
		};
		*self = DotAttrBlock::Raw(r);
		if let &mut DotAttrBlock::Raw(ref r) = self { return r }
		panic!();
	}
}

pub trait GraphDot {
	type NodeType;
	type EdgeType;

	fn configure(&self) -> String;
	fn nodes(&self) -> Vec<Self::NodeType>;
	fn edges(&self) -> Vec<Self::EdgeType>;
	fn get_node(&self, n: usize) -> Option<&Self::NodeType>;
	fn node_cluster(&self, _: usize) -> usize { 0 }

	fn node_skip(&self, &Self::NodeType) -> bool { false }
	fn node_attrs(&self, &Self::NodeType) -> DotAttrBlock;

	fn edge_skip(&self, &Self::EdgeType) -> bool { false }
	fn edge_attrs(&self, &Self::EdgeType) -> DotAttrBlock;

	fn edge_source(&self, &Self::EdgeType) -> usize;
	fn edge_target(&self, &Self::EdgeType) -> usize;
}

pub fn emit_dot<T: GraphDot>(g: &T) -> String {
	let mut result = String::new();
	result.push_str(&*g.configure());

	// Node configurations
	{
		let nodes = g.nodes();

		let mut links: Vec<usize> = Vec::with_capacity(nodes.len());
		let mut clustermap: HashMap<usize, (usize, usize)> = HashMap::new();

		for i in 0..nodes.len() {
			let cid = g.node_cluster(i);
			let entry = clustermap.entry(cid);
			let ec = entry.or_insert((i, i));
			links.push(ec.1);
			ec.1 = i;
		}

		//result.push_str(&*format!("subgraph cluster_A {{\n{{style=invis;\n"));
		//for (&cid, &(_, _)) in clustermap.iter() {
		//    result.push_str(&*add_strings!("n", format!("{}", cid)));
		//    result.push_str(";\n");
		//}
		//result.push_str(&*format!("}}\n"));

		for (&cid, &(first, last)) in clustermap.iter() {
			result.push_str(&*format!("subgraph cluster_{} {{\n", cid));
			let mut i = last;
			loop {
				result.push_str(&*add_strings!("n", i));
				result.push_str(&*g.node_attrs(&nodes[i]).bake());
				result.push_str(";\n");
				if i == first { break }
				i = links[i];
			}
			result.push_str("}\n");
		}

		//result.push_str("color = invis;\n");
		//result.push_str("}\n");
	}

	// Connect nodes by edges.
	for edge in g.edges().iter() {
		if g.edge_skip(edge) { continue }
		result.push_str(&*add_strings!(
				"n", g.edge_source(edge), " -> ",
				"n", g.edge_target(edge), g.edge_attrs(edge).bake(), ";\n"
				));
	}

	result.push_str("\n}\n");
	result
}
