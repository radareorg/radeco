// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Graph visualization traits and functions to emit dot code.

use std::collections::HashMap;
use std::hash::Hash;
use std::cmp::Eq;
use std::fmt::Debug;

use petgraph::graph::NodeIndex;

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

/// Represents the contents of a GraphViz attribute block
pub enum DotAttrBlock {
    /// The attribute block as string including the surrounding square brackets.
    /// Values have to be escaped manually.
    Raw(String),
    /// List of key-value pairs.
    /// Values will be escaped for you.
    Attributes(Vec<(String, String)>),
    /// Represents one line in a dot file
    Hybrid(String, Vec<(String, String)>),
}

impl DotAttrBlock {
    fn bake(&mut self) -> &String {
        let mut r = String::new();
        let attr = if let &mut DotAttrBlock::Hybrid(ref s, ref attr) = self {
            r.push_str(&s);
            attr.clone()
        } else {
            Vec::new()
        };

        if attr.len() > 0 {
            *self = DotAttrBlock::Attributes(attr);
        }

        let s: String = match self {
            &mut DotAttrBlock::Raw(ref l) => return l,
            &mut DotAttrBlock::Attributes(ref attrs) => {
                if attrs.len() == 0 {
                    "".to_owned()
                } else {
                    let mut t = " [".to_string();
                    for &(ref k, ref v) in attrs {
                        t.push_str(&*format!(" {}={}", k, v));
                    }
                    t.push_str(" ]");
                    t
                }
            }
            _ => unreachable!(),
        };

        r.push_str(&s);
        r.push_str(";\n");
        *self = DotAttrBlock::Raw(r);
        if let &mut DotAttrBlock::Raw(ref r) = self {
            return r;
        }
        unreachable!();
    }
}

pub trait Index {
    fn to_index(&self) -> usize;
}

impl Index for NodeIndex {
    fn to_index(&self) -> usize {
        self.index()
    }
}

/// This trait enables graphs to be generated from implementors.
pub trait GraphDot {
	type NodeIndex: Hash + Clone + Eq + Index + Debug;
	type EdgeIndex: Hash + Clone + Eq;

    fn node_index_new(usize) -> Self::NodeIndex;
    fn edge_index_new(usize) -> Self::EdgeIndex;

    fn configure(&self) -> String;
    fn node_count(&self) -> usize;
    fn edge_count(&self) -> usize;
    fn nodes(&self) -> Vec<Self::NodeIndex>;
    // fn edges(&self) -> Vec<Self::EdgeType>;
    // fn get_node(&self, n: usize) -> Option<&Self::NodeType>;

    /// Nodes with the same node_cluster return value will be put in the same
    /// graphviz-cluster.
    fn node_cluster(&self, _: &Self::NodeIndex) -> Option<usize> {
        Some(0)
    }

    fn node_skip(&self, &Self::NodeIndex) -> bool {
        false
    }
    fn node_attrs(&self, &Self::NodeIndex) -> DotAttrBlock;

    fn edge_skip(&self, &Self::EdgeIndex) -> bool {
        false
    }
    fn edge_attrs(&self, &Self::EdgeIndex) -> DotAttrBlock;

    fn edge_source(&self, &Self::EdgeIndex) -> Self::NodeIndex;
    fn edge_target(&self, &Self::EdgeIndex) -> Self::NodeIndex;
}

pub fn emit_dot<T: GraphDot>(g: &T) -> String {
    let mut result = String::new();
    result.push_str(&*g.configure());

    // Node configurations
    {
        let nodes = g.nodes();
        let mut clustermap = HashMap::<T::NodeIndex, Vec<T::NodeIndex>>::new();

        for i in &nodes {
            let block = g.node_cluster(i);
            clustermap.entry(T::node_index_new(block.unwrap()))
                      .or_insert(Vec::new())
                      .push(i.clone());
        }

        for (k, v) in clustermap.iter() {
            result.push_str(&*format!("subgraph cluster_{} {{\n", k.to_index()));
            result.push_str(&*format!("rankdir=TB;\n"));
            for node in v.iter() {
                result.push_str(&*g.node_attrs(node).bake());
            }
            result.push_str("}\n");
        }
    }

    // Connect nodes by edges.
    for i in 0..g.edge_count() {
        let edge_i = &T::edge_index_new(i);
        if g.edge_skip(edge_i) {
            continue;
        }
        result.push_str(g.edge_attrs(edge_i).bake());
    }

    result.push_str("\n}\n");
    result
}
