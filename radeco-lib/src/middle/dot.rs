// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Graph visualization traits and functions to emit dot code.

use std::cmp::Eq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use petgraph::graph::NodeIndex;

#[allow(unused_macros)]
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

/// Represents the contents of a `GraphViz` attribute block
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
        let attr = if let DotAttrBlock::Hybrid(ref s, ref attr) = *self {
            r.push_str(s);
            attr.clone()
        } else {
            Vec::new()
        };

        if !attr.is_empty() {
            *self = DotAttrBlock::Attributes(attr);
        }

        let s: String = match *self {
            DotAttrBlock::Raw(ref l) => return l,
            DotAttrBlock::Attributes(ref attrs) => {
                if attrs.is_empty() {
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
        if let DotAttrBlock::Raw(ref r) = *self {
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
    fn edges(&self) -> Vec<Self::EdgeIndex>;
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
            let block = g.node_cluster(i).unwrap_or_else(|| {
                radeco_err!("Block not found");
                0
            });
            clustermap
                .entry(T::node_index_new(block))
                .or_insert_with(Vec::new)
                .push(i.clone());
        }

        for (k, v) in &clustermap {
            result.push_str(&*format!("subgraph cluster_{} {{\n", k.to_index()));
            result.push_str("style=filled;\n");
            result.push_str("fillcolor=gray;\n");
            result.push_str("rankdir=TB;\n");
            for node in v.iter() {
                result.push_str(&*g.node_attrs(node).bake());
            }
            result.push_str("}\n");
        }
    }

    // Connect nodes by edges.
    for edge_i in g.edges() {
        if g.edge_skip(&edge_i) {
            continue;
        }
        result.push_str(g.edge_attrs(&edge_i).bake());
    }

    result.push_str("\n}\n");
    result
}
