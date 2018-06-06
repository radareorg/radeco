//! see [`nearest_common_dominator`]

use super::ix_bit_set::{IndexLike, IxBitSet};
use super::NonEmptyPath;
use petgraph::visit::{EdgeRef, GraphBase, IntoEdgesDirected};
use petgraph::Incoming;

/// Finds the nearest common dominator of a set of nodes.
///
/// A *common dominator* of a set of nodes is a node that dominates every node
/// in that set.
///
/// The *nearest common dominator* is the unique node that:
///   1. is a common dominator
///   2. does not dominate any other common dominator
///
/// This implements the algorithm described in
/// [this paper](https://doi.org/10.1016/0196-6774(92)90063-I).
pub fn nearest_common_dominator<G>(
    graph: G,
    entry: G::NodeId,
    nodes: &IxBitSet<G::NodeId>,
) -> G::NodeId
where
    G: IntoEdgesDirected,
    G::NodeId: IndexLike,
    G::EdgeId: IndexLike,
{
    if nodes.is_empty() {
        panic!("nearest common dominator of empty set");
    }
    if nodes.contains(entry) {
        return entry;
    }
    NcdState::new(graph, entry, nodes).run()
}

struct NcdState<G>
where
    G: IntoEdgesDirected,
    G::NodeId: IndexLike,
    G::EdgeId: IndexLike,
{
    graph: G,
    entry: G::NodeId,
    reentered: IxBitSet<G::NodeId>,
    visited_nodes: IxBitSet<G::NodeId>,
    visited_edges: IxBitSet<G::EdgeId>,
}

type Stack<G> = NonEmptyPath<<G as GraphBase>::NodeId, <G as GraphBase>::EdgeId>;

impl<G> NcdState<G>
where
    G: IntoEdgesDirected,
    G::NodeId: IndexLike,
    G::EdgeId: IndexLike,
{
    fn new(graph: G, entry: G::NodeId, nodes: &IxBitSet<G::NodeId>) -> Self {
        Self {
            graph,
            entry,
            reentered: nodes.clone(),
            visited_nodes: IxBitSet::new(),
            visited_edges: IxBitSet::new(),
        }
    }

    fn run(&mut self) -> G::NodeId {
        // "initialize one stack for each u in U and mark u as re-entered"
        let mut stacks: Vec<Stack<G>> = self.reentered.iter().map(NonEmptyPath::new).collect();

        // <invariant: forall stack in stacks, reentered.contains(stack.start)>

        // "repeat"
        let stack = loop {
            // "while there are more than one non-empty stacks do"
            let mut stack = loop {
                if let Some(stack) = remove_if_singleton(&mut stacks) {
                    break stack;
                }
                // "perform one round of multiple depth first search"
                // "for each non-empty stack do"
                // TODO: can we do this in-place?
                stacks = stacks
                    .into_iter()
                    .filter_map(|stack| {
                        // "if the top stack element is not s, the source then"
                        if stack.last_node() != self.entry {
                            // "perform one dfs-step for this stack"
                            self.dfs_step(stack)
                        } else {
                            // "else skip"
                            Some(stack)
                        }
                    })
                    .collect();
                // prevent infinite loop
                assert!(!stacks.is_empty());
            };

            // "let x be the top most stack element marked as re-entered"
            // "for each node v on top of x do"
            // "pop the stack"
            // <stack.start is re-entered, so stopping at start is fine>
            debug_assert!(self.reentered.contains(stack.start));
            while let Some((e, v)) = stack.segments.pop() {
                if self.reentered.contains(v) {
                    // undo "pop the stack"
                    stack.segments.push((e, v));
                    break;
                } else {
                    // "let w be the node just beneath v in the stack"
                    // "mark v and (v, w) as unvisited"
                    self.visited_nodes.remove(v);
                    self.visited_edges.remove(e);
                }
            }

            // "for each node remains in the stack do"
            for node in stack.nodes() {
                // "move the node to a new stack by itself"
                stacks.push(NonEmptyPath::new(node));
                // "mark it as re-entered"
                self.reentered.insert(node);
            }

            // "until there is only one non-empty stack"
            if let Some(stack) = remove_if_singleton(&mut stacks) {
                break stack;
            }
        };

        // "the single node in the non-empty stack is d"
        debug_assert!(stack.segments.is_empty());
        stack.start
    }

    fn dfs_step(&mut self, mut stack: Stack<G>) -> Option<Stack<G>> {
        // "while the stack is not empty do"
        let dfs_step_res = loop {
            // "let w be the node on the top of the stack"
            let w = stack.last_node();
            // "unvisited arc (v, w) for w"
            let new_edge_opt = self
                .graph
                .edges_directed(w, Incoming)
                .find(|e| !self.visited_edges.contains(e.id()));

            // rev "if there is no unvisited arc (v, w) for w then"
            if let Some(new_edge) = new_edge_opt {
                // "break"
                break Some((new_edge, stack));
            } else {
                // "pop the stack"
                // "while the stack is not empty"
                if stack.segments.pop().is_none() {
                    break None;
                }
            }
        };

        // "if the stack is not empty then"
        if let Some((new_edge, mut stack)) = dfs_step_res {
            // "let w be the node on the top of the stack and (v, w) be an unvisited arc"
            let v = new_edge.source();
            // "mark (v, w) as visited"
            self.visited_edges.insert(new_edge.id());
            // "if v is unvisited then"
            // <marking visited if visited is fine>
            // "mark v as visited"
            if self.visited_nodes.insert(v) {
                // "push v onto the stack"
                stack.segments.push((new_edge.id(), v));
            } else {
                // "mark v as re-entered"
                self.reentered.insert(v);
            }
            Some(stack)
        } else {
            None
        }
    }
}

/// If `v` contains exactly 1 element, removes and returns it.
/// Does nothing otherwise.
fn remove_if_singleton<T>(v: &mut Vec<T>) -> Option<T> {
    if v.len() == 1 {
        Some(v.pop().unwrap())
    } else {
        None
    }
}
