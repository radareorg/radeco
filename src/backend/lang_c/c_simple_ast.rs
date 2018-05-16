//! Block by block CAST

use std::{default, iter, fmt};
use std::collections::LinkedList;

use super::c_simple;
use super::c_simple::{CAST, CASTNode};
use frontend::radeco_containers::RadecoFunction;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use petgraph::visit::EdgeRef;
use petgraph::EdgeDirection;
use petgraph::Direction;

#[derive(Debug, Clone, PartialEq)]
enum SimpleCASTNode {
    Action(ActionNode),
    Value(ValueNode),
}

#[derive(Debug, Clone, PartialEq)]
enum ActionNode {
    Assignment,
    Call,
    Return,
    If,
    Goto,
}

#[derive(Debug, Clone, PartialEq)]
enum ValueNode {
    // The string is the name of variable
    Variable(String),
    // TODO
    Constant,
}

#[derive(Debug, Clone, PartialEq)]
enum SimpleCASTEdge {
    Action(ActionEdge),
    Value(ValueEdge),
}

#[derive(Debug, Clone, PartialEq)]
enum ActionEdge {
    IfThen,
    IfElse,
    FuncCall,
    FuncRet,
    Normal,
}

#[derive(Debug, Clone, PartialEq)]
enum ValueEdge {
    AssignSrc,
    AssignDst,
    Arg(u8),
    GotoDst,
}

struct SimpleCAST {
    // Name of function of this AST
    fname: String,
    entry: NodeIndex,
    ast: Graph<SimpleCASTNode, SimpleCASTEdge>,
}


impl SimpleCASTNode {
    fn is_exit(&self) -> bool {
        match self {
            &SimpleCASTNode::Action(ActionNode::Return) => true,
            _ => false,
        }
    }
}

impl SimpleCAST {

    fn next_actions(&self, idx: NodeIndex) -> Vec<NodeIndex> {
        self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Action(_) => Some(e.target()),
                    _ => None,
                }
            }).collect()
    }

    /// Returns a pair (IfThen, IfElse)
    fn branch(&self, idx: NodeIndex) -> Option<(NodeIndex, Option<NodeIndex>)> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::If)) {
            return None;
        }
        let ns = self.next_actions(idx);
        // TODO This implementation is not correct
        // specify node by matching IfThen, IfElse
        if ns.len() == 2 {
            Some((ns[0], Some(ns[1])))
        } else if ns.len() <= 1 {
            Some((ns[0], None))
        } else {
            radeco_warn!("ns.len() should return less than or equal 2");
            None
        }
    }

    /// Returns destination of goto statement
    fn goto(&self, idx: NodeIndex) -> Option<NodeIndex> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::Goto))  {
            return None;
        };
        let goto_dsts = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Value(ValueEdge::GotoDst) => Some(e.target()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if goto_dsts.len() != 1 {
            radeco_warn!("Destination of Goto has more than 1 labels");
            return None;
        };
        goto_dsts.first().map(|n| n.clone())
    }

    /// Returns a pair (Dst, Src) which represents Dst = Src
    fn assignment(&self, idx: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::Assignment)) {
            return None;
        }
        let src = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter(|e| e.weight() == &SimpleCASTEdge::Value(ValueEdge::AssignSrc))
            .map(|e| e.target())
            .next();
        let dst = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter(|e| e.weight() == &SimpleCASTEdge::Value(ValueEdge::AssignDst))
            .map(|e| e.target())
            .next();
        if src.is_some() && dst.is_some() {
            return None;
        }
        Some((dst.unwrap(), src.unwrap()))
    }

    /// Returns arguments of function call
    fn args_call(&self, idx: NodeIndex) -> Option<Vec<NodeIndex>> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::Call)) {
            return None;
        }
        let mut args = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Value(ValueEdge::Arg(o)) => Some((o, e.target())),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        args.sort_by_key(|k| k.0);
        let ret = args.into_iter()
            .map(|(_, b)| b)
            .collect();
        Some(ret)
    }

    fn to_c_ast(&self) -> CAST {
        let mut c_ast = CAST::new(&self.fname);
        let mut queue = LinkedList::new();
        queue.push_back(self.entry);
        while let Some(current_node) = queue.pop_front() {
            // XXX ?
            for n in self.next_actions(current_node) {
                queue.push_back(n);
            }
            match self.ast.node_weight(current_node) {
                Some(&SimpleCASTNode::Action(ActionNode::Assignment)) => {
                    if let Some((dst, src)) = self.assignment(current_node) {
                        // TODO insert node
                    } else {
                        // TODO
                        radeco_err!("Something wrong");
                    }
                },
                Some(&SimpleCASTNode::Action(ActionNode::Call)) => {
                    let args = self.args_call(current_node);
                    // TODO insert node, args
                },
                Some(&SimpleCASTNode::Action(ActionNode::Return)) => {
                    // TODO insert node
                },
                Some(&SimpleCASTNode::Action(ActionNode::If)) => {
                    // TODO insert node
                    if let Some((if_then, if_else)) = self.branch(current_node) {
                        // TODO
                    }
                },
                Some(&SimpleCASTNode::Action(ActionNode::Goto)) => {
                    if let Some(dst) = self.goto(current_node) {
                        // TODO insert label, gotos
                    }
                },
                _ => {},
            };
        }
        c_ast
    }

}
