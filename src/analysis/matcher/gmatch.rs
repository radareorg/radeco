//! Implements graph pattern matching
//!
//! This module implements grep-and-replace functionality on SSA graph.
//! The grep and replace pattern uses "bindings" to access variables.
//! A variable can be bound by using %<i> syntax. Examples below show the same.
//!
//! Example of a query expression:
//! (OpXor %1, %1) -> (OpConst(0))
//!
//! Example for comparison operation (JLE):
//!
//! Example for matching x86 function prologue:
//!

use std::collections::{HashMap, VecDeque};
use std::marker::PhantomData;
use std::fmt;

use regex::Regex;

use middle::ir::{MAddress, MOpcode};
use middle::ssa::ssa_traits::{NodeData, NodeType, SSA, SSAMod, SSAWalk, ValueType};
use middle::ssa::ssastorage::SSAStorage;

#[derive(Clone, Debug)]
struct ParseToken {
    current: Option<String>,
    lhs: Option<String>,
    rhs: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Match<T: Clone + fmt::Debug> {
    root: T,
    bindings: Vec<(String, T)>,
}

pub struct GraphMatcher<'a, I, S>
    where I: Iterator<Item = S::ValueRef>,
          S: 'a + SSA + SSAMod + SSAWalk<I>
{
    ssa: &'a mut S,
    seen: HashMap<String, ParseToken>,
    foo: PhantomData<I>,
}

#[macro_export]
macro_rules! grep {
    ($s: ident, $f: expr) => {
        {
            let matcher = gmatch::GraphMatcher::new(&mut $s);
            matcher.grep($f.to_owned())
        }
    };
}

#[macro_export]
macro_rules! grep_and_replace {
    ($s: ident, $f: expr => $r: expr) => {
        {
            let mut matcher = gmatch::GraphMatcher::new(&mut $s);
            for m in matcher.grep($f.to_owned()) {
                matcher.replace(m, $r.to_owned());
            }
        }
    };
}

impl<'a, I, S> GraphMatcher<'a, I, S>
where I: Iterator<Item=S::ValueRef>,
      S: 'a + SSA + SSAMod + SSAWalk<I>
{
    pub fn new(ssa: &'a mut S) -> GraphMatcher<'a, I, S> {
        GraphMatcher {
            ssa: ssa,
            seen: HashMap::new(),
            foo: PhantomData,
        }
    }

    // Some notes on parsing the expression (find / replace patterns).
    fn parse_expression(&self, expr: &str) -> ParseToken {
        lazy_static! {
            static ref RE: Regex = Regex::new("[(]([:alnum:]+) ([(][a-zA-Z0-9 %#,]+[)]|[a-zA-Z0-9 %#]+)(?:, ([(]?[:ascii:]+[)]?))?[)]").unwrap();
        }

        if let Some(ref cap) = RE.captures(expr) {
            ParseToken {
                current: cap.at(1).map(|x| x.to_owned()),
                lhs: cap.at(2).map(|x| x.to_owned()),
                rhs: cap.at(3).map(|x| x.to_owned()),
            }
        } else {
            ParseToken {
                current: Some(expr.to_owned()),
                lhs: None,
                rhs: None,
            }
        }
    }

    fn hash_data(&self, ni: S::ValueRef) -> String {
        let mut result = String::new();
        if let Ok(node_data) = self.ssa.get_node_data(&ni) {
            if let NodeType::Op(opc) = node_data.nt {
                result.push_str(&match opc {
                    MOpcode::OpConst(val) => format!("#x{:x}", val),
                    MOpcode::OpAdd => "OpAdd".to_owned(),
                    MOpcode::OpSub => "OpSub".to_owned(),
                    MOpcode::OpMul => "OpMul".to_owned(),
                    MOpcode::OpDiv => "OpDiv".to_owned(),
                    MOpcode::OpMod => "OpMod".to_owned(),
                    MOpcode::OpAnd => "OpAnd".to_owned(),
                    MOpcode::OpOr => "OpOr".to_owned(),
                    MOpcode::OpXor => "OpXor".to_owned(),
                    MOpcode::OpNot => "OpNot".to_owned(),
                    MOpcode::OpCmp => "OpCmp".to_owned(), 
                    MOpcode::OpGt => "OpGt".to_owned(),
                    MOpcode::OpLt => "OpLt".to_owned(),
                    MOpcode::OpLsl => "OpLsl".to_owned(),
                    MOpcode::OpLsr => "OpLsr".to_owned(),
                    MOpcode::OpLoad => "OpLoad".to_owned(),
                    MOpcode::OpStore => "OpStore".to_owned(), 
                    MOpcode::OpNarrow(w) => format!("OpNarrow{}", w),
                    MOpcode::OpWiden(w) => format!("OpWiden{}", w),
                    MOpcode::OpCall => "OpCall".to_owned(),
                    _ => unreachable!(),
                });
            } else if let NodeType::Comment(s) = node_data.nt {
                result.push_str(&s);
            }
        }
        result
    }

    /// Returns the root of the subtree that matches the given `find` expression.
    // TODO:
    //   1. Optimize by adding a first level of filtering for potential nodes.
    pub fn grep(&self, find: String) -> Vec<Match<S::ValueRef>> {
        let mut subtree_match_map = Vec::<(S::ValueRef, Option<String>)>::new();
        let mut bindings = HashMap::new();
        let mut seen_exprs = HashMap::new();
        let mut found = Vec::new();
        for node in self.ssa.inorder_walk() {
            let mut viable = true;
            subtree_match_map.clear();
            bindings.clear();
            subtree_match_map.push((node, Some(find.clone())));
            while let Some((inner_node, match_str)) = subtree_match_map.pop() {
                if match_str.is_none() {
                    // Mismatch
                    viable = false;
                    break;
                }
                let current_h = self.hash_data(inner_node);
                // Optimize by avoiding a regex match when an expression is already known/seen.
                let t = if let Some(tk) = seen_exprs.get(match_str.as_ref().unwrap()).cloned() {
                    tk
                } else {
                    let t_ = self.parse_expression(match_str.as_ref().unwrap());
                    seen_exprs.insert(match_str.unwrap(), t_.clone());
                    t_
                };

                if t.current.is_none() {
                    // Mismatch
                    viable = false;
                    break;
                }

                let current = t.current.unwrap();
                if current.starts_with("%") {
                    if let Some((old, _)) = bindings.insert(current,
                                                            (current_h.clone(), inner_node)) {
                        if old != current_h {
                            // Mismatch
                            viable = false;
                            break;
                        }
                    }
                } else if current_h != current {
                    // Mismatch
                    // No match case.
                    viable = false;
                    break;
                }

                let args = self.ssa.args_of(inner_node);

                // All the cases which can cause a mismatch in the node and it's arguments.
                if args.len() > 2 || (args.len() == 2 && (t.rhs.is_none() || t.lhs.is_none())) ||
                   (args.len() == 1 && t.lhs.is_none()) ||
                   (args.len() == 0 && (t.lhs.is_some() || t.rhs.is_some())) {
                    viable = false;
                    break;
                }

                for (i, argn) in args.iter().enumerate() {
                    let subtree_str = match i {
                        0 => t.lhs.clone(),
                        1 => t.rhs.clone(),
                        _ => unreachable!(),
                    };
                    subtree_match_map.push((*argn, subtree_str));
                }
            }

            if viable {
                let mut vbind = Vec::new();
                for (k, v) in &bindings {
                    vbind.push((k.clone(), v.1));
                }

                let nmatch = Match {
                    root: node,
                    bindings: vbind,
                };
                found.push(nmatch);
            }
        }

        found
    }

    fn map_token_to_node(&mut self,
                         t: &str,
                         block: &S::ActionRef,
                         addr: &mut MAddress)
                         -> S::ValueRef {
        let opcode = if t.starts_with("#x") {
            Some(MOpcode::OpConst(u64::from_str_radix(&t[2..], 16).expect("Invalid hex integer")))
        } else if t.starts_with("OpNarrow") {
            Some(MOpcode::OpNarrow(u16::from_str_radix(&t[8..], 10)
                                       .expect("Invalid decimal integer")))
        } else if t.starts_with("OpWiden") {
            Some(MOpcode::OpWiden(u16::from_str_radix(&t[7..], 10)
                                      .expect("Invalid decimal integer")))
        } else {
            match t {
                "OpAdd" => Some(MOpcode::OpAdd),
                "OpSub" => Some(MOpcode::OpSub),
                "OpMul" => Some(MOpcode::OpMul),
                "OpDiv" => Some(MOpcode::OpDiv),
                "OpMod" => Some(MOpcode::OpMod),
                "OpAnd" => Some(MOpcode::OpAnd),
                "OpOr" => Some(MOpcode::OpOr),
                "OpXor" => Some(MOpcode::OpXor),
                "OpNot" => Some(MOpcode::OpNot),
                "OpCmp" => Some(MOpcode::OpCmp),
                "OpGt" => Some(MOpcode::OpGt),
                "OpLt" => Some(MOpcode::OpLt),
                "OpLsl" => Some(MOpcode::OpLsl),
                "OpLsr" => Some(MOpcode::OpLsr),
                "OpLoad" => Some(MOpcode::OpLoad),
                "OpStore" => Some(MOpcode::OpStore),
                "OpCall" => Some(MOpcode::OpCall),
                _ => None,
            }
        };
        if let Some(op) = opcode {
            let node = self.ssa.add_op(op, ValueType::Integer { width: 64 }, None);
            match op {
                MOpcode::OpConst(_) => {}
                _ => {
                    addr.offset += 1;
                    self.ssa.add_to_block(node, *block, *addr);
                }
            }
            node
        } else {
            let node = self.ssa.add_comment(ValueType::Integer { width: 0 }, t.to_owned());
            addr.offset += 1;
            self.ssa.add_to_block(node, *block, *addr);
            node
        }
    }

    /// Replaces the subtree rooted at `S::ValueRef` by the `replace` expression.
    pub fn replace(&mut self, found: Match<S::ValueRef>, replace: String) {
        let bindings = found.bindings.iter().cloned().collect::<HashMap<_, _>>();
        // Vec of (parent, lhs/rhs (0/1), parse_expression)
        let mut worklist = Vec::new();
        // Remove operands to the root node and prepare for replace.
        let root = found.root;
        let mut address = self.ssa.get_address(&root);
        let block = self.ssa.block_of(&root);
        
        // Now we have a root node with no args, but retaining its uses.
        // Replace this node with the root node in the replace expression.
        for arg in &self.ssa.args_of(root) {
            self.ssa.disconnect(&root, arg);
        }

        let r = if let Some(tk) = self.seen.get(&replace).cloned() {
            tk
        } else {
            let t_ = self.parse_expression(&replace);
            self.seen.insert(replace, t_.clone());
            t_
        };

        let replace_root = self.map_token_to_node(r.current.as_ref().unwrap(),
                                                  &block,
                                                  &mut address);
        self.ssa.replace(root, replace_root);
        if let Some(lhs) = r.lhs {
            worklist.push((replace_root, 0, lhs));
        }
        if let Some(rhs) = r.rhs {
            worklist.push((replace_root, 1, rhs));
        }

        while let Some((parent, edge_idx, subexpr)) = worklist.pop() {
            let pt = if let Some(tk) = self.seen.get(&subexpr).cloned() {
                tk
            } else {
                let t_ = self.parse_expression(&subexpr);
                self.seen.insert(subexpr, t_.clone());
                t_
            };
            let current = pt.current.unwrap();
            let inner_node = if current.starts_with("%") {
                *bindings.get(&current).expect("Unknown Binding")
            } else {
                self.map_token_to_node(&current, &block, &mut address)
            };
            self.ssa.op_use(parent, edge_idx, inner_node);
            if let Some(lhs) = pt.lhs {
                worklist.push((inner_node, 0, lhs));
            }
            if let Some(rhs) = pt.rhs {
                worklist.push((inner_node, 1, rhs));
            }
        }

        address.offset += 1;
        self.ssa.set_addr(&replace_root, address);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use analysis::matcher::gmatch;
    use std::io::prelude::*;
    use std::io;
    use middle::ssa::ssastorage::SSAStorage;
    use middle::ssa::ssa_traits::{SSA, SSAMod, ValueType};
    use middle::ssa::cfg_traits::{CFG, CFGMod};
    use middle::ir::MOpcode;
    use middle::ir_writer::IRWriter;

    #[test]
    fn parse_expr() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(OpXor %1, %1)".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("OpXor".to_owned()), t.current);
        assert_eq!(Some("%1".to_owned()), t.lhs);
        assert_eq!(Some("%1".to_owned()), t.rhs);
    }

    #[test]
    fn parse_expr1() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(EEq eax, (EAdd eax, (EAdd eax, cf)))".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("EEq".to_owned()), t.current);
        assert_eq!(Some("eax".to_owned()), t.lhs);
        assert_eq!(Some("(EAdd eax, (EAdd eax, cf))".to_owned()), t.rhs);
    }

    #[test]
    fn parse_expr2() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(EAdd (EAdd eax, of), (EAdd eax, cf))".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("EAdd".to_owned()), t.current);
        assert_eq!(Some("(EAdd eax, of)".to_owned()), t.lhs);
        assert_eq!(Some("(EAdd eax, cf)".to_owned()), t.rhs);
    }

    #[test]
    fn parse_expr_unary() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(OpNot rax)".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("OpNot".to_owned()), t.current);
        assert_eq!(Some("rax".to_owned()), t.lhs);
        assert!(t.rhs.is_none());
    }

    #[test]
    fn simple_grep_test() {
        let mut ssa = SSAStorage::new();
        let vt = ValueType::Integer { width: 64 };
        let add = ssa.add_op(MOpcode::OpAdd, vt, None);
        let const_1 = ssa.add_const(1);
        let const_2 = ssa.add_const(2);
        ssa.op_use(add, 0, const_1);
        ssa.op_use(add, 1, const_2);
        ssa.mark_start_node(&add);
        let _ = ssa.add_op(MOpcode::OpAdd, vt, None);
        grep!(ssa, "(OpAdd #x1, #x2)");
    }

    #[test]
    fn simple_grep_test2() {
        let mut ssa = SSAStorage::new();
        let vt = ValueType::Integer { width: 64 };
        let add = ssa.add_op(MOpcode::OpXor, vt, None);
        let const_1 = ssa.add_const(1);
        ssa.op_use(add, 0, const_1);
        ssa.op_use(add, 1, const_1);
        ssa.mark_start_node(&add);
        grep!(ssa, "(OpXor %1, %1)");
    }
}
