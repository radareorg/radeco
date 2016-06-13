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

use middle::ir::MOpcode;
use middle::ssa::ssa_traits::{SSAMod, SSAWalk, SSA, NodeData, NodeType};
use middle::ssa::ssastorage::{SSAStorage};

#[derive(Clone, Debug)]
pub struct ParseToken {
    current: Option<String>,
    lhs: Option<String>,
    rhs: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Match<T: Clone + fmt::Debug> {
    root: T,
    bindings: Option<Vec<(String, T)>>,
}

pub struct GraphMatcher<'a, I, S>
    where I: Iterator<Item = S::ValueRef>,
          S: 'a + SSA + SSAMod + SSAWalk<I>
{
    ssa: &'a mut S,
    find: String,
    replace: Option<String>,
    foo: PhantomData<I>,
}


impl<'a, I, S> GraphMatcher<'a, I, S>
where I: Iterator<Item=S::ValueRef>,
      S: 'a + SSA + SSAMod + SSAWalk<I>
{
    pub fn new(ssa: &'a mut S, find: String, replace: Option<String>) -> GraphMatcher<'a, I, S> {
        GraphMatcher {
            ssa: ssa,
            find: find,
            replace: replace,
            foo: PhantomData,
        }
    }

    // Some notes on parsing the expression (find / replace patterns).
    pub fn next_token(&mut self, expr: &str) -> ParseToken {
        println!("{}", expr);
        lazy_static! {
            static ref RE: Regex = Regex::new("[(]([:alnum:]+) ([(]?[a-zA-Z0-9 ,%$]+[)]?), ([(]?[:ascii:]+[)]?)[)]").unwrap();
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

    fn hash_data(&mut self, ni: S::ValueRef) -> String {
        let mut result = String::new();
        if let Ok(node_data) = self.ssa.get_node_data(&ni) {
            if let NodeType::Op(opc) = node_data.nt {
                result.push_str(&match opc {
                    MOpcode::OpConst(val) => format!("${}", val),
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
                    MOpcode::OpNarrow(w) => format!("OpNarrow({})", w),
                    MOpcode::OpWiden(w) => format!("OpWiden({})", w),
                    MOpcode::OpCall => "OpCall".to_owned(),
                    _ => unreachable!(),
                });
            }
        }
        result
    }

    /// Returns the root of the subtree that matches the given `find` expression.
    pub fn grep(&mut self, find: String) -> Vec<Match<S::ValueRef>> {
        let mut subtree_match_map = Vec::<(S::ValueRef, Option<String>)>::new();
        let mut bindings = HashMap::new();
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
                let t = self.next_token(match_str.as_ref().unwrap());
                println!("Got: {:?}", t);

                if t.current.is_none() {
                    // Mismatch
                    viable = false;
                    break;
                }

                let current = t.current.unwrap();
                println!("{} {}", current, current_h);
                if current.starts_with("%") {
                    if let Some((old, _)) = bindings.insert(current, (current_h.clone(), inner_node)) {
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
                println!("{:?}", args);

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
                // XXX
                let mut vbind = Vec::new();
                for (k, v) in &bindings {
                    vbind.push((k.clone(), v.1));
                }

                let nmatch = Match {
                    root: node,
                    bindings: Some(vbind),
                };
                found.push(nmatch);
            }
        }

        println!("{:?}", found);
        found
    }

    /// Replaces the subtree rooted at `S::ValueRef` by the `replace` expression.
    pub fn replace(&mut self, subtree: S::ValueRef, replace: String) {
        unimplemented!()
    }

    pub fn match_and_replace(ssa: &mut SSAStorage, pattern: String, replace: String) {
        unimplemented!();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use middle::ssa::ssastorage::SSAStorage;
    use middle::ssa::ssa_traits::{SSAMod, SSA, ValueType};
    use middle::ssa::cfg_traits::{CFG, CFGMod};
    use middle::ir::MOpcode;

    #[test]
    fn match_token() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(OpXor %1, %1)".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa, find_pat.clone(), None);
        let t = matcher.next_token(&find_pat);
        assert_eq!(Some("OpXor".to_owned()), t.current);
        assert_eq!(Some("%1".to_owned()), t.lhs);
        assert_eq!(Some("%1".to_owned()), t.rhs);
    }

    #[test]
    fn match_token1() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(EEq eax, (EAdd eax, (EAdd eax, cf)))".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa, find_pat.clone(), None);
        let t = matcher.next_token(&find_pat);
        assert_eq!(Some("EEq".to_owned()), t.current);
        assert_eq!(Some("eax".to_owned()), t.lhs);
        assert_eq!(Some("(EAdd eax, (EAdd eax, cf))".to_owned()), t.rhs);
    }

    #[test]
    fn match_token2() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(EAdd (EAdd eax, of), (EAdd eax, cf))".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa, find_pat.clone(), None);
        let t = matcher.next_token(&find_pat);
        assert_eq!(Some("EAdd".to_owned()), t.current);
        assert_eq!(Some("(EAdd eax, of)".to_owned()), t.lhs);
        assert_eq!(Some("(EAdd eax, cf)".to_owned()), t.rhs);
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

        let find_pat = "(OpAdd $2, $1)".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa, find_pat.clone(), None);
        matcher.grep(find_pat);
    }

    #[test]
    fn simple_grep_test2() {
        let mut ssa = SSAStorage::new();
        let vt = ValueType::Integer { width: 64 };
        let add = ssa.add_op(MOpcode::OpXor, vt, None);
        let const_1 = ssa.add_const(1);
        let const_2 = ssa.add_const(1);
        ssa.op_use(add, 0, const_1);
        ssa.op_use(add, 1, const_2);
        ssa.mark_start_node(&add);

        let find_pat = "(OpXor %1, %1)".to_owned();
        let mut matcher = GraphMatcher::new(&mut ssa, find_pat.clone(), None);
        matcher.grep(find_pat);
    }
}
