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

use std::collections::HashMap;
use std::marker::PhantomData;
use std::fmt;

use middle::ir::{MAddress, MOpcode, WidthSpec};
use middle::ssa::ssa_traits::{NodeType, SSA, SSAMod, SSAWalk, ValueInfo};

#[derive(Clone, Debug)]
struct ParseToken {
    parsed: Vec<String>,
}

impl ParseToken {
    fn current(&self) -> Option<String> {
        self.op(0)
    }

    #[allow(dead_code)]
    fn rhs(&self) -> Option<String> {
        self.op(2)
    }

    #[allow(dead_code)]
    fn lhs(&self) -> Option<String> {
        self.op(1)
    }

    fn op(&self, i: usize) -> Option<String> {
        self.parsed.get(i).cloned()
    }

    fn len(&self) -> usize {
        self.parsed.len() - 1
    }
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
    hash_subtrees: HashMap<S::ValueRef, String>,
    foo: PhantomData<I>,
}

#[macro_export]
macro_rules! grep {
    ($s: expr, $f: expr) => {
        {
            let mut matcher = gmatch::GraphMatcher::new($s);
            matcher.grep($f.to_owned())
        }
    };
}

#[macro_export]
macro_rules! grep_and_replace {
    ($s: expr, $f: expr => $r: expr) => {
        {
            let mut matcher = gmatch::GraphMatcher::new($s);
            for m in matcher.grep($f.to_owned()) {
                matcher.replace_value(m, $r.to_owned());
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
            hash_subtrees: HashMap::new(),
            foo: PhantomData,
        }
    }

    // Some notes on parsing the expression (find / replace patterns).
    fn parse_expression(&self, expr: &str) -> ParseToken {
        let mut depth = 0;
        let mut sub_expr = Vec::new();
        sub_expr.push(String::new());
        for c in expr.chars() {
            match c {
                ' ' if depth == 1 => sub_expr.push(String::new()),
                ',' if depth == 1 => { },
                ')' => {
                    depth -= 1;
                    if depth > 0 {
                        sub_expr.last_mut().unwrap().push(c);
                    }
                }
                '(' => {
                    depth += 1;
                    if depth > 1 {
                        sub_expr.last_mut().unwrap().push(c);
                    }
                }
                _ => sub_expr.last_mut().unwrap().push(c),
            }
        }
        ParseToken {
            parsed: sub_expr
        }
    }

    // Warning: This function uses too much time when the fucntion is huge,
    // especially when there are too many call statements. Because call 
    // statements use all the registers for safety.
    //
    // Also, An important point is that this recursion should stop at Phi 
    // nodes, otherwise, if a phi node uses a node after itself, it will 
    // cause infinite loop.
    fn hash_subtree(&mut self, root: S::ValueRef) -> String {
        // Read Cache.
        if self.hash_subtrees.contains_key(&root) {
            return self.hash_subtrees.get(&root).expect("No hash_subtree found!").clone();
        }

        // Only Opcode should be considered its argument.
        let mut argh = String::new();
        if let Ok(node_data) = self.ssa.node_data(root) {
            if let NodeType::Op(_) = node_data.nt {
                for arg in self.ssa.operands_of(root) {
                    if !argh.is_empty() {
                        argh.push_str(", ");
                    }
                    argh = format!("{}{}", argh, self.hash_subtree(arg));
                }
            }
        }

        let node_h = self.hash_data(root);
        let result: String;
        if !argh.is_empty() {
            result = format!("({} {})", node_h, argh);
        } else {
            result = node_h;
        }

        // Cache the result.
        self.hash_subtrees.insert(root, result.clone());
        return result;
    }

    fn hash_data(&self, ni: S::ValueRef) -> String {
        let mut result = String::new();
        if let Ok(node_data) = self.ssa.node_data(ni) {
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
                    MOpcode::OpZeroExt(w) => format!("OpZeroExt{}", w),
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
    //   - Handle commutativity
    pub fn grep(&mut self, find: String) -> Vec<Match<S::ValueRef>> {
        let mut worklist = Vec::<(S::ValueRef, Option<String>)>::new();
        let mut bindings = HashMap::new();
        let mut seen_exprs = HashMap::new();
        let mut found = Vec::new();
        let first_expr = self.parse_expression(&find);
        for node in self.ssa.inorder_walk() {
            // First level of filtering.
            let nh = self.hash_data(node);
            if first_expr.current().as_ref().unwrap() != &nh {
                continue;
            }
            {
                let args = self.ssa.operands_of(node);
                // All the cases which can cause a mismatch in the node and it's arguments.
                if args.len() != first_expr.len() {
                    continue;
                }
                worklist.clear();
                bindings.clear();
                for (i, argn) in args.iter().enumerate() {
                    worklist.push((*argn, first_expr.op(i + 1)));
                }
            }

            let mut viable = true;
            while let Some((inner_node, match_str)) = worklist.pop() {
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

                if t.current().is_none() {
                    // Mismatch
                    viable = false;
                    break;
                }

                let current = t.current().unwrap();
                if current.starts_with('%') {
                    // Match the current subtree to the subtree bound by the variable before. If
                    // they do not match, then report as mismatch.
                    let subtreeh = self.hash_subtree(inner_node);
                    if let Some((old, _)) = bindings.insert(current.clone(),
                                                            (subtreeh.clone(), inner_node)) {
                        if old != subtreeh {
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

                let args = self.ssa.operands_of(inner_node);
                // All the cases which can cause a mismatch in the node and it's arguments.
                // Since "%" binds the entire subtree, it will not have any arguments. So we skip
                // this case.
                if args.len() != t.len() && !current.starts_with('%') {
                    viable = false;
                    break;
                }

                for (i, argn) in args.iter().enumerate() {
                    if i >= t.len() { break; };
                    worklist.push((*argn, t.op(i + 1)));
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
        } else if t.starts_with("OpZeroExt") {
            Some(MOpcode::OpZeroExt(u16::from_str_radix(&t[7..], 10)
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
        if let Some(ref op) = opcode {
            let node = self.ssa.insert_op(op.clone(), ValueInfo::new_unresolved(WidthSpec::from(64)), None)
                                .expect("Cannot insert new values");
            match *op {
                MOpcode::OpConst(_) => {}
                _ => {
                    addr.offset += 1;
                    self.ssa.insert_into_block(node, *block, *addr);
                }
            }
            node
        } else {
            let node = self.ssa.insert_comment(ValueInfo::new_unresolved(WidthSpec::from(64)), t.to_owned())
                                .expect("Cannot insert new comments");
            addr.offset += 1;
            self.ssa.insert_into_block(node, *block, *addr);
            node
        }
    }

    /// Replaces the subtree rooted at `S::ValueRef` by the `replace` expression.
    /// Returns the root of the replaced expression.
    pub fn replace_value(&mut self, found: Match<S::ValueRef>, replace: String) -> S::ValueRef {
        let bindings = found.bindings.iter().cloned().collect::<HashMap<_, _>>();
        // Vec of (parent, lhs/rhs (0/1), parse_expression)
        let mut worklist = Vec::new();
        // Remove operands to the root node and prepare for replace.
        let root = found.root;
        let mut address = self.ssa.address(root).expect("No address information found");
        let block = self.ssa.block_for(root).expect("Value node doesn't belong to any block");

        // Now we have a root node with no args, but retaining its uses.
        // Replace this node with the root node in the replace expression.
        for arg in &self.ssa.operands_of(root) {
            self.ssa.op_unuse(root, *arg);
        }

        let r = if let Some(tk) = self.seen.get(&replace).cloned() {
            tk
        } else {
            let t_ = self.parse_expression(&replace);
            self.seen.insert(replace, t_.clone());
            t_
        };

        let replace_root = if r.current().as_ref().unwrap().starts_with('%') {
            *bindings.get(r.current().as_ref().unwrap()).expect("Unknown Binding")
        } else {
            self.map_token_to_node(r.current().as_ref().unwrap(),
                                                  &block,
                                                  &mut address)
        };

        self.ssa.replace_value(root, replace_root);
        for i in 0..r.len() {
            worklist.push((replace_root, i as u8, r.op(i + 1).unwrap()));
        }

        while let Some((parent, edge_idx, subexpr)) = worklist.pop() {
            let pt = if let Some(tk) = self.seen.get(&subexpr).cloned() {
                tk
            } else {
                let t_ = self.parse_expression(&subexpr);
                self.seen.insert(subexpr, t_.clone());
                t_
            };
            let current = pt.current().unwrap();
            let inner_node = if current.starts_with('%') {
                *bindings.get(&current).expect("Unknown Binding")
            } else {
                self.map_token_to_node(&current, &block, &mut address)
            };
            self.ssa.op_use(parent, edge_idx, inner_node);
            for i in 0..pt.len() {
                worklist.push((inner_node, i as u8, pt.op(i + 1).unwrap()));
            }
        }

        address.offset += 1;
        self.ssa.set_address(replace_root, address);
        replace_root
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use analysis::matcher::gmatch;
    use middle::ssa::ssastorage::{SSAStorage, NodeData};
    use middle::ssa::ssa_traits::{SSA, SSAMod, ValueType, SSAWalk};
    use middle::ssa::cfg_traits::CFGMod;
    use middle::ir::{MOpcode, MAddress};

    #[test]
    fn parse_expr() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(OpXor %1, %1)".to_owned();
        let matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("OpXor".to_owned()), t.current());
        assert_eq!(Some("%1".to_owned()), t.lhs());
        assert_eq!(Some("%1".to_owned()), t.rhs());
    }

    #[test]
    fn parse_expr1() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(EEq eax, (EAdd eax, (EAdd eax, cf)))".to_owned();
        let matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("EEq".to_owned()), t.current());
        assert_eq!(Some("eax".to_owned()), t.lhs());
        assert_eq!(Some("(EAdd eax, (EAdd eax, cf))".to_owned()), t.rhs());
    }

    #[test]
    fn parse_expr2() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(EAdd (EAdd eax, of), (EAdd eax, cf))".to_owned();
        let matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("EAdd".to_owned()), t.current());
        assert_eq!(Some("(EAdd eax, of)".to_owned()), t.lhs());
        assert_eq!(Some("(EAdd eax, cf)".to_owned()), t.rhs());
    }

    #[test]
    fn parse_expr_unary() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(OpNot rax)".to_owned();
        let matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("OpNot".to_owned()), t.current());
        assert_eq!(Some("rax".to_owned()), t.lhs());
        assert!(t.rhs().is_none());
    }

    #[test]
    fn parse_expr_ternary() {
        let mut ssa = SSAStorage::new();
        let find_pat = "(OpStore %1, %2, %3)".to_owned();
        let matcher = GraphMatcher::new(&mut ssa);
        let t = matcher.parse_expression(&find_pat);
        assert_eq!(Some("OpStore".to_owned()), t.current());
        assert_eq!(Some("%1".to_owned()), t.lhs());
        assert_eq!(Some("%2".to_owned()), t.rhs());
        assert_eq!(Some("%3".to_owned()), t.op(3));
    }

    #[test]
    fn simple_grep_test() {
        let mut ssa = SSAStorage::new();
        let vt = ValueInfo::new_unresolved(WidthSpec::from(64));
        let add = ssa.insert_op(MOpcode::OpAdd, vt, None)
                            .expect("Cannot insert new expressions");
        let const_1 = ssa.insert_const(1)
                                .expect("Cannot insert new constants");
        let const_2 = ssa.insert_const(2)
                                .expect("Cannot insert new constants");
        ssa.op_use(add, 0, const_1);
        ssa.op_use(add, 1, const_2);
        ssa.set_entry_node(add);
        let _ = ssa.insert_op(MOpcode::OpAdd, vt, None)
                            .expect("Cannot insert new expressions");
        let m = grep!(&mut ssa, "(OpAdd #x1, #x2)");
        assert_eq!(m[0].root.index(), 0);
    }

    #[test]
    fn simple_grep_test2() {
        let mut ssa = SSAStorage::new();
        let vt = ValueInfo::new_unresolved(WidthSpec::from(64));
        let add = ssa.insert_op(MOpcode::OpXor, vt, None)
                            .expect("Cannot insert new expressions");
        let const_1 = ssa.insert_const(1)
                                .expect("Cannot insert new constants");
        ssa.op_use(add, 0, const_1);
        ssa.op_use(add, 1, const_1);
        ssa.set_entry_node(add);
        let m = grep!(&mut ssa, "(OpXor %1, %1)");
        assert_eq!(m[0].root.index(), 0);
        assert_eq!(m[0].bindings[0].0, "%1");
        assert_eq!(m[0].bindings[0].1.index(), 1);
    }

    #[test]
    fn simple_grep_replace1() {
        let mut ssa = SSAStorage::new();
        {
            let blk = ssa.insert_dynamic().expect("Cannot insert new dynamics");
            let vt = ValueInfo::new_unresolved(WidthSpec::from(64));
            let add = ssa.insert_op(MOpcode::OpAdd, vt, None)
                            .expect("Cannot insert new expressions");
            let const_1 = ssa.insert_const(1)
                                .expect("Cannot insert new constants");
            let const_2 = ssa.insert_const(2)
                                .expect("Cannot insert new constants");
            let addr = MAddress::new(0, 0);
            ssa.insert_into_block(add, blk, addr);
            ssa.insert_into_block(const_1, blk, addr);
            ssa.insert_into_block(const_2, blk, addr);
            ssa.op_use(add, 0, const_1);
            ssa.op_use(add, 1, const_2);
            ssa.set_entry_node(blk);
        }

        grep_and_replace!(&mut ssa, "(OpAdd %1, %2)" => "(OpSub %1, %2)");
        let sub_node = ssa.inorder_walk().last().expect("No last node!");
        let args = ssa.operands_of(sub_node);

        assert!({
            match ssa.g[sub_node] {
                NodeData::Op(MOpcode::OpSub, _) => true,
                _ => false,
            }
        });

        assert!({
            match ssa.g[args[0]] {
                NodeData::Op(MOpcode::OpConst(1), _) => true,
                _ => false,
            }
        });

        assert!({
            match ssa.g[args[1]] {
                NodeData::Op(MOpcode::OpConst(2), _) => true,
                _ => false,
            }
        });
    }
}
