//! Combines sequences of arithmetic or logical instructions into single instructions.

use middle::ir::MOpcode;
use middle::ssa::ssa_traits::*;
use middle::ssa::ssastorage::SSAStorage;

use either::*;

use std::collections::HashMap;
use std::fmt;
use std::num::Wrapping;

mod combine_rules;

type SSAValue = <SSAStorage as SSA>::ValueRef;

/// For every instruction, try to combine one of its operands into itself. This
/// transforms linear data-dependency chains into trees.
pub fn run(ssa: &mut SSAStorage) -> () {
    Combiner::new().run(ssa);
}

/// Represents binary operations that are effectively unary because one of the
/// operands is constant. In other words, represents curried binary operations.
/// e.g. `(|x| 7 & x)`, `(|x| x + 3)`, etc.
#[derive(Clone)]
struct CombinableOpInfo(MOpcode, CombinableOpConstInfo);
#[derive(Clone, Debug)]
enum CombinableOpConstInfo {
    /// no const, like in `(|x| !x)`
    Unary,
    /// const is on the left, like in `(|x| 3 - x)`
    Left(u64),
    /// const is on the right, like in `(|x| x - 3)`
    Right(u64),
}

use self::CombinableOpConstInfo as COCI;

#[derive(Debug)]
struct Combiner {
    /// Nodes that could potentially be combined into another
    combine_candidates: HashMap<SSAValue, (SSAValue, CombinableOpInfo)>,
}

impl Combiner {
    fn new() -> Self {
        Combiner {
            combine_candidates: HashMap::new(),
        }
    }

    /// Calls `visit_node` on every node and replaces the node in the SSA with
    /// its return value.
    fn run(&mut self, ssa: &mut SSAStorage) -> () {
        for node in ssa.inorder_walk() {
            if let Some(repl_node) = self.visit_node(node, ssa) {
                let blk = ssa.block_for(node).unwrap();
                let addr = ssa.address(node).unwrap();
                ssa.replace_value(node, repl_node);
                if !ssa.is_constant(repl_node) {
                    ssa.insert_into_block(repl_node, blk, addr);
                }
            }
        }
    }

    /// Returns `Some(new_node)` if one of `cur_node`'s operands can be combined
    /// into `cur_node`, resulting in `new_node`.
    /// Returns `None` if no simplification can occur.
    fn visit_node(&mut self, cur_node: SSAValue, ssa: &mut SSAStorage) -> Option<SSAValue> {
        // bail if non-combinable
        let extracted = extract_opinfo(cur_node, ssa)?;
        match extracted {
            Left((sub_node, cur_opinfo, cur_vt)) => {
                let opt_new_node = self.make_combined_node(&cur_opinfo, cur_vt, sub_node, ssa);
                if let Some((new_node, new_sub_node, new_opinfo)) = opt_new_node {
                    radeco_trace!(
                        "combined ({:?} = {:?} {:?} {:?}) into ({:?} = {:?} {:?})",
                        cur_node,
                        new_sub_node,
                        self.combine_candidates[&sub_node].1,
                        cur_opinfo,
                        new_node,
                        new_sub_node,
                        new_opinfo
                    );
                    self.combine_candidates
                        .insert(new_node, (new_sub_node, new_opinfo));
                    Some(new_node)
                } else {
                    self.combine_candidates
                        .insert(cur_node, (sub_node, cur_opinfo));
                    None
                }
            }
            Right(c_val) => {
                let c_node = ssa.insert_const(c_val)?;
                Some(c_node)
            }
        }
    }

    /// Returns (the node that was just created, its operand, its opinfo)
    fn make_combined_node(
        &self,
        cur_opinfo: &CombinableOpInfo,
        cur_vt: ValueInfo,
        sub_node: SSAValue,
        ssa: &mut SSAStorage,
    ) -> Option<(SSAValue, SSAValue, CombinableOpInfo)> {
        // bail if the operand isn't combinable
        let (sub_sub_node, sub_opinfo) = self.combine_candidates.get(&sub_node)?;
        // bail if no combination exists
        let new_opinfo = combine_rules::combine_opinfo(cur_opinfo, sub_opinfo)?;

        // make the new node
        let CombinableOpInfo(new_opcode, new_coci) = new_opinfo.clone();
        let new_node = ssa.insert_op(new_opcode, cur_vt, None)?;
        match new_coci {
            COCI::Unary => {
                ssa.op_use(new_node, 0, sub_node);
            }
            COCI::Left(new_c) => {
                let new_cnode = ssa.insert_const(new_c)?;
                ssa.op_use(new_node, 0, new_cnode);
                ssa.op_use(new_node, 1, *sub_sub_node);
            }
            COCI::Right(new_c) => {
                let new_cnode = ssa.insert_const(new_c)?;
                ssa.op_use(new_node, 0, *sub_sub_node);
                ssa.op_use(new_node, 1, new_cnode);
            }
        };
        Some((new_node, *sub_sub_node, new_opinfo))
    }
}

/// Returns `Some(Left(_))` if `cur_node` has exactly one non-const operand.
/// Returns `Some(Right(_))` if `cur_node` has all const operands.
/// Returns `None` if `cur_node` is non-combinable.
fn extract_opinfo(
    cur_node: SSAValue,
    ssa: &SSAStorage,
) -> Option<Either<(SSAValue, CombinableOpInfo, ValueInfo), u64>> {
    // bail if non-`NodeType::Op`
    let (cur_opcode, cur_vt) = extract_opcode(cur_node, ssa)?;
    let cur_operands = ssa.operands_of(cur_node);
    match cur_operands.as_slice() {
        &[sub_node] => {
            let cur_opinfo = CombinableOpInfo(cur_opcode, COCI::Unary);
            Some(Left((sub_node, cur_opinfo, cur_vt)))
        }
        &[sub_node1, sub_node2] => {
            match (ssa.constant(sub_node1), ssa.constant(sub_node2)) {
                (Some(c1), Some(c2)) => {
                    // this is const_prop's job, but we can do this here too
                    // bail if `cur_opcode` is non-evalable, since that also
                    // implies it's non-combinable
                    let res_val = cur_opcode.eval_binop(c1, c2)?;
                    Some(Right(res_val))
                }
                (None, Some(c)) => {
                    let cur_opinfo = CombinableOpInfo(cur_opcode, COCI::Right(c));
                    Some(Left((sub_node1, cur_opinfo, cur_vt)))
                }
                (Some(c), None) => {
                    let cur_opinfo = CombinableOpInfo(cur_opcode, COCI::Left(c));
                    Some(Left((sub_node2, cur_opinfo, cur_vt)))
                }
                (None, None) => None,
            }
        }
        _ => None,
    }
}

fn extract_opcode(node: SSAValue, ssa: &SSAStorage) -> Option<(MOpcode, ValueInfo)> {
    if let NodeData {
        vt,
        nt: NodeType::Op(opcode),
    } = ssa.node_data(node).ok()?
    {
        Some((opcode, vt))
    } else {
        None
    }
}

impl fmt::Debug for CombinableOpInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.1 {
            COCI::Unary => fmt.write_fmt(format_args!("-> ({:?} .)", self.0)),
            COCI::Left(c) => fmt.write_fmt(format_args!("-> ({:#x} {:?} .)", c, self.0)),
            COCI::Right(c) => fmt.write_fmt(format_args!("-> (. {:?} {:#x})", self.0, c)),
        }
    }
}
