//! Combines sequences of arithmetic or logical instructions into single instructions.

use middle::ir::MOpcode;
use middle::ssa::ssa_traits::*;
use middle::ssa::ssastorage::SSAStorage;

use either::*;

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;

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
                if !ssa.is_constant(repl_node) && ssa.address(repl_node).is_none() {
                    ssa.insert_into_block(repl_node, blk, addr);
                }
            }
        }
    }

    /// Returns `Some(new_node)` if one of `cur_node`'s operands can be combined
    /// into `cur_node`, resulting in `new_node`; or, if `cur_node` could be
    /// simplified, resulting in `new_node`.
    /// Returns `None` if no simplification can occur.
    fn visit_node(&mut self, cur_node: SSAValue, ssa: &mut SSAStorage) -> Option<SSAValue> {
        // bail if non-combinable
        let extracted = extract_opinfo(cur_node, ssa)?;
        match extracted {
            Left((sub_node, cur_opinfo, cur_vt)) => {
                radeco_trace!(
                    "trying to combine ({:?} = {:?} {:?})",
                    cur_node,
                    sub_node,
                    cur_opinfo
                );
                let opt_new_node = self.make_combined_node(&cur_opinfo, cur_vt, sub_node, ssa);
                match opt_new_node {
                    Some(Left((new_node, new_sub_node, new_opinfo))) => {
                        radeco_trace!(
                            "  {:?} ==> ({:?} = {:?} {:?})",
                            cur_node,
                            new_node,
                            new_sub_node,
                            new_opinfo
                        );
                        self.combine_candidates
                            .insert(new_node, (new_sub_node, new_opinfo));
                        Some(new_node)
                    }
                    Some(Right(new_node)) => {
                        radeco_trace!("  {:?} ==> no-op", cur_node);
                        Some(new_node)
                    }
                    None => {
                        // no change; still add to `combine_candidates`
                        self.combine_candidates
                            .insert(cur_node, (sub_node, cur_opinfo));
                        None
                    }
                }
            }
            Right(c_val) => {
                // combined to constant
                radeco_trace!("{:?} = {:#x}", cur_node, c_val);
                let c_node = ssa.insert_const(c_val, None)?;
                Some(c_node)
            }
        }
    }

    /// Returns `Left(new_node, new_sub_node, new_opinfo)` if `cur_opinfo`
    /// combined with an operand or if `cur_opinfo` was simplified.
    /// Returns `Right(new_node)` if `cur_opinfo` canceled with an
    /// operand to make a no-op or was originally a no-op.
    /// Returns `None` if no combination or simplification exists.
    fn make_combined_node(
        &self,
        cur_opinfo: &CombinableOpInfo,
        cur_vt: ValueInfo,
        sub_node: SSAValue,
        ssa: &mut SSAStorage,
    ) -> Option<Either<(SSAValue, SSAValue, CombinableOpInfo), SSAValue>> {
        let (new_opinfo, new_sub_node) = self
            .combine_opinfo(cur_opinfo, sub_node)
            .map(|(oi, sn)| (Cow::Owned(oi), sn))
            .unwrap_or((Cow::Borrowed(cur_opinfo), sub_node));

        // simplify
        match simplify_opinfo(&new_opinfo) {
            Some(Some(simpl_new_opinfo)) => {
                radeco_trace!(
                    "    simplified ({:?}) into ({:?})",
                    new_opinfo,
                    simpl_new_opinfo
                );
                // make the new node
                let new_node =
                    make_opinfo_node(cur_vt, simpl_new_opinfo.clone(), new_sub_node, ssa)?;
                Some(Left((new_node, new_sub_node, simpl_new_opinfo)))
            }
            Some(None) => {
                radeco_trace!("    simplified ({:?}) into no-op", new_opinfo);
                Some(Right(new_sub_node))
            }
            None => {
                // no simplification
                match new_opinfo {
                    Cow::Borrowed(_) => None,
                    Cow::Owned(new_opinfo) => {
                        // combined, but no further simplification
                        let new_node =
                            make_opinfo_node(cur_vt, new_opinfo.clone(), new_sub_node, ssa)?;
                        Some(Left((new_node, new_sub_node, new_opinfo)))
                    }
                }
            }
        }
    }

    /// Tries to combine `sub_node` into `cur_opinfo`.
    /// Returns `None` if no combination exists.
    fn combine_opinfo(
        &self,
        cur_opinfo: &CombinableOpInfo,
        sub_node: SSAValue,
    ) -> Option<(CombinableOpInfo, SSAValue)> {
        let &(sub_sub_node, ref sub_opinfo) = self.combine_candidates.get(&sub_node)?;
        let new_opinfo = combine_rules::combine_opinfo(cur_opinfo, sub_opinfo)?;
        radeco_trace!(
            "    combined ({:?} {:?}) into ({:?})",
            sub_opinfo,
            cur_opinfo,
            new_opinfo
        );
        Some((new_opinfo, sub_sub_node))
    }
}

/// Creates an SSA node from the given `CombinableOpInfo`.
/// Returns `None` on SSA error.
fn make_opinfo_node(
    vt: ValueInfo,
    opinfo: CombinableOpInfo,
    sub_node: SSAValue,
    ssa: &mut SSAStorage,
) -> Option<SSAValue> {
    let ret = ssa.insert_op(opinfo.0, vt, None)?;
    match opinfo.1 {
        COCI::Unary => {
            ssa.op_use(ret, 0, sub_node);
        }
        COCI::Left(new_c) => {
            let new_cnode = ssa.insert_const(new_c, None)?;
            ssa.op_use(ret, 0, new_cnode);
            ssa.op_use(ret, 1, sub_node);
        }
        COCI::Right(new_c) => {
            let new_cnode = ssa.insert_const(new_c, None)?;
            ssa.op_use(ret, 0, sub_node);
            ssa.op_use(ret, 1, new_cnode);
        }
    };
    Some(ret)
}

/// Returns an equivalent `CombinableOpInfo`, but "simpler" in some sence.
/// Currently, this converts `OpAdd`s or `OpSub`s with "negative" constants into
/// equivalent operations with positive constants.
/// Returns `Some(None)` if `info` is a no-op.
/// Returns `None` if no simplification exists.
fn simplify_opinfo(info: &CombinableOpInfo) -> Option<Option<CombinableOpInfo>> {
    use self::CombinableOpConstInfo as COCI;
    use self::CombinableOpInfo as COI;
    use middle::ir::MOpcode::*;

    match info {
        COI(OpAdd, COCI::Left(0))
        | COI(OpAdd, COCI::Right(0))
        | COI(OpSub, COCI::Right(0))
        | COI(OpAnd, COCI::Left(0xFFFFFFFFFFFFFFFF))
        | COI(OpAnd, COCI::Right(0xFFFFFFFFFFFFFFFF))
        | COI(OpOr, COCI::Left(0))
        | COI(OpOr, COCI::Right(0))
        | COI(OpXor, COCI::Left(0))
        | COI(OpXor, COCI::Right(0)) => Some(None),
        COI(OpAdd, COCI::Left(c)) | COI(OpAdd, COCI::Right(c)) if *c > u64::max_value() / 2 => {
            let c = OpSub.eval_binop(0, *c).unwrap();
            Some(Some(COI(OpSub, COCI::Right(c))))
        }
        COI(OpSub, COCI::Right(c)) if *c > u64::max_value() / 2 => {
            let c = OpSub.eval_binop(0, *c).unwrap();
            Some(Some(COI(OpAdd, COCI::Left(c))))
        }
        _ => None,
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
