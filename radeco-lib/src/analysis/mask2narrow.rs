//! Module that implements translation from bit mask to MOpcode::OpNarrow

use crate::middle::ir::MOpcode;
use crate::middle::ssa::cfg_traits::CFG;
use crate::middle::ssa::ssa_traits::{SSAMod, ValueInfo, SSA};
use crate::middle::ssa::ssastorage::SSAStorage;
use petgraph::graph::NodeIndex;

fn mask2narrow(ssa: &SSAStorage, expr: NodeIndex) -> Option<MOpcode> {
    let width_opt = ssa
        .node_data(expr)
        .ok()
        .and_then(|nd| nd.vt.width().get_width());
    // The width returned by OpNarrow should be less than the width of operand.
    // In case the width will be the same size to the operand, it returns OpMov
    match (ssa.constant_value(expr), width_opt) {
        (Some(0xffffffffffffffff), Some(64)) => Some(MOpcode::OpMov),
        (Some(x), Some(w)) if (x + 1).count_ones() == 1 => {
            let n = (x + 1).trailing_zeros() as u16;
            if n == w {
                Some(MOpcode::OpMov)
            } else {
                Some(MOpcode::OpNarrow(n))
            }
        }
        _ => None,
    }
}

pub fn run(ssa: &mut SSAStorage) {
    let it = ssa
        .blocks()
        .into_iter()
        .flat_map(|b| ssa.exprs_in(b))
        .filter(|&e| match ssa.opcode(e) {
            Some(MOpcode::OpAnd) => true,
            _ => false,
        })
        .collect::<Vec<_>>();

    for node in it {
        let mut _ops = ssa.operands_of(node);
        let mut ops = _ops.iter().take(2).cloned();
        if let (Some(x), Some(y)) = (ops.next(), ops.next()) {
            visit_expr(ssa, node, x, y);
            visit_expr(ssa, node, y, x);
        }
    }
}

fn visit_expr(ssa: &mut SSAStorage, expr: NodeIndex, n: NodeIndex, mask: NodeIndex) -> Option<()> {
    let op = mask2narrow(ssa, mask)?;
    let vt = ssa.node_data(expr).ok()?.vt;
    let addr = ssa.address(expr)?;
    let blk = ssa.block_for(expr)?;
    match op {
        MOpcode::OpMov => {
            let new_op = ssa.insert_op(op, vt, Some(addr.address))?;
            ssa.op_use(new_op, 0, n);
            ssa.replace_value(expr, new_op);
            ssa.insert_into_block(new_op, blk, addr);
        }
        MOpcode::OpNarrow(w) => {
            let narrowed_op = {
                let mut x = scalar!(w);
                x.vty = vt.vty;
                ssa.insert_op(op, x, Some(addr.address))
            }?;
            let extended_op = {
                let www = vt
                    .width()
                    .get_width()
                    .expect("vt.width() should not be `None`");
                ssa.insert_op(MOpcode::OpZeroExt(www), vt, Some(addr.address))
            }?;
            ssa.op_use(narrowed_op, 0, n);
            ssa.op_use(extended_op, 0, narrowed_op);
            ssa.insert_into_block(narrowed_op, blk, addr);
            ssa.insert_into_block(extended_op, blk, addr);
            ssa.replace_value(expr, extended_op);
        }
        _ => unreachable!(),
    };
    None
}
