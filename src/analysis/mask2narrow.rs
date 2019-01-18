//! Module that implements translation from bit mask to MOpcode::OpNarrow

use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{SSAMod, ValueInfo, SSA};
use middle::ssa::ssastorage::SSAStorage;
use petgraph::graph::NodeIndex;

fn mask2narrow(ssa: &SSAStorage, expr: NodeIndex) -> Option<MOpcode> {
    let width = ssa
        .node_data(expr)
        .ok()
        .and_then(|nd| nd.vt.width().get_width());
    match (ssa.constant_value(expr), width) {
        (Some(0xffffffffffffffff), Some(64)) => Some(MOpcode::OpNarrow(64)),
        (Some(x), Some(_)) if (x + 1).count_ones() == 1 => {
            Some(MOpcode::OpNarrow((x + 1).trailing_zeros() as u16))
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
    let vt = {
        let w = match op {
            MOpcode::OpNarrow(w) => w,
            _ => panic!(),
        };
        let mut x = scalar!(w);
        let vty = ssa.node_data(expr).ok()?.vt.vty;
        x.vty = vty;
        x
    };
    let addr = ssa.address(expr)?;
    let blk = ssa.block_for(expr)?;
    let new_op = ssa.insert_op(op, vt, Some(addr.address))?;
    ssa.op_use(new_op, 0, n);
    ssa.replace_value(expr, new_op);
    ssa.insert_into_block(new_op, blk, addr);
    None
}
