//! Parses textual IL as emited by [`ir_writer`](::middle::ir_writer).

mod parser_util;
mod parser;
mod lowering;
mod simple_ast;

use middle::ssa::ssastorage::SSAStorage;

/// Parses textual IL as emited by [`ir_writer`](::middle::ir_writer).
/// The returned SSA is empty if an error occured.
pub fn parse_il<T: AsRef<str>>(il: T) -> SSAStorage {
    let mut ret = SSAStorage::new();
    match parser::FunctionParser::new().parse(il.as_ref()) {
        Ok(sast) => lowering::lower_simpleast(&mut ret, sast)
            .unwrap_or_else(|e| radeco_err!("Error lowering AST to SSA: {:?}", e)),
        Err(s) => radeco_err!("Error parsing IL: {}", s),
    }
    ret
}

#[cfg(test)]
mod test {
    use middle::ir_writer::IRWriter;
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;

    const SSA_TXT: &str = r#"
define-fun main(unknown) -> unknown {
    registers: $r15,$r14,$r13,$r12,$rbp,$rbx,$r11,$r10,$r9,$r8,$rax,$rcx,$rdx,$rsi,$rdi,$rip,$cs,$cf,$pf,$af,$zf,$sf,$tf,$if,$df,$of,$rsp,$ss,$fs_base,$gs_base,$ds,$es,$fs,$gs;
    bb_0x0005FA.0000():
        [@0x0005FA.0001] %1: $Unknown64(*?) = {rsi} + #x1;
    exit-node:
    final-register-state:
        $r15 = {r15};
        $r14 = {r14};
        $r13 = {r13};
        $r12 = {r12};
        $rbp = {rbp};
        $rbx = {rbx};
        $r11 = {r11};
        $r10 = {r10};
        $r9 = {r9};
        $r8 = {r8};
        $rax = %1;
        $rcx = {rcx};
        $rdx = {rdx};
        $rsi = {rsi};
        $rdi = {rdi};
        $rip = {rip};
        $cs = {cs};
        $cf = {cf};
        $pf = {pf};
        $af = {af};
        $zf = {zf};
        $sf = {sf};
        $tf = {tf};
        $if = {if};
        $df = {df};
        $of = {of};
        $rsp = {rsp};
        $ss = {ss};
        $fs_base = {fs_base};
        $gs_base = {gs_base};
        $ds = {ds};
        $es = {es};
        $fs = {fs};
        $gs = {gs};
        $mem = {mem};
}"#;

    #[test]
    fn check_ssa_parse() {
        use middle::ir::MOpcode::*;
        use middle::ir::WidthSpec;
        use middle::ssa::cfg_traits::*;
        use middle::ssa::ssa_traits::*;
        use middle::ssa::ssastorage::NodeData;
        use middle::ssa::ssastorage::SSAStorage;
        use std::collections::HashSet;

        let ssa = super::parse_il(SSA_TXT);

        let entry = ssa.entry_node().unwrap();
        let succs = ssa.succs_of(entry);
        assert_eq!(succs.len(), 1);
        let real_entry = succs[0];
        let exprs = ssa.exprs_in(real_entry).into_iter().collect::<HashSet<_>>();
        let exit = ssa.exit_node().unwrap();
        let frs_node = ssa.registers_in(exit).unwrap();
        let frs = ssa.operands_of(frs_node);

        let rbp_i = ssa.regnames.iter().position(|r| r == "rax").unwrap();
        let v1 = frs[rbp_i];
        assert!(exprs.contains(&v1));
        assert_eq!(
            ssa.g[v1],
            NodeData::Op(OpAdd, ValueInfo::new_unresolved(WidthSpec::new_known(64)))
        );
        let v1_ops = ssa.operands_of(v1);
        assert_eq!(v1_ops.len(), 2);
        let cmt_rsi = v1_ops[0];
        assert_eq!(
            ssa.g[cmt_rsi],
            NodeData::Comment(
                ValueInfo::new_unresolved(WidthSpec::Unknown),
                "rsi".to_owned()
            )
        );
        let x1 = v1_ops[1];
        assert_eq!(
            ssa.g[x1],
            NodeData::Op(OpConst(1), ValueInfo::new_scalar(WidthSpec::new_known(64)))
        );
    }

    #[test]
    fn roundtrip_basic_ssa() {
        roundtrip("main".to_owned(), SSA_TXT);
    }

    #[test]
    fn roundtrip_bin1_main() {
        roundtrip_file("main".to_owned(), "test_files/bin1_main_ssa");
    }

    fn roundtrip_file<P: AsRef<Path>>(fn_name: String, file_path: P) {
        let ssa_txt = {
            let mut ssa_txt_file = File::open(file_path).expect("Error opening file");
            let mut contents = String::new();
            ssa_txt_file
                .read_to_string(&mut contents)
                .expect("Error reading file");
            contents
        };
        roundtrip(fn_name, &ssa_txt);
    }

    fn roundtrip(fn_name: String, ssa_txt: &str) {
        let parsed = super::parse_il(ssa_txt);
        let emited = IRWriter::default().emit_il(Some(fn_name), &parsed);
        assert_eq!(ssa_txt, emited);
    }
}
