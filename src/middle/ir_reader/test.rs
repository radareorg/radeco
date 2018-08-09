use middle::ir_writer;
use middle::regfile::SubRegisterFile;
use serde_json;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::sync::Arc;

#[cfg_attr(rustfmt, rustfmt_skip)]
const SSA_TXT: &str = "\
define-fun sym.foo(unknown) -> unknown {
    entry-register-state:
        %1: $Unknown64(*?) = $r15;
        %2: $Unknown64(*?) = $r14;
        %3: $Unknown64(*?) = $r13;
        %4: $Unknown64(*?) = $r12;
        %5: $Unknown64(*?) = $rbp;
        %6: $Unknown64(*?) = $rbx;
        %7: $Unknown64(*?) = $r11;
        %8: $Unknown64(*?) = $r10;
        %9: $Unknown64(*?) = $r9;
        %10: $Unknown64(*?) = $r8;
        %11: $Unknown64(*?) = $rcx;
        %12: $Unknown64(*?) = $rdx;
        %13: $Unknown64(*?) = $rsi;
        %14: $Unknown64(*?) = $rdi;
        %15: $Unknown64(*?) = $rip;
        %16: $Unknown64(*?) = $cs;
        %17: $Unknown1(*?) = $cf;
        %18: $Unknown1(*?) = $pf;
        %19: $Unknown1(*?) = $af;
        %20: $Unknown1(*?) = $zf;
        %21: $Unknown1(*?) = $sf;
        %22: $Unknown1(*?) = $tf;
        %23: $Unknown1(*?) = $if;
        %24: $Unknown1(*?) = $df;
        %25: $Unknown1(*?) = $of;
        %26: $Unknown64(*?) = $rsp;
        %27: $Unknown64(*?) = $ss;
        %28: $Unknown64(*?) = $fs_base;
        %29: $Unknown64(*?) = $gs_base;
        %30: $Unknown64(*?) = $ds;
        %31: $Unknown64(*?) = $es;
        %32: $Unknown64(*?) = $fs;
        %33: $Unknown64(*?) = $gs;
        %34: $Unknown0 = $mem;
    bb_0x000610.0000(sz 0x0):
        [@0x000610.0001] %35: $Unknown64(*?) = #x1 + %14;
        RETURN
    exit-node:
    final-register-state:
        $r15 = %1;
        $r14 = %2;
        $r13 = %3;
        $r12 = %4;
        $rbp = %5;
        $rbx = %6;
        $r11 = %7;
        $r10 = %8;
        $r9 = %9;
        $r8 = %10;
        $rax = %35;
        $rcx = %11;
        $rdx = %12;
        $rsi = %13;
        $rdi = %14;
        $rip = %15;
        $cs = %16;
        $cf = %17;
        $pf = %18;
        $af = %19;
        $zf = %20;
        $sf = %21;
        $tf = %22;
        $if = %23;
        $df = %24;
        $of = %25;
        $rsp = %26;
        $ss = %27;
        $fs_base = %28;
        $gs_base = %29;
        $ds = %30;
        $es = %31;
        $fs = %32;
        $gs = %33;
        $mem = %34;
}
";
const REGISTER_PROFILE: &'static str = "test_files/x86_register_profile.json";

lazy_static! {
    static ref REGISTER_FILE: Arc<SubRegisterFile> = {
        let s = ::std::fs::read_to_string(REGISTER_PROFILE).unwrap();
        let reg_profile = serde_json::from_str(&*s).unwrap();
        Arc::new(SubRegisterFile::new(&reg_profile))
    };
}

#[test]
fn check_ssa_parse() {
    use middle::ir::MOpcode::*;
    use middle::ir::WidthSpec;
    use middle::regfile::RegisterId;
    use middle::ssa::cfg_traits::*;
    use middle::ssa::ssa_traits::*;
    use middle::ssa::ssastorage::NodeData;
    use middle::ssa::utils;
    use std::collections::HashSet;

    let ssa = super::parse_il(SSA_TXT, REGISTER_FILE.clone());

    let entry = ssa.entry_node().unwrap();
    let ers_node = ssa.registers_in(entry).unwrap();
    let ers = utils::register_state_info(ers_node, &ssa);
    let succs = ssa.succs_of(entry);
    assert_eq!(succs.len(), 1);
    let real_entry = succs[0];
    let exprs = ssa.exprs_in(real_entry).into_iter().collect::<HashSet<_>>();
    let exit = ssa.exit_node().unwrap();
    let frs_node = ssa.registers_in(exit).unwrap();
    let frs = utils::register_state_info(frs_node, &ssa);

    let rax_i = ssa.regfile.register_id_by_name("rax").unwrap().to_usize();
    let v1 = frs[RegisterId::from_usize(rax_i)].0;
    assert!(exprs.contains(&v1));
    assert_eq!(
        ssa.g[v1],
        NodeData::Op(OpAdd, ValueInfo::new_unresolved(WidthSpec::new_known(64)))
    );
    let v1_ops = ssa.operands_of(v1);
    assert_eq!(v1_ops.len(), 2);
    let x1 = v1_ops[0];
    assert_eq!(
        ssa.g[x1],
        NodeData::Op(OpConst(1), ValueInfo::new_scalar(WidthSpec::new_known(64)))
    );
    let cmt_rdi = v1_ops[1];
    assert_eq!(
        ssa.g[cmt_rdi],
        NodeData::Comment(
            ValueInfo::new_unresolved(WidthSpec::new_known(64)),
            "rdi".to_owned()
        )
    );
    let rdi_i = ssa.regfile.register_id_by_name("rdi").unwrap().to_usize();
    assert_eq!(cmt_rdi, ers[RegisterId::from_usize(rdi_i)].0);
}

#[test]
fn roundtrip_basic_ssa() {
    roundtrip("sym.foo".to_owned(), SSA_TXT);
}

#[test]
fn roundtrip_bin1_main() {
    roundtrip_file("main".to_owned(), "test_files/bin1_main_ssa");
}

#[test]
fn roundtrip_loopy_main() {
    roundtrip_file("sym.main".to_owned(), "test_files/loopy_main_ssa");
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
    let parsed = super::parse_il(ssa_txt, REGISTER_FILE.clone());
    let mut emitted = String::new();
    ir_writer::emit_il(&mut emitted, Some(fn_name), &parsed).unwrap();
    assert_eq!(ssa_txt, emitted);
}
