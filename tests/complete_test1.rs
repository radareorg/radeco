//! Complete test one. Tests various aspects of the decompiler against:
//!
//! ```asm
//!       0x004004e6      55             push rbp
//!       0x004004e7      4889e5         mov rbp, rsp
//!       0x004004ea      4883ec10       sub rsp, 0x10
//!       0x004004ee      48c745f8ffff.  mov qword [rbp - local_8h], -1
//!       0x004004f6      c745f4000000.  mov dword [rbp - local_ch], 0
//!       0x004004fd      b8ffffffff     mov eax, 0xffffffff
//!       0x00400502      483145f8       xor qword [rbp - local_8h], rax
//!       0x00400506      48837df80a     cmp qword [rbp - local_8h], 0xa
//!   ┌─< 0x0040050b      7e19           jle 0x400526
//!   │   0x0040050d      c745f4000000.  mov dword [rbp - local_ch], 0
//!  ┌──< 0x00400514      eb0a           jmp 0x400520
//! ┌───> 0x00400516      bfc4054000     mov edi, 0x4005c4
//! │││   0x0040051b      e8a0feffff     call sym.imp.puts
//! │└──> 0x00400520      837df418       cmp dword [rbp - local_ch], 0x18
//! └───< 0x00400524      7ef0           jle 0x400516
//!   └─> 0x00400526      bfc6054000     mov edi, 0x4005c6
//!       0x0040052b      e890feffff     call sym.imp.puts
//!       0x00400530      b800000000     mov eax, 0
//!       0x00400535      c9             leave
//!       0x00400536      c3             ret
//! ```
//!
//! Equivalent ESIL:
//!
//! ```
//!       0x004004e6      55             8,rsp,-=,rbp,rsp,=[8]
//!       0x004004e7      4889e5         rsp,rbp,=
//! 0x004004ea      4883ec10
//! 16,rsp,-=,$o,of,=,$s,sf,=,$z,zf,=,$p,pf,=,$b8,cf,=
//!       0x004004ee      48c745f8ffff.  -1,0x8,rbp,-,=[8]
//!       0x004004f6      c745f4000000.  0,0xc,rbp,-,=[4]
//!       0x004004fd      b8ffffffff     4294967295,rax,=
//! 0x00400502      483145f8
//! rax,0x8,rbp,-,^=[8],$z,zf,=,$p,pf,=,$s,sf,=,$0,cf,=,$0,of,=
//! 0x00400506      48837df80a
//! 10,0x8,rbp,-,[8],==,$z,zf,=,$b64,cf,=,$p,pf,=,$s,sf,=
//!   ┌─< 0x0040050b      7e19           of,sf,^,zf,|,?{,4195622,rip,=,}
//!   │   0x0040050d      c745f4000000.  0,0xc,rbp,-,=[4]
//!  ┌──< 0x00400514      eb0a           0x400520,rip,=
//! ┌───> 0x00400516      bfc4054000     4195780,rdi,=
//! │││   0x0040051b      e8a0feffff     rip,8,rsp,-=,rsp,=[],4195264,rip,=
//! │└──> 0x00400520      837df418
//! 24,0xc,rbp,-,[4],==,$z,zf,=,$b32,cf,=,$p,pf,=,$s,sf,=
//! └───< 0x00400524      7ef0           of,sf,^,zf,|,?{,4195606,rip,=,}
//!   └─> 0x00400526      bfc6054000     4195782,rdi,=
//!       0x0040052b      e890feffff     rip,8,rsp,-=,rsp,=[],4195264,rip,=
//!       0x00400530      b800000000     0,rax,=
//!       0x00400535      c9             rbp,rsp,=,rsp,[8],rbp,=,8,rsp,+=
//!       0x00400536      c3             rsp,[8],rip,=,8,rsp,+=
//! ```
//!

#[macro_use] extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;
extern crate serde_json;

use std::fs::File;
use std::io::prelude::*;

use r2api::structs::{LFunctionInfo, LRegInfo};

use radeco_lib::frontend::ssaconstructor::SSAConstruct;
use radeco_lib::middle::ssa::ssastorage::SSAStorage;
use radeco_lib::middle::ssa::verifier;
use radeco_lib::middle::ir_writer::IRWriter;
use radeco_lib::middle::{dce, dot};
use radeco_lib::analysis::sccp;
use radeco_lib::analysis::cse::cse::CSE;

use radeco_lib::analysis::matcher::gmatch;
use radeco_lib::backend::x86::x86_idioms;

const REGISTER_PROFILE: &'static str = "test_files/x86_register_profile.json";
const INSTRUCTIONS: &'static str = "test_files/ct1_instructions.json";

fn before_test(reg_profile: &mut LRegInfo, instructions: &mut LFunctionInfo, from: &str) {
    let mut register_profile = File::open(REGISTER_PROFILE).unwrap();
    let mut s = String::new();
    register_profile.read_to_string(&mut s).unwrap();
    *reg_profile = serde_json::from_str(&*s).unwrap();
    let mut instruction_file = File::open(from).unwrap();
    let mut s = String::new();
    instruction_file.read_to_string(&mut s).unwrap();
    *instructions = serde_json::from_str(&*s).unwrap();
}

fn run_construction() -> SSAStorage {
    let mut reg_profile = Default::default();
    let mut instructions = Default::default();
    before_test(&mut reg_profile, &mut instructions, INSTRUCTIONS);
    let mut ssa = SSAStorage::new();
    {
        let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
        constructor.run(instructions.ops.unwrap().as_slice());
    }
    {
        dce::collect(&mut ssa);
    }
    {
        let tmp = dot::emit_dot(&ssa);
        let mut f = File::create("ct1.dot").unwrap();
        f.write_all(tmp.as_bytes()).expect("Write failed!");
    }
    ssa
}

fn run_sccp(ssa: &mut SSAStorage) -> SSAStorage {
    let mut ssa = {
        let mut analyzer = sccp::Analyzer::new(ssa);
        analyzer.analyze();
        analyzer.emit_ssa()
    };
    {
        dce::collect(&mut ssa);
    }
    ssa
}

fn run_cse(ssa: &mut SSAStorage) -> SSAStorage {
    {
        let mut cse = CSE::new(ssa);
        cse.run();
    }
    ssa.clone()
}

#[test]
fn ct1_construction() {
    let ssa = run_construction();
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn ct1_sccp() {
    let ssa = {
        let mut ssa_ = run_construction();
        run_sccp(&mut ssa_)
    };
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn ct1_cse() {
    let ssa = {
        let mut ssa_ = run_construction();
        run_cse(&mut ssa_)
    };
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn ct1_cse_sccp() {
    let ssa = {
        let mut ssa_ = run_construction();
        {
            run_cse(&mut ssa_);
        }
        run_sccp(&mut ssa_)
    };
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn ct1_grep_replace() {
    let ssa = {
        let mut ssa_ = run_construction();
        {
            run_cse(&mut ssa_);
        }
        grep_and_replace!(&mut ssa_, "(OpSub (OpSub rsp, #x8), %1)" => "&var_C");
        grep_and_replace!(&mut ssa_, "(OpSub (OpSub rsp, #x8), #x8)" => "&var_8");
        grep_and_replace!(&mut ssa_, "(OpLoad (OpStore %1, &var_C, %3), &var_C)" => "%3");
        grep_and_replace!(&mut ssa_, "(OpLoad (OpStore %1, &var_8, %3), &var_8)" => "%3");
        grep_and_replace!(&mut ssa_, "(OpStore (OpStore %1, %2, %3), %2, %4)" => "(OpStore %1, %2, %4)");
        grep_and_replace!(&mut ssa_, "(OpStore mem, (OpSub rsp, #x8), rbp)" => "mem'");
        run_sccp(&mut ssa_)
    };
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn ct1_x86_idioms() {
    let mut ssa = run_construction();
    {
        x86_idioms::replace(&mut ssa);
    }
    {
        dce::collect(&mut ssa);
    }
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}
