extern crate arch;
extern crate r2pipe;
extern crate r2api;

use r2pipe::r2::R2;
use r2api::api_trait::R2Api;

use arch::regfile::x86regfile::*;

fn main() {
    let path = "/bin/ls";
    // Open a new r2 session
    let mut r2 = R2::new(Some(path)).expect("Failed to spawn r2");
    r2.init();

    // Get register information
    let reg_info = r2.reg_info().unwrap();

    // Create new X86RegisterFile with obtained LRegInfo
    let x86_reg_file = X86RegisterFile::new(&reg_info);
    println!("{:#?}", x86_reg_file);
}
