use r2api::structs::Endian;

use crate::arch::arch::*;
use crate::cc::calling_convention::*;
use crate::regfile::regfile::*;

/********************
 * x86 architecture *
 * ******************/

declare_architecture!(pub struct X86 {
});

register_architecture!(X86);

// Barebones architecture default for x86
impl<CC: CallingConvention, RF: RegisterFile> Default for X86<CC, RF> {
    fn default() -> X86<CC, RF> {
        X86 {
            name: String::from("x86"),
            endian: Endian::Little,
            bits: 32,
            int_size: 32,
            long_size: 32,
            calling_convention: None,
            regfile: None
        }
    }
}
