#![allow(non_camel_case_types)]
#![recursion_limit="256"]

extern crate r2api;

#[macro_use] pub mod utils;

pub mod arch {
    pub mod arch;
    pub mod x86;
}

pub mod cc {
    pub mod calling_convention;
    pub mod cdecl;
}
    
pub mod regfile {
    pub mod regfile;
    pub mod x86regfile;
}
