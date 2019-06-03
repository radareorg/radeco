//! Defines traits that need to be implemented for a source to be considered as
//! an `InstructionStream`.

use std::fmt::Debug;
use std::path;
use std::collections::HashMap;
use std::hash::Hash;
use std::io::prelude::*;
use std::fs::File;

use rustc_serialize::{Decodable};
use rustc_serialize::json;

use r2pipe::r2::R2;
use r2api::structs::LOpInfo;
use r2api::api_trait::R2Api;

pub trait InstructionStream {
    type Output: Debug + Clone;
    type Index: Debug + Clone;

    fn new() -> Self;
    fn at(&mut self, _: Self::Index) -> Option<Self::Output>;
}


impl InstructionStream for R2 {
    type Output = LOpInfo;
    type Index = u64;

    fn new() -> R2 {
        R2::new::<&str>(None).expect("Unable to open R2")
    }

    fn at(&mut self, addr: u64) -> Option<Self::Output> {
        let addr_ = format!("{}", addr);
        Some(self.insts(Some(1), Some(&addr_)).unwrap()[0].clone())
    }
}

// InstructionStream that reads and provides instructions from files.
// This is useful for tests, debug and other smaller applications.
// Maintains a HashMap from address to LOpInfo that it should provide
// when asked for that address.
#[derive(Clone, Debug, RustcDecodable, Default)]
pub struct FileStream<I, Op>
    where I: Debug + Clone + Decodable + Hash + PartialEq + Eq,
          Op: Debug + Clone + Decodable
{
    insts: HashMap<I, Op>,
}


impl<I, Op> FileStream<I, Op>
    where I: Debug + Clone + Decodable + Hash + PartialEq + Eq,
          Op: Debug + Clone + Decodable
{
    pub fn load<T: AsRef<path::Path>>(&mut self, fname: T) {
        let mut f = File::open(fname).expect("Failed to open file");
        let mut s = String::new();
        f.read_to_string(&mut s).expect("Failed to read from file");
        self.insts = json::decode(&s).expect("Failed to decode json");
    }
}

impl<I, Op> InstructionStream for FileStream<I, Op>
where I: Debug + Clone + Decodable + Hash + PartialEq + Eq,
      Op: Debug + Clone + Decodable {
    type Output = Op;
    type Index = I;

    fn new() -> FileStream<I, Op> {
        FileStream { insts: HashMap::new() }
    }

    fn at(&mut self, addr: I) -> Option<Op> {
        self.insts.get(&addr).cloned()
    }
}
