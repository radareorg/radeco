
use libsmt::backends::smtlib2::{SMTLib2};
use libsmt::logics::qf_abv;
use r2api::structs::Endian;

use std::fmt::Debug;


pub trait Memory: Clone + Debug {
    type VarRef;

    /// Create a new memory instance
    fn new(addr_width: usize, endian: Endian) -> Self;
    
    /// Initialize memory to be a new variable with the solver
    fn init_memory(&mut self, solver: &mut SMTLib2<qf_abv::QF_ABV>);

    /// Read x bytes of memory at a certain location
    fn read(&mut self, addr: Self::VarRef, read_size: usize, solver: &mut SMTLib2<qf_abv::QF_ABV>) -> Self::VarRef;

    /// Write x bytes of memory at a certain location
    fn write(&mut self, addr: Self::VarRef, data: Self::VarRef, write_size: usize, solver: &mut SMTLib2<qf_abv::QF_ABV>);
}

