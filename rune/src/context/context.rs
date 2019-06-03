//!  Defines `Context` trait to be used by symbolic emulator

use std::fmt::Debug;
use std::hash::Hash;

use std::collections::HashMap;

use libsmt::backends::smtlib2::SMTProc;

pub trait Context: Clone + Debug
                   + RegisterRead
                   + RegisterWrite<VarRef=<Self as RegisterRead>::VarRef>
                   + Evaluate<VarRef=<Self as RegisterRead>::VarRef>
                   + MemoryRead<VarRef=<Self as RegisterRead>::VarRef>
                   + MemoryWrite<VarRef=<Self as RegisterRead>::VarRef>
{
    fn ip(&self) -> u64;
    fn is_symbolic(&self) -> bool {
        true
    }
    fn is_concrete(&self) -> bool {
        !self.is_symbolic()
    }
    fn increment_ip(&mut self, _: u64);
    fn set_ip(&mut self, _: u64);
    fn define_const(&mut self, _: u64, _: usize) -> <Self as RegisterRead>::VarRef;
    fn alias_of(&self, _: String) -> Option<String>;
    fn e_old(&self) -> <Self as RegisterRead>::VarRef;
    fn e_cur(&self) -> <Self as RegisterRead>::VarRef;

    fn solve<S: SMTProc>(&mut self, _: &mut S) -> HashMap<<Self as RegisterRead>::VarRef, u64>;

    fn var_named<T: AsRef<str>>(& self, _: T) -> Option<<Self as RegisterRead>::VarRef>;
    fn set_e_old(&mut self, _: <Self as RegisterRead>::VarRef);
    fn set_e_cur(&mut self, _: <Self as RegisterRead>::VarRef);
}

pub trait MemoryRead: Sized {
    type VarRef: Clone + Debug + Hash + Eq;
    fn mem_read(&mut self, _: Self::VarRef, _: usize) -> Self::VarRef;
}

pub trait MemoryWrite: Sized {
    type VarRef: Clone + Debug + Hash + Eq;
    fn mem_write(&mut self, _: Self::VarRef, _: Self::VarRef, _: usize);
}

pub trait RegisterRead: Sized {
    type VarRef: Clone + Debug + Hash + Eq;
    fn reg_read<T: AsRef<str>>(&mut self, _: T) -> Self::VarRef;
}

pub trait RegisterWrite: Sized {
    type VarRef: Clone + Debug + Hash + Eq;
    fn reg_write<T: AsRef<str>>(&mut self, _: T, _: Self::VarRef);
}

pub trait Evaluate {
    type VarRef: Clone + Debug + Hash + Eq;
    type IFn: Clone + Debug;

    fn eval<T, Q>(&mut self, _: T, _: Q) -> Self::VarRef
        where T: Into<Self::IFn>,
              Q: AsRef<[Self::VarRef]>;
}

/// Optional trait intended to boost usability of `Context`
/// NOTE: All functions that are descibed in `ContextAPI` must be composed of one or more functions
/// from `Context`. That is, there must be no loss of functionality by not implementing
/// `ContextAPI`.
pub trait ContextAPI: Context {
    /// Set register to hold either symbolic or concrete values.
    fn set_reg_as_const<T: AsRef<str>>(&mut self, _: T, _: u64) -> <Self as RegisterRead>::VarRef;
    fn set_reg_as_sym<T: AsRef<str>>(&mut self, _: T) -> <Self as RegisterRead>::VarRef;

    /// Set memory to hold either symbolic or concrete values.
    fn set_mem_as_const(&mut self, _: u64, _: u64, _: usize) -> <Self as RegisterRead>::VarRef;
    fn set_mem_as_sym(&mut self, _: u64, _: usize) -> <Self as RegisterRead>::VarRef;

    /// Set registers that are not set to be a constant zero.
    fn zero_registers(&mut self);

    /// An iterator over registers.
    fn registers(&self) -> Vec<String>;
}
