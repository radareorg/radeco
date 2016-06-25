//! Defines `Module` and `Function` that act as containers.

use std::fmt::Debug;

pub struct RadecoModule { }
pub struct RadecoFunction { }

pub trait RFunction { }
pub trait RModule {
    type FnRef: Copy + Clone + Debug;
}
