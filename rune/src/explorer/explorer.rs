//! Defines traits that guides the symbolic emulator

use crate::context::context::Context;
use std::fmt::Debug;

use crate::context::context::RegisterRead;

pub trait PathExplorer {
    type C: Clone + Debug;
    type Ctx: Context;

    fn new() -> Self;
    fn next(&mut self, _: &mut Self::Ctx) -> Self::C;
    fn next_job(&mut self, _: &mut Self::Ctx) -> Option<Self::C>;

    fn register_branch(&mut self, _: &mut Self::Ctx, _: <Self::Ctx as RegisterRead>::VarRef) -> Self::C;
}
