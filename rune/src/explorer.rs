//! Defines traits that guides the symbolic emulator

use std::fmt::Debug;

use crate::context::{Context, RegisterRead};

pub mod bfs;
pub mod dfs;
pub mod directed;
pub mod interactive;

mod error;
pub use error::{ExplorerError, ExplorerResult};

/// Represents a collection of methods for exploring an instruction path.
pub trait PathExplorer {
    type C: Clone + Debug;
    type Ctx: Context;

    fn new() -> Self;
    fn next(&mut self, _: &mut Self::Ctx) -> Self::C;
    fn next_job(&mut self, _: &mut Self::Ctx) -> Option<Self::C>;

    fn register_branch(
        &mut self,
        _: &mut Self::Ctx,
        _: <Self::Ctx as RegisterRead>::VarRef,
    ) -> ExplorerResult<Self::C>;
}
