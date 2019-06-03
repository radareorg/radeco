//! `PathExplorer` that works by exploring the CFG in Depth First Order.

use std::collections::VecDeque;

use libsmt::theories::core;

use crate::explorer::explorer::PathExplorer;
use crate::engine::rune::RuneControl;
use crate::context::context::{Context, Evaluate, RegisterRead};
use crate::context::rune_ctx::RuneContext;
use crate::memory::qword_mem::QWordMemory;
use crate::regstore::regfile::RuneRegFile;

#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(dead_code)]
enum BranchType {
    True,
    False,
}

#[derive(Clone, Debug)]
struct SavedState<C: Context> {
    pub ctx: C,
    pub branch: BranchType,
}

impl<C: Context> SavedState<C> {
    fn new(ctx: C, b: BranchType) -> SavedState<C> {
        SavedState {
            ctx: ctx,
            branch: b,
        }
    }
}

/// An explorer that traverses the program states in a depth first order.
#[derive(Default)]
pub struct DFSExplorer<Ctx: Context> {
    /// Depth First Queue
    queue: VecDeque<SavedState<Ctx>>,
}

// TODO: [X] Add constraints for T/F branch
//       [ ] Check if the paths are feasible before enqueue
impl PathExplorer for DFSExplorer<RuneContext<QWordMemory, RuneRegFile>> {
    type C = RuneControl;
    type Ctx = RuneContext<QWordMemory, RuneRegFile>;

    fn new() -> DFSExplorer<Self::Ctx> {
        DFSExplorer { queue: VecDeque::new() }
    }

    // TODO: Terminate the current execution path if the depth is greater than a
    // preset threshold.
    fn next(&mut self, _: &mut Self::Ctx) -> RuneControl {
        RuneControl::Continue
    }

    // When rune finishes its execution, pop another unexplored path for it to
    // explore.
    fn next_job(&mut self, ctx: &mut Self::Ctx) -> Option<RuneControl> {
        if let Some(ref state) = self.queue.pop_back() {
            *ctx = state.ctx.clone();
            Some(match state.branch {
                BranchType::True => RuneControl::ExploreTrue,
                BranchType::False => RuneControl::ExploreFalse,
            })
        } else {
            None
        }
    }

    fn register_branch(&mut self,
                       ctx: &mut Self::Ctx,
                       condition: <Self::Ctx as RegisterRead>::VarRef)
                       -> RuneControl {
        // When a new branch is encountered, push the false branch into the queue and
        // explore the
        // true branch. Note that this choice is arbitrary and we could have as well
        // chosen the
        // other part without changing the nature of this explorer.
        let mut false_ctx = ctx.clone();
        {
            let zero = ctx.define_const(0, 1);
            false_ctx.eval(core::OpCodes::Cmp, &[condition, zero]);
        }
        self.queue.push_back(SavedState::new(false_ctx, BranchType::False));
        {
            let one = ctx.define_const(1, 1);
            ctx.eval(core::OpCodes::Cmp, &[condition, one]);
        }
        RuneControl::ExploreTrue
    }
}
