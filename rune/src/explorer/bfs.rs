//! `PathExplorer` that works by exploring the CFG in Breadth First Order.
use std::collections::VecDeque;

use crate::explorer::explorer::{PathExplorer};
use crate::engine::rune::RuneControl;
use crate::context::context::{Context, RegisterRead};

#[derive(Clone, Copy, Debug, PartialEq)]
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
            branch: b
        }
    }
}

/// An explorer that traverses the program states in a Breadth First Order.
#[derive(Default)]
pub struct BFSExplorer<Ctx: Context> {
    /// Breadth First Queue
    queue: VecDeque<SavedState<Ctx>>,
}

impl<Ctx> PathExplorer for BFSExplorer<Ctx>
where Ctx: Context {
    type C = RuneControl;
    type Ctx = Ctx;

    fn new() -> BFSExplorer<Ctx> {
        BFSExplorer {
            queue: VecDeque::new(),
        }
    }

    fn next(&mut self, _: &mut Self::Ctx) -> RuneControl {
        // This function is present so that the path explorer can pre-empt the current path,
        // save the context and peform other heuristic analysis. But, since this is a pure BFS,
        // this function does nothing and asks rune to continue with its execution into the next
        // instruction.
        RuneControl::Continue
    }

    fn next_job(&mut self, ctx: &mut Self::Ctx) -> Option<RuneControl> {
        // When rune reaches the end of the current job and there are no more
        // instructions in the current execution path, then it calls next_job to request for the
        // next path in the queue. This function sets the correct context insformation by choosing
        // a path from the queue based on some decision making procedure, which in this case is a
        // breadth first search. If this function returns `None` it means that all states have been
        // completely explored and rune will halt.
        if let Some(ref state) = self.queue.pop_front() {
            *ctx = state.ctx.clone();
            Some(
                match state.branch {
                    BranchType::True => RuneControl::ExploreTrue,
                    BranchType::False => RuneControl::ExploreFalse,
                })
        } else {
            None
        }
    }

    // TODO: register_branch does not add constraints when adding true/false branch to the list
    fn register_branch(&mut self, ctx: &mut Self::Ctx, _condition: <Self::Ctx as RegisterRead>::VarRef) -> RuneControl {
        // When rune encounters a conditional branch instruction, it needs to decide which state to
        // explore next. To resolve this, it makes a call to the path explorer which decides on the
        // path to be explored next. Path explorer saves the current context information in order
        // to return to the path that is not taken in the future. It returns a `RuneControl` to
        // rune in order to direct its path of execution.

        self.queue.push_back(SavedState::new(ctx.clone(), BranchType::True));
        self.queue.push_back(SavedState::new(ctx.clone(), BranchType::False));

        // Switch to a new path in the BFS Queue and pre-empt the current instruction, forcing run
        // to load a new job from the queue.
        RuneControl::TerminatePath
    }
}
