//! Defines transfer and propagate traits used for interprocess analysis.

use frontend::containers::{RModule, RFunction};

pub trait InterProcAnalysis<T: RModule> {
    fn new() -> Self;
    fn transfer(&mut self, &mut T, &T::FnRef);
    fn propagate(&mut self, &mut T, &T::FnRef);
}
