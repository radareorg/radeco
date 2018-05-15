//! Defines transfer and propagate traits used for interprocess analysis.

use frontend::radeco_containers::RadecoModule;

pub trait InterProcAnalysis {
    fn new() -> Self;
    fn transfer(&mut self, &mut RadecoModule, u64);
    fn propagate(&mut self, &mut RadecoModule, u64);
}
