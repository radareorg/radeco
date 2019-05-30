//! Defines transfer and propagate traits used for interprocess analysis.

use crate::frontend::radeco_containers::RadecoModule;

pub trait InterProcAnalysis {
    fn new() -> Self;
    fn transfer(&mut self, _: &mut RadecoModule, _: u64);
    fn propagate(&mut self, _: &mut RadecoModule, _: u64);
}
