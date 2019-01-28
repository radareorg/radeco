use std::any::Any;
use std::fmt::Debug;

use frontend::radeco_containers::{RadecoFunction, RadecoModule};

/// This trait provides access to extra informations generated during the analysis pass.
pub trait AnalyzerResult : Any + Debug { }

/// Kind of `Analyzer`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnalyzerKind {
    CallSiteFixer,
    CopyPropagation,
    CSE,
    Inferer,
}

/// Basic trait for all analyzers.
pub trait Analyzer : Any + Debug {
    /// Return the name of this `Analyzer`.
    fn name(&self) -> String;

    /// Return the kind of this `Analyzer`.
    fn kind(&self) -> AnalyzerKind;

    /// Return a list of `Analyzer`s to run before this one.
    fn requires(&self) -> Vec<AnalyzerKind>;
}

/// An `Analyzer` that takes a function.
pub trait FuncAnalyzer : Analyzer {
    fn analyze(&mut self, func: &mut RadecoFunction) -> Option<Box<AnalyzerResult>>;
}

/// An `Analyzer` that takes a module.
pub trait ModuleAnalyzer : Analyzer {
    fn analyze(&mut self, module: &mut RadecoModule) -> Option<Box<AnalyzerResult>>;
}

/// Get all the available `FuncAnalyzer`s
pub fn all_func_analysis() -> Vec<AnalyzerKind> {
    vec![AnalyzerKind::CopyPropagation,
         AnalyzerKind::CSE]
}

/// Get all the available `ModuleAnalyzer`s
pub fn all_module_analysis() -> Vec<AnalyzerKind> {
    vec![AnalyzerKind::CallSiteFixer,
         AnalyzerKind::Inferer]
}
