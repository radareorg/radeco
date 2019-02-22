use std::any::Any;
use std::fmt::Debug;

use petgraph::graph::NodeIndex;

use frontend::radeco_containers::{RadecoFunction, RadecoModule};

/// This trait provides access to extra informations generated during the analysis pass.
pub trait AnalyzerResult : Any + Debug {
    fn as_any(&self) -> &dyn Any;
}

/// Kind of `Analyzer`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnalyzerKind {
    CallSiteFixer,
    Combiner,
    CopyPropagation,
    CSE,
    DCE,
    Inferer,
    InterProc,
    SCCP,
}

/// Basic trait for all the analyzers.
pub trait Analyzer : Any + Debug {
    /// Returns the name of this `Analyzer`.
    fn name(&self) -> String;

    /// Returns the kind of this `Analyzer`.
    fn kind(&self) -> AnalyzerKind;

    /// Returns a list of `Analyzer`s to run before this one.
    fn requires(&self) -> Vec<AnalyzerKind>;

    /// Returns `true` if this `Analyzer` uses a policy function to decide whether to apply or not
    /// `Changes` to the IR. In this case a policy function must be provided when calling `analyze`.
    /// Returns `false` if this `Analyzer` does not use a policy function and changes directly the
    /// IR. Any policy function passed to `analyze` is ignored by this kind of analyzers.
    ///
    /// The former is the type of `Analyzer`s which (typically) produce one single atomic `Change`
    /// at time, e.g. `DCE` removes useless expressions one by one. Interrupting their work in the
    /// middle will still deliver some useful output.
    ///
    /// The latter type of `Analyzer`s are module-oriented, meaning that they work on the entire
    /// function/module at the same time and it does not make sense to split their work into smaller
    /// atomic `Change`s.
    fn uses_policy(&self) -> bool;

    fn as_any(&self) -> &dyn Any;
}

/// An atomic change to the IR.
///
/// It represents a high-level, `Analyzer` specific change to apply to the IR.
pub trait Change : Any + Debug {
    fn as_any(&self) -> &dyn Any;
}

/// A `Change` which replaces a node with another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ReplaceValue(pub NodeIndex, pub NodeIndex);
impl Change for ReplaceValue {
    fn as_any(&self) -> &dyn Any { self }
}

/// A `Change` which removes a node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RemoveValue(pub NodeIndex);
impl Change for RemoveValue {
    fn as_any(&self) -> &dyn Any { self }
}

/// An `Action` to take with respect to a `Change`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Action {
    Apply,
    Skip,
    Abort,
}

/// Short-hand policy to apply all the `Change`s.
pub fn all(_change: Box<Change>) -> Action {
    Action::Apply
}

/// Short-hand policy to skip all the `Changes`s.
pub fn none(_change: Box<Change>) -> Action {
    Action::Skip
}

/// An `Analyzer` that takes a function.
pub trait FuncAnalyzer : Analyzer {
    /// Look for possbile `Change`s to apply to `func`. When one is found `policy` is called with
    /// that `Change` as parameter; then according to the return value it is applied or discarded.
    ///
    /// As `Change`s are applied, the IR is not the same as before, thus `Change`s previously
    /// discarded could be proposed again by the `Analyer`. On the other hand an `Analyzer` is
    /// not expected to propose again a `Change` if all the previous other `Change`s were skipped.
    fn analyze<T: Fn(Box<Change>) -> Action>(&mut self, func: &mut RadecoFunction, policy: Option<T>) -> Option<Box<AnalyzerResult>>;
}

/// An `Analyzer` that takes a module.
pub trait ModuleAnalyzer : Analyzer {
    /// Look for possbile `Change`s to apply to `mod`. When one is found `policy` is called with
    /// that `Change` as parameter; then according to the return value it is applied or discarded.
    ///
    /// As `Change`s are applied, the IR is not the same as before, thus `Change`s previously
    /// discarded could be proposed again by the `Analyer`. On the other hand an `Analyzer` is
    /// not expected to propose again a `Change` if all the previous other `Change`s were skipped.
    fn analyze<T: Fn(Box<Change>) -> Action>(&mut self, module: &mut RadecoModule, policy: Option<T>) -> Option<Box<AnalyzerResult>>;
}

/// Get all the available `FuncAnalyzer`s
pub fn all_func_analysis() -> Vec<AnalyzerKind> {
    vec![AnalyzerKind::Combiner,
         AnalyzerKind::CopyPropagation,
         AnalyzerKind::CSE,
         AnalyzerKind::DCE,
         AnalyzerKind::SCCP]
}

/// Get all the available `ModuleAnalyzer`s
pub fn all_module_analysis() -> Vec<AnalyzerKind> {
    vec![AnalyzerKind::CallSiteFixer,
         AnalyzerKind::Inferer,
         AnalyzerKind::InterProc]
}
