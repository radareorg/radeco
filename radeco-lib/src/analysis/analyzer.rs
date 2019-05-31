use std::any::Any;
use std::convert::From;
use std::fmt::Debug;

use petgraph::graph::NodeIndex;

use crate::analysis::cse::cse;
use crate::analysis::functions::{fix_ssa_opcalls, infer_regusage};
use crate::analysis::interproc::interproc;
use crate::analysis::{arithmetic, copy_propagation, dce, inst_combine, sccp};
use crate::frontend::radeco_containers::{RadecoFunction, RadecoModule};

/// This trait provides access to extra informations generated during the analysis pass.
pub trait AnalyzerResult: Any + Debug {
    fn as_any(&self) -> &dyn Any;
}

/// Kind of `Analyzer`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnalyzerKind {
    Arithmetic,
    CallSiteFixer,
    Combiner,
    CopyPropagation,
    CSE,
    DCE,
    Inferer,
    InterProc,
    SCCP,
}

/// A struct providing information about an analyzer.
#[derive(Debug)]
pub struct AnalyzerInfo {
    /// The name of this `Analyzer`.
    pub name: &'static str,

    /// The type of this `Analyzer`.
    pub kind: AnalyzerKind,

    /// A list of `Analyzer`s that should be run before running this one.
    ///
    /// # Note
    /// These analyzers are **not guaranteed** to be called before this one.
    pub requires: &'static [AnalyzerKind],

    /// It is `true` if this `Analyzer` uses a policy function to decide whether or not to apply
    /// `Changes` to the IR. In this case a policy function must be provided when calling `analyze`.
    /// It is `false` if this `Analyzer` does not use a policy function and it changes directly the
    /// IR.
    ///
    /// The former is the kind of `Analyzer`s which (typically) produce one single atomic `Change`
    /// at time, e.g. `DCE` removes useless expressions one by one. Interrupting their work in the
    /// middle will still deliver some useful output.
    ///
    /// The latter kind of `Analyzer`s are module-oriented, meaning that they work on the entire
    /// function/module at the same time and it does not make sense to split their work into smaller
    /// atomic `Change`s. Any policy function passed to `analyze` is ignored by this kind of analyzers.
    pub uses_policy: bool,
}

impl From<AnalyzerKind> for &'static AnalyzerInfo {
    fn from(kind: AnalyzerKind) -> &'static AnalyzerInfo {
        match kind {
            AnalyzerKind::Arithmetic => &arithmetic::INFO,
            AnalyzerKind::CallSiteFixer => &fix_ssa_opcalls::INFO,
            AnalyzerKind::Combiner => &inst_combine::INFO,
            AnalyzerKind::CopyPropagation => &copy_propagation::INFO,
            AnalyzerKind::CSE => &cse::INFO,
            AnalyzerKind::DCE => &dce::INFO,
            AnalyzerKind::Inferer => &infer_regusage::INFO,
            AnalyzerKind::InterProc => &interproc::INFO,
            AnalyzerKind::SCCP => &sccp::INFO,
        }
    }
}

/// Basic trait for all the analyzers.
pub trait Analyzer: Any + Debug {
    fn info(&self) -> &'static AnalyzerInfo;
    fn as_any(&self) -> &dyn Any;
}

/// An atomic change to the IR.
///
/// It represents a high-level, `Analyzer` specific change to apply to the IR.
pub trait Change: Any + Debug {
    fn as_any(&self) -> &dyn Any;
}

/// A `Change` which replaces a node with another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ReplaceValue(pub NodeIndex, pub NodeIndex);
impl Change for ReplaceValue {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// A `Change` which removes a node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RemoveValue(pub NodeIndex);
impl Change for RemoveValue {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// An `Action` to take with respect to a `Change`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Action {
    Apply,
    Skip,
    Abort,
}

/// Short-hand policy to apply all the `Change`s.
pub fn all(_change: Box<dyn Change>) -> Action {
    Action::Apply
}

/// Short-hand policy to skip all the `Changes`s.
pub fn none(_change: Box<dyn Change>) -> Action {
    Action::Skip
}

/// An `Analyzer` that takes a function.
pub trait FuncAnalyzer: Analyzer {
    /// Look for possbile `Change`s to apply to `func`. When one is found `policy` is called with
    /// that `Change` as parameter; then according to the return value it is applied or discarded.
    ///
    /// As `Change`s are applied, the IR is not the same as before, thus `Change`s previously
    /// discarded could be proposed again by the `Analyer`. On the other hand an `Analyzer` is
    /// not expected to propose again a `Change` if all the previous other `Change`s were skipped.
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        func: &mut RadecoFunction,
        policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>>;
}

/// An `Analyzer` that takes a module.
pub trait ModuleAnalyzer: Analyzer {
    /// Look for possbile `Change`s to apply to `mod`. When one is found `policy` is called with
    /// that `Change` as parameter; then according to the return value it is applied or discarded.
    ///
    /// As `Change`s are applied, the IR is not the same as before, thus `Change`s previously
    /// discarded could be proposed again by the `Analyer`. On the other hand an `Analyzer` is
    /// not expected to propose again a `Change` if all the previous other `Change`s were skipped.
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        module: &mut RadecoModule,
        policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>>;
}

/// Get all the available `FuncAnalyzer`s
pub fn all_func_analyzers() -> Vec<AnalyzerKind> {
    vec![
        AnalyzerKind::Arithmetic,
        AnalyzerKind::Combiner,
        AnalyzerKind::CopyPropagation,
        AnalyzerKind::CSE,
        AnalyzerKind::DCE,
        AnalyzerKind::SCCP,
    ]
}

/// Get all the available `ModuleAnalyzer`s
pub fn all_module_analyzers() -> Vec<AnalyzerKind> {
    vec![
        AnalyzerKind::CallSiteFixer,
        AnalyzerKind::Inferer,
        AnalyzerKind::InterProc,
    ]
}
