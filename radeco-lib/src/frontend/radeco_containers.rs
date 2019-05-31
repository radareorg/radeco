//! This module defines `Containers` used to hold results of analysis
//!
//! Containers are broken down into three levels of hierarchy to reflect different
//! levels of program analysis.
//!
//! The top-level container, `RadecoProject`, is in most cases, the gateway
//! to start using radeco-lib. A project essentially contains all the state/information
//! required for analysis of a binary. A single `RadecoProject` contains several
//! `RadecoModule`, one module for the main binary and, optionally, one for every
//! shared library. Lastly, `RadecoModule` is broken down into `RadecoFunction`, which holds
//! the per-function information for every identified function in a `RadecoModule`.
//!
//! Corresponding to each of the containers is a loader that constructs the containers.
//! Loaders are configurable, with sane defaults, and are designed to work independently
//! of loaders above it. Although the loaders are powerful way to interact with the loading
//! process, the basic process is quite straight forward. Here is a quick example with all
//! default options:
//!
//! ```rust ignore
//! # use radeco_lib::frontend::radeco_containers::{RadecoProject, ProjectLoader};
//! # fn main() {
//! let mut rp: RadecoProject = ProjectLoader::default()  // setup the default loader
//!                                 .path("/bin/ls")      // path to bin to analyze
//!                                 .load();              // fire-off the loading
//! # }
//! ```
//!
//! All default options are defined under `radeco_containers::loader_defaults`.
//!
//! For more examples of loading, check the `examples/` directory of this project.

use crate::frontend::imports::ImportInfo;
use crate::frontend::llanalyzer;
use crate::frontend::radeco_source::Source;
use crate::frontend::ssaconstructor::SSAConstruct;

use crate::middle::regfile::{RegisterUsage, SubRegisterFile};
use crate::middle::ssa::cfg_traits::CFG;
use crate::middle::ssa::ssa_traits::{NodeType, SSA};

use crate::middle::ssa::ssastorage::SSAStorage;
use petgraph::Direction;

use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::EdgeRef;
use r2api::api_trait::R2Api;
use r2api::structs::{
    LCCInfo, LEntryInfo, LExportInfo, LOpInfo, LRelocInfo, LSectionInfo, LStringInfo, LSymbolInfo,
    LSymbolType, LVarInfo,
};

use r2pipe::r2::R2;
use rayon::prelude::*;
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::btree_map;
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::rc::Rc;
use std::slice;
use std::sync::Arc;

// use cpuprofiler::PROFILER;

/// Defines sane defaults for the loading process.
pub mod loader_defaults {
    use super::FLResult;
    use super::{FunctionKind, RadecoFunction, RadecoModule};
    use crate::frontend::radeco_source::Source;
    use r2api::structs::LSymbolType;
    use std::borrow::Cow;
    use std::rc::Rc;

    /// Use symbol information to identify functions
    pub fn strat_use_symbols(
        _source: Option<&Rc<dyn Source>>,
        _fl: &FLResult,
        rmod: &RadecoModule,
    ) -> FLResult {
        rmod.symbols
            .iter()
            .filter(|f| {
                if let Some(LSymbolType::Func) = f.stype {
                    true
                } else {
                    false
                }
            })
            .fold(FLResult::default(), |mut acc, s| {
                let mut rfn = RadecoFunction::default();
                rfn.name = Cow::from(s.name.as_ref().unwrap().to_owned());
                rfn.offset = s.vaddr.unwrap();
                rfn.size = s.size.unwrap();

                acc.functions.insert(rfn.offset, rfn);
                acc.new += 1;
                acc
            })
    }

    /// Use analysis that `Source` provides to identify functions
    pub fn strat_use_source(
        source: Option<&Rc<dyn Source>>,
        fl: &FLResult,
        _rmod: &RadecoModule,
    ) -> FLResult {
        // Load function information fom `Source`
        if let Some(ref src) = source {
            let mut new_fl = FLResult::default();

            // FIXME -> Handle errors properly.
            let relocs = src
                .relocs()
                .expect("failed to get relocs")
                .into_iter()
                .map(|reloc| reloc.name.unwrap())
                .collect::<Vec<_>>();
            let imports = src
                .imports()
                .expect("failed to get imports")
                .into_iter()
                .map(|import| import.name.unwrap())
                .collect::<Vec<_>>();

            let functions = src.functions().expect("failed to get functions");

            functions
                .into_iter()
                .map(|f| {
                    let name = f.clone().name.unwrap();

                    if relocs.iter().any(|reloc| name.ends_with(reloc)) {
                        (f, FunctionKind::Relocated)
                    } else if imports.iter().any(|import| name.ends_with(import)) {
                        (f, FunctionKind::Imported)
                    } else {
                        (f, FunctionKind::Local)
                    }
                })
                .for_each(|(function, kind)| {
                    let mut rfn = RadecoFunction::default();
                    rfn.offset = function.offset.unwrap();
                    rfn.size = function.size.unwrap();
                    rfn.name = Cow::from(function.name.as_ref().unwrap().to_owned());
                    rfn.callconv_name = function.calltype.as_ref().unwrap().to_owned();
                    rfn.kind = kind;
                    new_fl.functions.insert(rfn.offset, rfn);
                    new_fl.new = new_fl.new + 1;
                });
            new_fl
        } else {
            fl.clone()
        }
    }
}

/// Top level container used to hold all analysis
pub struct RadecoProject {
    /// Map of loaded modules
    modules: Vec<RadecoModule>,
    /// Register/Arch information for loaded project
    reginfo: Arc<SubRegisterFile>,
}

// Graph where every node is an Address (function start address) and edges are labeled
// by the `callsite`, i.e., the actual location of the call.
pub type CallGraph = Graph<u64, CallContextInfo>;
pub trait CGInfo {
    // Return a list of callers to function at offset, along with their callsites
    fn callers<'a>(&'a self, idx: NodeIndex) -> Box<dyn Iterator<Item = (u64, NodeIndex)> + 'a>;
    // Return (callsite, call target)
    fn callees<'a>(&'a self, idx: NodeIndex) -> Box<dyn Iterator<Item = (u64, NodeIndex)> + 'a>;
}

impl CGInfo for CallGraph {
    // Return a list of callers to function at offset, along with their callsites
    fn callers<'a>(&'a self, idx: NodeIndex) -> Box<dyn Iterator<Item = (u64, NodeIndex)> + 'a> {
        box self
            .edges_directed(idx, Direction::Incoming)
            .map(|er| (er.weight().csite, er.target()))
    }

    // Return (callsite, call target)
    fn callees<'a>(&'a self, idx: NodeIndex) -> Box<dyn Iterator<Item = (u64, NodeIndex)> + 'a> {
        box self
            .edges_directed(idx, Direction::Outgoing)
            .map(|er| (er.weight().csite, er.target()))
    }
}

#[derive(Default)]
/// Container to store information about a single loaded binary or library.
pub struct RadecoModule {
    /// Human-readable name for the  module
    name: Cow<'static, str>,
    // Information from the loader
    symbols: Vec<LSymbolInfo>,
    strings: Vec<LStringInfo>,
    sections: Arc<Vec<LSectionInfo>>,
    // Map from PLT entry address to `ImportInfo` for an import
    pub imports: HashMap<u64, ImportInfo>,
    exports: Vec<LExportInfo>,
    relocs: Vec<LRelocInfo>,
    libs: Vec<String>,
    entrypoint: Vec<LEntryInfo>,
    // Information from early/low-level analysis
    /// Call graph for current module
    pub callgraph: CallGraph,
    /// Map of functions loaded
    pub functions: BTreeMap<u64, RadecoFunction>,
    /// Source used to load this module
    pub source: Option<Rc<dyn Source>>,
}

impl fmt::Debug for RadecoModule {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub enum FunctionType {
    /// Function defined in the current binary
    Function,
    /// Import from another module. Set to u16::max_value() to represent `Unknown`
    /// Fixed up when the corresponding library that defines this function is loaded
    Import(u16),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum BindingType {
    // Arguments - ith argument
    RegisterArgument(usize),
    StackArgument(usize),
    // Local variables - base register, offset
    RegisterLocal(String, i64),
    // Stack offset (from "SP")
    StackLocal(usize),
    // Return
    Return,
    // Unknown
    Unknown,
}

impl Default for BindingType {
    fn default() -> BindingType {
        BindingType::Unknown
    }
}

impl BindingType {
    pub fn is_argument(&self) -> bool {
        match *self {
            BindingType::RegisterArgument(_) | BindingType::StackArgument(_) => true,
            _ => false,
        }
    }

    pub fn is_local(&self) -> bool {
        match *self {
            BindingType::RegisterLocal(_, _) | BindingType::StackLocal(_) => true,
            _ => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            BindingType::Return => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct VarBinding {
    pub btype: BindingType,
    name: Cow<'static, str>,
    pub type_str: String,
    // Index of the register in regfile that represents this varbinding
    pub ridx: Option<u64>,
    pub idx: NodeIndex, // Some arbitrary, serializable data can be added to these fields later.
    is_preserved: bool,
}

impl VarBinding {
    pub fn new(
        btype: BindingType,
        ty_str: String,
        name: Option<String>,
        idx: NodeIndex,
        ridx: Option<u64>,
    ) -> VarBinding {
        let name = Cow::from(name.unwrap_or_default());
        VarBinding {
            name: name,
            type_str: ty_str,
            btype: btype,
            idx: idx,
            ridx: ridx,
            is_preserved: false,
        }
    }

    fn local(local: LVarInfo) -> VarBinding {
        let r = local.reference.unwrap();
        let btype = BindingType::RegisterLocal(r.base.unwrap(), r.offset.unwrap());
        VarBinding::new(
            btype,
            local.vtype.unwrap(),
            local.name,
            NodeIndex::end(),
            None,
        )
    }

    fn is_matched_reg_local(binding: &VarBinding, reg_name: String, offset: i64) -> bool {
        match binding.btype {
            BindingType::RegisterLocal(ref reg, n) if reg.clone() == reg_name && n == offset => {
                true
            }
            _ => false,
        }
    }

    pub fn index(&self) -> NodeIndex {
        self.idx
    }

    pub fn btype(&self) -> BindingType {
        self.btype.clone()
    }

    pub fn btype_mut(&mut self) -> &mut BindingType {
        &mut self.btype
    }

    pub fn name(&self) -> &str {
        &*self.name
    }

    pub fn is_preserved(&self) -> bool {
        self.is_preserved
    }

    pub fn mark_preserved(&mut self) {
        self.is_preserved = true;
    }
}

pub type VarBindings = Vec<VarBinding>;

/// The type of this function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Local,
    Imported,
    Relocated,
}

impl Default for FunctionKind {
    fn default() -> Self {
        FunctionKind::Local
    }
}

#[derive(Debug, Clone, Default)]
/// Container to store information about identified function.
/// Used as a basic unit in intra-functional analysis.
pub struct RadecoFunction {
    // Represents the type of function
    // ftype: FunctionType,
    /// Raw instruction information for the current function
    pub instructions: Vec<LOpInfo>,
    /// Is current function known to be recursive
    is_recursive: Option<bool>,
    /// Human readable name for the function. Taken either from
    /// the symbol table or assigned based on offset.
    pub name: Cow<'static, str>,
    /// Start address of the function
    pub offset: u64,
    /// Size of the function in bytes
    size: u64,
    /// List of (data-) addresses this function references
    datarefs: Vec<u64>,
    /// Constructed SSA for the function
    ssa: SSAStorage,
    /// Node index in the module-level callgraph
    cgid: NodeIndex,
    /// Variable bindings
    bindings: VarBindings,
    // XXX Should it be separated from RadecoFunction?
    // NodeIndex is SSAStorage's NodeIndex
    binding_map: HashMap<NodeIndex, VarBindings>,
    /// Calling convention of this function
    pub callconv: Option<LCCInfo>,
    /// Register usage of this function
    pub regusage: RegisterUsage,
    /// Name of the calling convention of this function (e.g. amd64, ms, arm64, etc.)
    // see https://github.com/radare/radare2/tree/9e08da0fa6b6c36edf04db72d22e065ccc90d381/libr/anal/d
    pub callconv_name: String,

    /// Kind of the function.
    pub kind: FunctionKind,
}

#[derive(Default)]
/// Top-level loader used to initialize a `RadecoProject`
pub struct ProjectLoader<'a> {
    load_libs: bool,
    path: Cow<'static, str>,
    load_library_path: Option<Cow<'static, str>>,
    filter_modules: Option<fn(&RadecoModule) -> bool>,
    source: Option<Rc<dyn Source>>,
    mloader: Option<ModuleLoader<'a>>,
}

impl<'a> ProjectLoader<'a> {
    pub fn new() -> ProjectLoader<'a> {
        ProjectLoader {
            load_libs: false,
            path: Cow::from(""),
            load_library_path: None,
            filter_modules: None,
            source: None,
            mloader: None,
        }
    }
    // TODO:
    //  - Associate identified bins/libs with their ModuleLoaders
    //  - Implement loading of libraries
    //  - Parallelize module loading as they should have different sources
    //  - Use filter option
    //  - Setup arch information in `RadecoProject`
    /// Enable loading of libraries
    pub fn load_libs(mut self) -> ProjectLoader<'a> {
        self.load_libs = true;
        self
    }

    /// Set path to load from
    pub fn path<T: AsRef<str>>(mut self, path: T) -> ProjectLoader<'a> {
        self.path = Cow::from(path.as_ref().to_owned());
        self
    }

    /// Setup and configure `ModuleLoader` to use
    pub fn module_loader<'b: 'a>(mut self, mloader: ModuleLoader<'b>) -> ProjectLoader<'a> {
        self.mloader = Some(mloader);
        self
    }

    /// Set the source to use for loading. This is propagated to every `ModuleLoader`
    /// unless it is reconfigured.
    pub fn source(mut self, source: Rc<dyn Source>) -> ProjectLoader<'a> {
        self.source = Some(source);
        self
    }

    /// Set path to look for libraries. The `ProjectLoader` looks for
    /// matching filenames recursively within this directory.
    /// Only used if `load_libs` is true.
    pub fn load_library_path(mut self, path: &'static str) -> ProjectLoader<'a> {
        self.load_library_path = Some(Cow::from(path));
        self
    }

    /// Filter loading of `RadecoModules` based on `f`
    pub fn filter_modules(mut self, f: fn(&RadecoModule) -> bool) -> ProjectLoader<'a> {
        self.filter_modules = Some(f);
        self
    }

    /// Kick everything off based on the config/defaults
    pub fn load(mut self) -> RadecoProject {
        if self.source.is_none() {
            // Load r2 source.
            let mut r2 = R2::new(Some(&self.path)).expect("Unable to open r2");
            let _ = r2.raw("e bin.minstr=1".to_string());
            //New r2 process is launched thus it needs to analyze
            r2.analyze_all();
            let r2w = Rc::new(RefCell::new(r2));
            self.source = Some(Rc::new(r2w));
        };

        let source = self.source.as_ref().unwrap();

        // TODO: Load more arch specific information from the source

        if self.mloader.is_none() {
            self.mloader = Some(
                ModuleLoader::default()
                    .source(Rc::clone(source))
                    .build_ssa()
                    .build_callgraph()
                    .load_datarefs()
                    .load_locals()
                    .parallel()
                    // .assume_cc()
                    .stub_imports(),
            );
        }

        let mut mod_map = Vec::new();

        {
            let mod_loader = self.mloader.as_mut().unwrap();
            // TODO: Set name correctly
            mod_map.push(mod_loader.load(Rc::clone(source)));
        }

        // Clear out irrelevant fields in self and move it into project loader
        // XXX: Do when needed!
        // self.mod_loader = None;
        let regfile = SubRegisterFile::new(
            &source
                .register_profile()
                .expect("Unable to load register profile"),
        );

        RadecoProject {
            modules: mod_map,
            // XXX
            reginfo: Arc::new(regfile),
        }
    }
}

// Iterators over RadecoProject to yeils RadecoModules
/// `RadecoModule` with project information `zipped` into it
pub struct ZippedModule<'m> {
    pub project: &'m RadecoProject,
    pub module: &'m RadecoModule,
}

pub struct ModuleIter<'m> {
    project: &'m RadecoProject,
    iter: slice::Iter<'m, RadecoModule>,
}

// TODO: Add a way to access the project
pub struct ZippedModuleMut<'m> {
    pub module: &'m mut RadecoModule,
}

pub struct ModuleIterMut<'m> {
    iter: slice::IterMut<'m, RadecoModule>,
}

impl<'m> Iterator for ModuleIter<'m> {
    type Item = ZippedModule<'m>;
    fn next(&mut self) -> Option<ZippedModule<'m>> {
        if let Some(rmod) = self.iter.next() {
            Some(ZippedModule {
                project: &self.project,
                module: rmod,
            })
        } else {
            None
        }
    }
}

impl<'m> Iterator for ModuleIterMut<'m> {
    type Item = ZippedModuleMut<'m>;
    fn next(&mut self) -> Option<ZippedModuleMut<'m>> {
        if let Some(rmod) = self.iter.next() {
            Some(ZippedModuleMut { module: rmod })
        } else {
            None
        }
    }
}

pub struct ZippedFunction<'f> {
    pub module: &'f RadecoModule,
    pub function: (&'f u64, &'f RadecoFunction),
}

pub struct FunctionIter<'f> {
    module: &'f RadecoModule,
    iter: btree_map::Iter<'f, u64, RadecoFunction>,
}

// TODO: Add a way to access module
pub struct ZippedFunctionMut<'f> {
    pub function: (&'f u64, &'f mut RadecoFunction),
}

pub struct FunctionIterMut<'f> {
    iter: btree_map::IterMut<'f, u64, RadecoFunction>,
}

impl<'f> Iterator for FunctionIter<'f> {
    type Item = ZippedFunction<'f>;
    fn next(&mut self) -> Option<ZippedFunction<'f>> {
        if let Some(rfn) = self.iter.next() {
            Some(ZippedFunction {
                module: self.module,
                function: rfn,
            })
        } else {
            None
        }
    }
}

impl<'f> Iterator for FunctionIterMut<'f> {
    type Item = ZippedFunctionMut<'f>;
    fn next(&mut self) -> Option<ZippedFunctionMut<'f>> {
        if let Some(rfn) = self.iter.next() {
            Some(ZippedFunctionMut { function: rfn })
        } else {
            None
        }
    }
}

#[derive(Default)]
/// Module-level loader used to construct a `RadecoModule`
pub struct ModuleLoader<'a> {
    source: Option<Rc<dyn Source>>,
    floader: Option<FunctionLoader<'a>>,
    filter: Option<fn(&RadecoFunction) -> bool>,
    build_callgraph: bool,
    build_ssa: bool,
    load_datarefs: bool,
    load_locals: bool,
    parallel: bool,
    assume_cc: bool,
    stub_imports: bool,
}

impl<'a> ModuleLoader<'a> {
    // TODO:
    //  1. Callgraph from source
    //  2. As a part of above, fill in callrefs and callxrefs in RadecoFunction
    //  3. Expose SSA Construction as a part of loading process with options
    //     to parallelize it.
    //  4. Optionally load datarefs
    //  5. Optionally load local var information for functions
    /// Setup `Source` for `ModuleLoader`
    pub fn source<'b: 'a>(mut self, src: Rc<dyn Source>) -> ModuleLoader<'a> {
        self.source = Some(src);
        self
    }

    /// Builds callgraph. Needs support from `Source`
    pub fn build_callgraph(mut self) -> ModuleLoader<'a> {
        self.build_callgraph = true;
        self
    }

    /// Builds SSA for loaded functions
    pub fn build_ssa(mut self) -> ModuleLoader<'a> {
        self.build_ssa = true;
        self
    }

    /// Loads information about datareferences for loaded functions.
    /// Needs support from `Source`
    pub fn load_datarefs(mut self) -> ModuleLoader<'a> {
        self.load_datarefs = true;
        self
    }

    /// Loads local variable information for loaded functions.
    /// Needs support from `Source`
    pub fn load_locals(mut self) -> ModuleLoader<'a> {
        self.load_locals = true;
        self
    }

    /// Executes parallelizable functions in parallel. Uses `num_thread` number
    /// of threads. Defaults to 8 if `None`.
    pub fn parallel(mut self) -> ModuleLoader<'a> {
        self.parallel = true;
        self
    }

    /// Assume calling convention information in regfile to be true. This is used for setting up
    /// bindings for arguments and return values for functions.
    pub fn assume_cc(mut self) -> ModuleLoader<'a> {
        self.assume_cc = true;
        self
    }

    /// Create blank, stub entries for imported functions.
    /// Required for load-libs, auto set when load_libs is true for the project loader.
    pub fn stub_imports(mut self) -> ModuleLoader<'a> {
        self.stub_imports = true;
        self
    }

    fn init_fn_bindings(rfn: &mut RadecoFunction, sub_reg_f: &SubRegisterFile) {
        // Setup binding information for functions based on reg_p. Note that this essential
        // marks the "potential" arguments without worrying about if they're ever used. Future
        // analysis can refine this information to make argument recognition more precise.

        // Get register state at entry block (for arguments) and at exit block (for returns).
        let (entry_state, exit_state) = {
            let ssa = rfn.ssa();
            let entry = ssa.entry_node().expect("No entry node found for function!");
            let exit = ssa.exit_node().expect("No exit node found for function!");

            let entry_state = ssa
                .registers_in(entry)
                .expect("No registers found in entry");
            let exit_state = ssa.registers_in(exit).expect("No registers found in entry");
            (ssa.operands_of(entry_state), ssa.operands_of(exit_state))
        };

        let mut tbindings: Vec<VarBinding> = sub_reg_f
            .alias_info
            .iter()
            .filter_map(|reg| {
                let alias = reg.0;
                if let &Some(idx) = &["A0", "A1", "A2", "A3", "A4", "A5", "SN"]
                    .iter()
                    .position(|f| f == alias)
                {
                    let mut vb = VarBinding::default();
                    if idx < 6 {
                        vb.btype = BindingType::RegisterArgument(idx);
                        vb.idx = *entry_state
                            .iter()
                            .find(|&&ridx| {
                                if let Ok(NodeType::Comment(ref s)) =
                                    rfn.ssa().node_data(ridx).map(|n| n.nt)
                                {
                                    if s == reg.1 {
                                        true
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                }
                            })
                            .unwrap_or(&NodeIndex::end());
                    } else {
                        vb.btype = BindingType::Return;
                        vb.idx = *exit_state
                            .iter()
                            .find(|&&ridx| {
                                if let Ok(NodeType::Comment(ref s)) =
                                    rfn.ssa().node_data(ridx).map(|n| n.nt)
                                {
                                    if s == reg.1 {
                                        true
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                }
                            })
                            .unwrap_or(&NodeIndex::end());
                    }
                    vb.ridx = sub_reg_f
                        .register_id_by_alias(alias)
                        .map(|rid| rid.to_u8() as u64);
                    Some(vb)
                } else {
                    None
                }
            })
            .collect();

        tbindings.sort_by(|x, y| match (&x.btype, &y.btype) {
            (BindingType::RegisterArgument(i), BindingType::RegisterArgument(ref j)) => i.cmp(j),
            (BindingType::RegisterArgument(_), _) => Ordering::Less,
            (_, BindingType::RegisterArgument(_)) => Ordering::Greater,
            (_, _) => Ordering::Equal,
        });

        rfn.bindings = tbindings;
    }

    /// Kick everything off and load module information based on config and defaults
    pub fn load(&mut self, src: Rc<dyn Source>) -> RadecoModule {
        let source = if self.source.is_some() {
            self.source.as_ref().unwrap()
        } else {
            &src
        };

        if self.floader.is_none() {
            self.floader = Some(FunctionLoader::default().include_defaults());
        }
        // Setup source for the FunctionLoader
        let floader = self.floader.as_mut().unwrap();
        floader.source = Some(Rc::clone(source));

        let mut rmod = RadecoModule::default();

        // Fill in module level information from the `Source`
        match source.symbols() {
            Ok(sym_info) => rmod.symbols = sym_info,
            Err(_e) => radeco_warn!(_e),
        }

        match source.sections() {
            Ok(section_info) => rmod.sections = Arc::new(section_info),
            Err(_e) => radeco_warn!(_e),
        }

        match source.imports() {
            // TODO: Set the node in callgraph, either now or later.
            Ok(import_info) => {
                rmod.imports = import_info
                    .iter()
                    .filter_map(|ii| {
                        if let Some(plt) = ii.plt {
                            if let Some(LSymbolType::Func) = ii.itype {
                                Some((
                                    plt,
                                    ImportInfo::new_stub(
                                        plt,
                                        Cow::from(ii.name.as_ref().unwrap().clone()),
                                    ),
                                ))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect();
            }
            Err(_e) => radeco_warn!(_e),
        }

        match source.exports() {
            Ok(exports) => rmod.exports = exports,
            Err(_e) => radeco_warn!(_e),
        }

        match source.relocs() {
            Ok(relocs) => rmod.relocs = relocs,
            Err(_e) => radeco_warn!(_e),
        }

        match source.libraries() {
            Ok(libs) => rmod.libs = libs,
            Err(_e) => radeco_warn!(_e),
        }

        match source.entrypoint() {
            Ok(ep) => rmod.entrypoint = ep,
            Err(_e) => radeco_warn!(_e),
        }

        match source.strings(true) {
            Ok(strings) => rmod.strings = strings,
            Err(_e) => radeco_warn!(_e),
        }

        let mut flresult = floader.load(&rmod);
        flresult.functions = if self.filter.is_some() {
            let filter_fn = self.filter.as_ref().unwrap();
            flresult
                .functions
                .into_iter()
                .filter(|&(_, ref v)| filter_fn(v))
                .collect()
        } else {
            flresult.functions
        };

        rmod.functions = flresult.functions;

        // Load instructions into functions
        for rfn in rmod.functions.values_mut() {
            if let FunctionKind::Local = rfn.kind {
                rfn.instructions = source.disassemble_function(&rfn.name).unwrap_or(Vec::new());
            }
        }

        // Load calling conventions for all functions and imports
        for (&rfn_addr, rfn) in &mut rmod.functions {
            rfn.callconv = source.cc_info_of(rfn_addr).ok();
        }
        for (&imp_addr, imp_info) in &mut rmod.imports {
            let imp_rfn = &mut *imp_info.rfn.borrow_mut();
            imp_rfn.callconv = source.cc_info_of(imp_addr).ok();
            if let Some(ref f) = rmod.functions.get(&imp_addr) {
                imp_rfn.callconv_name = f.callconv_name.clone();
            }
        }

        // Optionally construct the SSA.
        let reg_p = source
            .register_profile()
            .expect("Unable to load register profile");
        let sub_reg_f = SubRegisterFile::new(&reg_p);
        if self.build_ssa {
            if self.parallel {
                let ascc = self.assume_cc;
                rmod.functions.par_iter_mut().for_each(|(_, rfn)| {
                    SSAConstruct::<SSAStorage>::construct(rfn, &reg_p, ascc, true);
                });
            } else {
                for rfn in rmod.functions.values_mut() {
                    SSAConstruct::<SSAStorage>::construct(rfn, &reg_p, self.assume_cc, true);
                }
            }
        }

        if self.stub_imports {
            for ifn in rmod.imports.values_mut() {
                SSAConstruct::<SSAStorage>::construct(
                    &mut ifn.rfn.borrow_mut(),
                    &reg_p,
                    self.assume_cc,
                    true,
                );
            }
        }

        // Load optional information. These need support from `Source` for analysis
        if self.build_callgraph || self.load_datarefs || self.load_locals {
            let aux_info = match source.functions() {
                Ok(info) => info,
                Err(_e) => {
                    radeco_warn!(_e);
                    Vec::new()
                }
            };

            if self.build_callgraph {
                rmod.callgraph = llanalyzer::load_call_graph(aux_info.as_slice(), &rmod);
                // Iterate through nodes and associate nodes with the correct functions
                for nidx in rmod.callgraph.node_indices() {
                    if let Some(cg_addr) = rmod.callgraph.node_weight(nidx) {
                        if let Some(rfn) = rmod.functions.get_mut(cg_addr) {
                            // Functions defined in this binary
                            rfn.cgid = nidx;
                        }
                        if let Some(ifn) = rmod.imports.get_mut(cg_addr) {
                            // Handle imports
                            ifn.rfn.borrow_mut().cgid = nidx;
                        }
                    }
                }
            }

            if self.load_datarefs {
                for info in &aux_info {
                    if let Some(mut rfn) = rmod.functions.get_mut(&info.offset.unwrap()) {
                        rfn.datarefs = info.datarefs.clone().unwrap_or_default();
                    }
                }
            }

            if self.load_locals {
                for info in &aux_info {
                    if let Some(rfn) = rmod.functions.get_mut(&info.offset.unwrap()) {
                        let locals_res = self.source.as_ref().map(|s| s.locals_of(rfn.offset));
                        let mut locals = match locals_res {
                            Some(Ok(_locals)) => _locals
                                .into_iter()
                                .map(|l| VarBinding::local(l))
                                .collect::<Vec<_>>(),
                            Some(Err(_e)) => {
                                radeco_warn!("{:?}", _e);
                                Vec::new()
                            }
                            None => {
                                radeco_warn!("Source is not found");
                                Vec::new()
                            }
                        };
                        rfn.bindings_mut().append(&mut locals);
                    }
                }
            }
        }

        if self.build_callgraph && self.assume_cc {
            for rfn in rmod.functions.values_mut() {
                ModuleLoader::init_fn_bindings(rfn, &sub_reg_f);
            }
            // Do the same for imports.
            for ifn in rmod.imports.values_mut() {
                ModuleLoader::init_fn_bindings(&mut ifn.rfn.borrow_mut(), &sub_reg_f);
            }

            llanalyzer::init_call_ctx(&mut rmod);
        }

        for rfn in rmod.functions.values_mut() {
            rfn.mark_locals();
            rfn.mark_args();
        }
        // Set source
        rmod.source = Some(Rc::clone(&source));

        rmod
    }

    /// Setup a function loader for the module
    pub fn function_loader(mut self, f: FunctionLoader<'a>) -> ModuleLoader<'a> {
        self.floader = Some(f);
        self
    }

    /// Filter identified/loaded functions based on filter function
    pub fn filter(mut self, f: fn(&RadecoFunction) -> bool) -> ModuleLoader<'a> {
        self.filter = Some(f);
        self
    }
}

#[derive(Default)]
/// Breaks down `RadecoModule` into functions
/// Performs low-level function identification.
pub struct FunctionLoader<'a> {
    source: Option<Rc<dyn Source>>,
    strategies: Vec<&'a dyn PredicatedLoader>,
}

pub trait PredicatedLoader {
    /// Decide if the current loading strategy should be executed, based on previous results.
    ///
    /// Defaults to true, need not implement if the Loader is not conditional
    fn predicate(&self, _: &FLResult) -> bool {
        true
    }
    /// Function to execute to breakdown the `RadecoModule`
    fn strategy(
        &self,
        source: Option<&Rc<dyn Source>>,
        last: &FLResult,
        rmod: &RadecoModule,
    ) -> FLResult;
}

impl<T> PredicatedLoader for T
where
    T: Fn(Option<&Rc<dyn Source>>, &FLResult, &RadecoModule) -> FLResult,
{
    fn strategy(
        &self,
        source: Option<&Rc<dyn Source>>,
        last: &FLResult,
        rmod: &RadecoModule,
    ) -> FLResult {
        self(source, last, rmod)
    }
}

#[derive(Default, Clone)]
/// Results from `FunctionLoader`
pub struct FLResult {
    /// Map from identified function offset to the RadecoFunction instance
    functions: BTreeMap<u64, RadecoFunction>,
    /// Number of functions identified
    new: u32,
}

impl<'a> FunctionLoader<'a> {
    /// Add a function identification strategy to the pipeline
    pub fn strategy<'b: 'a>(mut self, strat: &'b dyn PredicatedLoader) -> FunctionLoader<'a> {
        self.strategies.push(strat);
        self
    }

    /// Kick everything off and breakdown a radeco module into functions
    pub fn load(&mut self, rmod: &RadecoModule) -> FLResult {
        self.strategies
            .iter()
            .fold(FLResult::default(), |mut acc, f| {
                if f.predicate(&acc) {
                    let fl = f.strategy(self.source.as_ref(), &acc, rmod);
                    acc.new += fl.new;
                    acc.functions.extend(fl.functions.into_iter());
                }
                acc
            })
    }

    /// Include default strategies to identify functions in the loaded binary
    pub fn include_defaults(mut self) -> FunctionLoader<'a> {
        // TODO: Append these to the front
        self.strategies.push(&loader_defaults::strat_use_source);
        self
    }
}

impl RadecoProject {
    pub fn new() -> RadecoProject {
        RadecoProject {
            modules: Vec::new(),
            reginfo: Arc::new(SubRegisterFile::default()),
        }
    }

    pub fn regfile(&self) -> &Arc<SubRegisterFile> {
        &self.reginfo
    }

    pub fn nth_module(&self, idx: usize) -> Option<&RadecoModule> {
        if self.modules.len() > idx {
            Some(&self.modules[idx])
        } else {
            None
        }
    }

    pub fn nth_module_mut<'a>(&mut self, idx: usize) -> Option<&mut RadecoModule> {
        if self.modules.len() > idx {
            Some(&mut self.modules[idx])
        } else {
            None
        }
    }

    pub fn iter<'a>(&'a self) -> ModuleIter<'a> {
        ModuleIter {
            project: &self,
            iter: self.modules.iter(),
        }
    }

    pub fn iter_mut<'a>(&'a mut self) -> ModuleIterMut<'a> {
        ModuleIterMut {
            iter: self.modules.iter_mut(),
        }
    }
}

impl RadecoModule {
    pub fn new(path: String) -> RadecoModule {
        let mut rmod = RadecoModule::default();
        rmod.name = Cow::from(path.clone());
        rmod
    }

    pub fn name(&self) -> &str {
        &*self.name
    }

    pub fn function(&self, offset: u64) -> Option<&RadecoFunction> {
        self.functions.get(&offset)
    }

    pub fn function_mut(&mut self, offset: u64) -> Option<&mut RadecoFunction> {
        self.functions.get_mut(&offset)
    }

    pub fn function_rename(&mut self, offset: u64, new_name: &str) -> Option<String> {
        assert!(!new_name.is_empty());
        let mut old_name = None;
        if let Some(rfn) = self.function_mut(offset) {
            old_name = Some(String::from(&*rfn.name));
            rfn.name = From::from(String::from(new_name));
        }
        if let Some(ref mut src) = self.source {
            let _ = src.raw(format!("afn {} {:x}", new_name, offset));
        }
        old_name
    }

    pub fn iter<'a>(&'a self) -> FunctionIter<'a> {
        FunctionIter {
            module: &self,
            iter: self.functions.iter(),
        }
    }

    pub fn iter_mut<'a>(&'a mut self) -> FunctionIterMut<'a> {
        FunctionIterMut {
            iter: self.functions.iter_mut(),
        }
    }

    pub fn sections(&self) -> &Arc<Vec<LSectionInfo>> {
        &self.sections
    }

    pub fn strings(&self) -> &Vec<LStringInfo> {
        &self.strings
    }

    pub fn callees_of(&self, rfn: &RadecoFunction) -> Vec<(u64, NodeIndex)> {
        // TODO More efficient implementation
        let csite_nodes = rfn
            .call_sites(&self.callgraph)
            .into_iter()
            .map(|c| c.csite_node);
        csite_nodes
            .flat_map(|cn| self.callgraph.callees(cn).collect::<Vec<_>>())
            .collect::<Vec<_>>()
    }
}

impl RadecoFunction {
    pub fn new() -> RadecoFunction {
        RadecoFunction::default()
    }

    pub fn instructions(&self) -> &[LOpInfo] {
        self.instructions.as_slice()
    }

    pub fn ssa(&self) -> &SSAStorage {
        &self.ssa
    }

    pub fn ssa_mut(&mut self) -> &mut SSAStorage {
        &mut self.ssa
    }

    /// Returns the id in the call graph for this function.
    pub fn cgid(&self) -> NodeIndex {
        self.cgid
    }

    pub fn bindings(&self) -> &VarBindings {
        &self.bindings
    }

    pub fn bindings_mut(&mut self) -> &mut VarBindings {
        &mut self.bindings
    }

    pub fn call_sites(&self, call_graph: &CallGraph) -> Vec<CallContextInfo> {
        call_graph
            .edges_directed(self.cgid, Direction::Outgoing)
            .into_iter()
            .map(|e| e.weight().clone())
            .collect()
    }

    pub fn callees(&self, call_graph: &CallGraph) -> Vec<NodeIndex> {
        call_graph.callees(self.cgid).map(|(_, n)| n).collect()
    }

    pub fn datarefs(&self) -> &Vec<u64> {
        &self.datarefs
    }

    pub fn locals(&self) -> VarBindings {
        self.bindings
            .iter()
            .filter(|vb| vb.btype.is_local())
            .map(|vb| vb.clone())
            .collect::<Vec<_>>()
    }

    pub fn args(&self) -> VarBindings {
        unimplemented!()
    }

    pub fn set_args(&mut self, _args: &Vec<usize>) {
        unimplemented!()
    }

    pub fn set_modifides(&mut self, _locals: &Vec<usize>) {
        unimplemented!()
    }

    pub fn set_locals(&mut self, _locals: &Vec<usize>) {
        unimplemented!()
    }

    pub fn set_returns(&mut self, returns: &Vec<usize>) {
        for i in returns {
            if let Some(ref mut var) = self.bindings.iter_mut().nth(*i) {
                var.btype = BindingType::Return;
            }
        }
    }

    fn retrieve_binding(&self, node: NodeIndex) -> Vec<VarBinding> {
        use crate::middle::ir::MOpcode;
        let sign = match self.ssa.opcode(node) {
            Some(MOpcode::OpSub) => -1,
            Some(MOpcode::OpAdd) => 1,
            _ => 1,
        };
        let mut ret_bindings = Vec::new();
        match self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid) {
            MOpcode::OpSub | MOpcode::OpAdd => {
                let ops = self.ssa.operands_of(node);
                assert!(ops.len() == 2);
                if let Some(val) = self.ssa.constant_value(ops[1]) {
                    let left_regs = self.ssa.registers(ops[0]);
                    for left_reg in left_regs {
                        let mut bindings = self
                            .bindings
                            .iter()
                            .filter(|binding| {
                                let offset = sign * (val as i64);
                                VarBinding::is_matched_reg_local(binding, left_reg.clone(), offset)
                            })
                            .cloned()
                            .collect::<Vec<_>>();
                        ret_bindings.append(&mut bindings);
                    }
                }
                // TODO
                // let right_regs = self.ssa.registers(ops[1]);
                ret_bindings
            }
            _ => Vec::new(),
        }
    }

    pub fn mark_args(&mut self) {
        // TODO
    }

    pub fn mark_locals(&mut self) {
        use crate::middle::ir::MOpcode;
        use crate::middle::ssa::ssa_traits::SSAWalk;
        let ssa = &self.ssa;
        for node in ssa.inorder_walk() {
            match ssa.opcode(node) {
                Some(MOpcode::OpStore) | Some(MOpcode::OpLoad) => {
                    let dst = ssa.operands_of(node)[1];
                    let bindings = self.retrieve_binding(dst);
                    if bindings.len() > 0 {
                        self.binding_map.insert(dst, bindings);
                    }
                }
                _ => {}
            }
        }
    }

    pub fn local_at(&self, node: NodeIndex, forward: bool) -> Option<VarBindings> {
        let next = self.ssa.replaced_map.get(&node).cloned();
        if next.is_some() && forward {
            self.binding_map.get(&next.unwrap()).cloned()
        } else {
            self.binding_map.get(&node).cloned()
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct CallContextInfo {
    /// NodeIndex mapping from a node in the caller's context to a node in callee's context
    pub map: Vec<(NodeIndex, NodeIndex)>,
    /// NodeIndex corresponding to callsite (`OpCall`) in the caller context
    pub csite_node: NodeIndex,
    /// Address of callsite
    pub csite: u64,
}

#[cfg(test)]
mod test {
    #[test]
    fn test_fn_loader() {
        // let ld = |x: &FLResult, y: &RadecoModule| -> FLResult { unimplemented!() };

        // let mut fl = FunctionLoader::default();
        // fl.strategy(&ld);
    }
}
