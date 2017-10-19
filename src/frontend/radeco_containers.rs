//! Defines `RadecoProject`, `RadecoModule` and `RadecoFunction`.
//! Temporarily defines `Loader` and `Source`.
//!
//!
//!
//! Example Usage
//! ====
//!
//! Default example:
//! ```rust
//! let options = LoadOptions::default();
//! let mut loader = R2ProjectLoader::new();
//! let mut rp = loader.load("/bin/ls", load_options);
//! for mut ref rmod in rp.iter_mod_mut() {
//!     loader.load_functions(rmod)?;
//! }
//!
//! for mut ref rmod in rp.iter_mod_mut() {
//!     for mut ref rfn in rmod.iter_fn_mut() {
//!         // Construct SSA, Do some analysis.
//!     }
//! }
//! ```
//!
//! Example with custom function loader/detection:
//! ```
//! ...
//! let mut fident = FunctionIdentifier::new();
//! let mut ref rmod is rp.itrer_mod_mut() {
//!     fident.identify_functions(rmod);
//! }
//! ```

use frontend::bindings::{Binding, RBindings, RadecoBindings};
use frontend::radeco_source::{WrappedR2Api, Source};

use middle::ssa::ssastorage::SSAStorage;

use petgraph::graph::NodeIndex;
use r2api::api_trait::R2Api;
use r2api::structs::{LOpInfo, LRegInfo, LSymbolInfo, LRelocInfo, LImportInfo, LExportInfo,
                     LSectionInfo, LEntryInfo, LSymbolType};

use r2pipe::r2::R2;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::collections::{btree_map, hash_map};
use std::path::Path;
use std::rc::Rc;

pub mod loader_defaults {
    use frontend::radeco_source::Source;
    use r2api::structs::LSymbolType;
    use std::borrow::Cow;
    use std::rc::Rc;
    use super::{FLResult, PredicatedLoader};
    use super::{RadecoModule, RadecoFunction};

    pub fn strat_use_symbols(source: Option<&Rc<Source>>,
                             fl: &FLResult,
                             rmod: &RadecoModule)
                             -> FLResult {
        rmod.symbols
            .iter()
            .filter(|f| if let Some(LSymbolType::Func) = f.stype {
                true
            } else {
                false
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

    pub fn strat_use_source(source: Option<&Rc<Source>>,
                            fl: &FLResult,
                            rmod: &RadecoModule)
                            -> FLResult {
        // If there was any symbol information, don't use this
        // Load function information fom `Source`
        if let Some(ref src) = source {
            let mut new_fl = FLResult::default();
            if let Ok(ref functions) = src.functions() {
                for function in functions {
                    let mut rfn = RadecoFunction::default();
                    rfn.offset = function.offset.unwrap();
                    rfn.size = function.size.unwrap();
                    rfn.name = Cow::from(function.name.as_ref().unwrap().to_owned());
                    new_fl.functions.insert(rfn.offset, rfn);
                    new_fl.new = new_fl.new + 1;
                }
            }
            new_fl
        } else {
            fl.clone()
        }
    }
}

#[derive(Debug)]
/// Top level overall project
pub struct RadecoProject {
    /// Map of loaded modules
    modules: HashMap<String, RadecoModule>,
    /// Register/Arch information for loaded project
    /// TODO: Discuss and replace with arch
    reginfo: LRegInfo,
}

#[derive(Debug, Default)]
/// Container to store information about a single loaded binary or library.
pub struct RadecoModule {
    /// Human-readable name for the  module
    name: Cow<'static, str>,
    /// Path on disk to the loaded library
    path: Cow<'static, str>,
    /// //////////////////////////////////////////////
    /// / Information from the loader
    /// //////////////////////////////////////////////
    symbols: Vec<LSymbolInfo>,
    sections: Vec<LSectionInfo>,
    imports: Vec<LImportInfo>,
    exports: Vec<LExportInfo>,
    relocs: Vec<LRelocInfo>,
    libs: Vec<String>,
    entrypoint: Vec<LEntryInfo>,
    /// ///////////////////////////////////////////////
    /// / Information from early/low-level analysis
    /// ///////////////////////////////////////////////
    /// TODO: Placeholder. Fix this with real graph.
    /// Call graph for current module
    call_graph: Option<String>,
    /// Map of functions loaded
    functions: BTreeMap<u64, RadecoFunction>,
}

#[derive(Default, Clone, Debug)]
pub struct CallRefs {
    /// Offset of the caller function
    caller_fn_offset: u64,
    /// Offset of callee function
    callee_fn_offset: u64,
    /// Offset of the call instruction
    call_site_offset: u64,
}

#[derive(Debug, Clone)]
pub enum FunctionType {
    /// Function defined in the current binary
    Function,
    /// Import from another module. Set to u16::max_value() to represent `Unknown`
    /// Fixed up when the corresponding library that defines this function is loaded
    Import(u16),
}

#[derive(Debug, Clone, Default)]
/// Container to store information about identified function.
/// Used as a basic unit in intra-functional analysis.
pub struct RadecoFunction {
    // Identified variable bindings for `RadecoFunction`
    // bindings: RadecoBindings<Binding<NodeIndex>>,
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
    // TODO: Calls to the function
    // refs: Vec<CallRefs>,
    /// Size of the function in bytes
    size: u64,
    /// Constructed SSA for the function
    ssa: SSAStorage, /* TODO: Calls from the current function to other functions
                      * xrefs: Vec<CallRefs>, */
}

#[derive(Default)]
pub struct ProjectLoader<'a> {
    load_libs: bool,
    path: Cow<'static, str>,
    load_library_path: Option<Cow<'static, str>>,
    filter_modules: Option<fn(&RadecoModule) -> bool>,
    source: Option<Rc<Source>>,
    mloader: Option<ModuleLoader<'a>>,
}

impl<'a> ProjectLoader<'a> {
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
    pub fn source(mut self, source: Rc<Source>) -> ProjectLoader<'a> {
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

    pub fn filter_modules(mut self, f: fn(&RadecoModule) -> bool) -> ProjectLoader<'a> {
        self.filter_modules = Some(f);
        self
    }

    /// Kick everything off based on the config/defaults
    pub fn load(mut self) -> RadecoProject {
        if self.source.is_none() {
            // Load r2 source.
            let mut r2 = R2::new(Some(&self.path)).expect("Unable to open r2");
            let mut r2w: WrappedR2Api<R2> = Rc::new(RefCell::new(r2));
            self.source = Some(Rc::new(r2w));
        };

        let source = self.source.as_ref().unwrap();

        // TODO: Load more arch specific information from the source

        if self.mloader.is_none() {
            self.mloader = Some(ModuleLoader::default().source(Rc::clone(source)));
        }

        let mut mod_map = HashMap::new();

        {
            let mod_loader = self.mloader.as_mut().unwrap();
            // TODO: Set name correctly
            mod_map.insert("main".to_owned(), mod_loader.load());
        }

        // Clear out irrelevant fields in self and move it into project loader
        // XXX: Do when needed!
        // self.mod_loader = None;

        RadecoProject {
            modules: mod_map,
            // XXX
            reginfo: LRegInfo::default(),
        }
    }
}

#[derive(Default)]
pub struct ModuleLoader<'a> {
    source: Option<Rc<Source>>,
    floader: Option<FunctionLoader<'a>>,
    filter: Option<fn(&RadecoFunction) -> bool>,
}

impl<'a> ModuleLoader<'a> {
    pub fn source<'b: 'a>(mut self, src: Rc<Source>) -> ModuleLoader<'a> {
        self.source = Some(src);
        self
    }

    /// Kick everything off and load module information based on config and defaults
    pub fn load(&mut self) -> RadecoModule {
        let source = self.source.as_ref().unwrap();
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
            Err(e) => radeco_warn!(e),
        }

        match source.sections() {
            Ok(section_info) => rmod.sections = section_info,
            Err(e) => radeco_warn!(e),
        }

        match source.imports() {
            Ok(import_info) => rmod.imports = import_info,
            Err(e) => radeco_warn!(e),
        }

        match source.exports() {
            Ok(exports) => rmod.exports = exports,
            Err(e) => radeco_warn!(e),
        }

        match source.relocs() {
            Ok(relocs) => rmod.relocs = relocs,
            Err(e) => radeco_warn!(e),
        }

        match source.libraries() {
            Ok(libs) => rmod.libs = libs,
            Err(e) => radeco_warn!(e),
        }

        match source.entrypoint() {
            Ok(ep) => rmod.entrypoint = ep,
            Err(e) => radeco_warn!(e),
        }

        let mut flresult = floader.load(&rmod);
        flresult.functions = if self.filter.is_some() {
            let filter_fn = self.filter.as_ref().unwrap();
            flresult.functions.into_iter().filter(|&(ref x, ref v)| filter_fn(v)).collect()
        } else {
            flresult.functions
        };

        for (x, v) in flresult.functions.iter() {
            println!("{:#x}: {}", x, v.name);
        }

        rmod.functions = flresult.functions;
        rmod
    }

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
pub struct FunctionLoader<'a> {
    source: Option<Rc<Source>>,
    strategies: Vec<&'a PredicatedLoader>,
}

pub trait PredicatedLoader {
    /// Defaults to true, need not implement if the Loader is not conditional
    fn predicate(&self, x: &FLResult) -> bool {
        true
    }
    /// Function to execute to breakdown the `RadecoModule`
    fn strategy(&self,
                source: Option<&Rc<Source>>,
                last: &FLResult,
                rmod: &RadecoModule)
                -> FLResult;
}

impl<T> PredicatedLoader for T
    where T: Fn(Option<&Rc<Source>>, &FLResult, &RadecoModule) -> FLResult
{
    fn strategy(&self,
                source: Option<&Rc<Source>>,
                last: &FLResult,
                rmod: &RadecoModule)
                -> FLResult {
        self(source, last, rmod)
    }
}

#[derive(Default, Clone)]
pub struct FLResult {
    /// Map from identified function offset to the RadecoFunction instance
    functions: BTreeMap<u64, RadecoFunction>,
    new: u32,
}

impl<'a> FunctionLoader<'a> {
    /// Add a function identification strategy to the pipeline
    pub fn strategy<'b: 'a>(mut self, strat: &'b PredicatedLoader) -> FunctionLoader<'a> {
        self.strategies.push(strat);
        self
    }

    /// Kick everything off and breakdown a radeco module into functions
    pub fn load(&mut self, rmod: &RadecoModule) -> FLResult {
        self.strategies.iter().fold(FLResult::default(), |mut acc, f| {
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
        self.strategies.push(&loader_defaults::strat_use_symbols);
        self.strategies.push(&loader_defaults::strat_use_source);
        self
    }
}

impl RadecoProject {
    pub fn new() -> RadecoProject {
        RadecoProject {
            modules: HashMap::new(),
            reginfo: LRegInfo::default(),
        }
    }

    pub fn new_module(&mut self, module_path: String) -> Result<&mut RadecoModule, &'static str> {
        let module_name = Path::new(&module_path).file_stem().unwrap();
        let module_name_str = module_name.to_str().unwrap().to_owned();
        if self.modules.contains_key(&module_name_str) {
            Err("Module with same name already loaded")
        } else {
            let rmod = RadecoModule::new(module_path.clone());
            self.modules.insert(module_name_str.clone(), rmod);
            Ok(self.modules.get_mut(&module_name_str).unwrap())
        }
    }

    pub fn modules(&self) -> hash_map::Values<String, RadecoModule> {
        self.modules.values()
    }

    pub fn modules_mut(&mut self) -> hash_map::ValuesMut<String, RadecoModule> {
        self.modules.values_mut()
    }

    pub fn reginfo(&self) -> &LRegInfo {
        &self.reginfo
    }

    pub fn set_reginfo(&mut self, reginfo: LRegInfo) {
        self.reginfo = reginfo;
    }
}

impl RadecoModule {
    pub fn new(path: String) -> RadecoModule {
        let mut rmod = RadecoModule::default();
        rmod.name = Cow::from(path.clone());
        rmod
    }

    pub fn new_function_at(&mut self, offset: u64) -> Result<&mut RadecoFunction, &'static str> {
        if self.functions.contains_key(&offset) {
            return Err("Function already defined at current `offset`");
        } else {
            self.functions.insert(offset, RadecoFunction::new(offset, 0));
            Ok(self.functions.get_mut(&offset).unwrap())
        }
    }

    pub fn function_at(&self, offset: &u64) -> Option<&RadecoFunction> {
        self.functions.get(offset)
    }

    pub fn functions(&self) -> btree_map::Values<u64, RadecoFunction> {
        self.functions.values()
    }

    pub fn functions_mut(&mut self) -> btree_map::ValuesMut<u64, RadecoFunction> {
        self.functions.values_mut()
    }
}

impl RadecoFunction {
    pub fn new(offset: u64, size: u64) -> RadecoFunction {
        let name = format!("fun{:x}", offset);
        RadecoFunction {
            // bindings: RadecoBindings::new(),
            // ftype: FunctionType::Function,
            instructions: Vec::new(),
            is_recursive: None,
            name: Cow::from(name),
            offset: offset,
            // refs: Vec::new(),
            size: size,
            ssa: SSAStorage::new(), // xrefs: Vec::new(),
        }
    }

    pub fn ssa(&self) -> &SSAStorage {
        &self.ssa
    }

    pub fn ssa_mut(&mut self) -> &mut SSAStorage {
        &mut self.ssa
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_fn_loader() {
        let ld = |x: &FLResult, y: &RadecoModule| -> FLResult { unimplemented!() };

        let mut fl = FunctionLoader::default();
        fl.strategy(&ld);
    }
}
