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

use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::collections::{btree_map, hash_map};
use std::path::Path;

use petgraph::graph::NodeIndex;

use r2pipe::r2::R2;
use r2api::structs::{LOpInfo, LRegInfo};
use r2api::api_trait::R2Api;

use middle::ssa::ssastorage::SSAStorage;
use frontend::bindings::{Binding, RBindings, RadecoBindings};
use frontend::source::Source;

//pub trait ProjectLoader {
    //fn load<T: AsRef<str>>(T, Option<ProjectOptions>) -> Result<RadecoProject, String>;
//}

#[derive(Debug, Default)]
pub struct ProjectOptions {
    load_libs: bool,
    load_path: Cow<'static, str>,
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

#[derive(Debug)]
/// Container to store information about a single loaded binary or library.
pub struct RadecoModule {
    /// Flag to check if functions are loaded/indentified
    are_functions_loaded: bool,
    // TODO: Placeholder. Fix this with real graph.
    /// Call graph for current module
    call_graph: Option<String>,
    /// Map of functions loaded
    functions: BTreeMap<u64, RadecoFunction>,
    // TODO: This needs to be something better than just a String.
    /// List of imports for the current module
    imports: Vec<String>,
    /// Library name
    name: Cow<'static, str>,
    /// Path on disk to the loaded library
    path: Cow<'static, str>,
    // TODO:
    // sections: ,
    // symbols: ,
    // loader: ,
    // imports: ,
    // exports: ,
}

#[derive(Debug)]
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

pub const UNKNOWN_MODULE: u16 = 0xFFFF;

#[derive(Debug, Clone)]
/// Container to store information about identified function.
/// Used as a basic unit in intra-functional analysis.
pub struct RadecoFunction {
    /// Identified variable bindings for `RadecoFunction`
    bindings: RadecoBindings<Binding<NodeIndex>>,
    /// Current module being analyzed, i.e., the index of the module that returned this
    /// RadecoFunction instance. Note that this is not necessarily
    /// the module where the function is defined, in case of an import.
    current_module: u16,
    /// Represents the type of function
    ftype: FunctionType,
    /// Raw instruction information for the current function
    pub instructions: Vec<LOpInfo>,
    /// Is current function known to be recursive
    is_recursive: bool,
    /// Human readable name for the function. Taken either from
    /// the symbol table or assigned based on offset.
    pub name: Cow<'static, str>,
    /// Start address of the function
    pub offset: u64,
    // Calls to the function
    //refs: Vec<CallRefs>,
    /// Size of the function in bytes
    size: u64,
    /// Constructed SSA for the function
    ssa: SSAStorage,
    // Calls from the current function to other functions
    //xrefs: Vec<CallRefs>,
}

//#[derive(Debug)]
// Default project loader that uses r2 and r2pipe
// Implements the `ProjectLoader` trait
//pub struct R2ProjectLoader { }

//#[derive(Debug)]
// Load a RadecoProject from formatted JSON files
//pub struct JSONProjectLoader { }

pub struct ProjectLoader<'a> {
    load_libs: bool,
    mloader: Option<ModuleLoader<'a>>,
}

pub struct ModuleLoader<'a> {
    source: Box<Source>,
    floader: Option<FunctionLoader<'a>>,
    filter: Option<fn (x: RadecoFunction) -> bool>,
}

pub trait PredicatedLoader {
    fn predicate(&self, x: &FLResult) -> bool;
    fn strategy(&self, last: &FLResult, rmod: &RadecoModule) -> FLResult;
}

impl<T> PredicatedLoader for T
where T: Fn (&FLResult, &RadecoModule) -> FLResult {
    fn predicate(&self, x: &FLResult) -> bool { true }
    fn strategy(&self, last: &FLResult, rmod: &RadecoModule) -> FLResult { 
        self(last, rmod)
    }
}

#[derive(Debug, Default)]
pub struct FLResult {
    functions: Vec<RadecoFunction>,
    new: u32,
}

#[derive(Default)]
pub struct FunctionLoader<'a> {
    strategies: Vec<&'a PredicatedLoader>,
}

impl<'a> FunctionLoader<'a> {
    pub fn strategy<'b: 'a>(mut self, strat: &'b PredicatedLoader) -> FunctionLoader<'a> {
        self.strategies.push(strat);
        self
    }

    // TODO: Add default strats
    pub fn load(&mut self, rmod: &RadecoModule) -> FLResult {
        self.strategies.iter().fold(FLResult::default(), |mut acc, f| {
            if f.predicate(&acc) {
                let fl = f.strategy(&acc, rmod);
                acc.new += fl.new;
                acc.functions.extend(fl.functions.iter().cloned());
            }
            acc
        })
    }
}

impl RadecoProject {
    pub fn new() -> RadecoProject {
        RadecoProject {
            modules: HashMap::new(),
            reginfo: LRegInfo::default(),
        }
    }

    pub fn new_module(&mut self,
                      module_path: String) -> Result<&mut RadecoModule, &'static str> {
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
        RadecoModule {
            are_functions_loaded: false,
            call_graph: None,
            functions: BTreeMap::new(),
            imports: Vec::new(),
            name: Cow::from(path.clone()),
            // FIXME
            path: Cow::from(path),
        }
    }

    pub fn new_function_at(&mut self, offset: u64) -> Result<&mut RadecoFunction, &'static str> {
        if self.functions.contains_key(&offset) {
            return Err("Function already defined at current `offset`")
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
            bindings: RadecoBindings::new(),
            current_module: UNKNOWN_MODULE,
            ftype: FunctionType::Function,
            instructions: Vec::new(),
            is_recursive: false,
            name: Cow::from(name),
            offset: offset,
            //refs: Vec::new(),
            size: size,
            ssa: SSAStorage::new(),
            //xrefs: Vec::new(),
        }
    }

    pub fn ssa(&self) -> &SSAStorage {
        &self.ssa
    }

    pub fn ssa_mut(&mut self) -> &mut SSAStorage {
        &mut self.ssa
    }
}

//impl ProjectLoader for R2ProjectLoader {
    //fn load<T: AsRef<str>>(bin: T,
                           //options: Option<ProjectOptions>)
        //-> Result<RadecoProject, String>
    //{
        //// TODO: Parse/use option
        //let options = options.unwrap_or(ProjectOptions::default());
        //let bpath = String::from(bin.as_ref());
        //let mut r2 = R2::new(Some(bin.as_ref()))?;
        //r2.init();
        //let mut rp = RadecoProject::new();
        //rp.set_reginfo(r2.reg_info().expect("Unable to load `LRegInfo`"));
        //{
            //// TODO: Setup the project with more information,
            //// such as: arch, platform etc.
            //let mut main_mod = rp.new_module(bpath)?;
            //// TODO: Load more information such as sections, symbol table, global defines,
            //// constant strings etc.
            //main_mod.are_functions_loaded = true;
            //for function in r2.functions() {
                //let name = function.name.as_ref().unwrap();
                //let offset = function.offset.expect("Invalid offset");
                //let ref mut rfn = main_mod.new_function_at(offset)?;
                //rfn.name = Cow::from(name.to_owned());
                //rfn.ftype = if name.contains("sym.imp") {
                    //FunctionType::Import(UNKNOWN_MODULE)
                //} else {
                    //FunctionType::Function
                //};
                //let insts = r2.instructions_at(offset);
                //rfn.instructions = insts;
            //}
        //}
        //Ok(rp)
    //}
//}

// Function Loader.
//
// Function loader takes in a module, as a part of the module loader process
// and chunks it down to individual functions.
// The actual implementation of this may vary depending on the needs,
// from simply reading off from the symbol table to performing analysis
// to figure out function boundaries.
// In any case, at the end of this process, the module should be broken down
// into several functions.
//
// More complex loaders, example: using symbol table when available and performing
// analysis otherwise, may be composed and built from simpler ones.
//pub trait FunctionLoader: Sized {
    //fn function_loader(rmod: &RadecoModule) -> Self;
    //fn load() -> Self;
//}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_fn_loader() {
        let ld = |x: &FLResult, y: &RadecoModule| -> FLResult {
            unimplemented!()
        };

        let mut fl = FunctionLoader::default();
        fl.strategy(&ld);
    }
}
