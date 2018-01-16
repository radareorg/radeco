extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;

use r2api::api_trait::R2Api;
use r2pipe::R2;

use radeco_lib::middle::regfile::SubRegisterFile;
use radeco_lib::frontend::radeco_containers::{ProjectLoader, ModuleLoader, FunctionLoader};
use radeco_lib::frontend::radeco_source::Source;
use radeco_lib::analysis::functions::{FuncArgumentAnalyzer};

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

fn main() {
    let mut r2 = R2::new(Some("/home/ammar/junk/rust-dwarf-x86/test_files/register_argument")).expect("Failed to load r2");
    r2.analyze();
    let src: Rc<Source> = Rc::new(Rc::new(RefCell::new(r2)));
    let mut p = ProjectLoader::default()
        .path("/home/ammar/junk/rust-dwarf-x86/test_files/register_argument")
        .source(Rc::clone(&src))
        .module_loader(ModuleLoader::default()
                        .parallel()
                        .build_ssa()
                        .build_callgraph()
                        .load_datarefs()
                        .function_loader(FunctionLoader::default().include_defaults()))
        .load();

    let regfile = Arc::clone(p.regfile());

    for m in p.iter_mut() {
        let sections = Arc::clone(m.module.sections());

        for wrapper in m.module.iter_mut() {
            let analyzer = FuncArgumentAnalyzer::new(&regfile, &sections);

            let (current_offset, current_fn) = wrapper.function;
            analyzer.determine_arguments(current_fn);
        }
    }
}