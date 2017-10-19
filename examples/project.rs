// Example illustrates project loading

extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;

use r2api::api_trait::R2Api;
use r2pipe::R2;
use radeco_lib::frontend::radeco_containers::{ProjectLoader, ModuleLoader, FunctionLoader};

use radeco_lib::frontend::radeco_source::Source;
use std::cell::RefCell;

use std::rc::Rc;


fn main() {
    let mut r2 = R2::new(Some("/bin/ls")).expect("Hahahaha");
    r2.analyze();
    let src: Rc<Source> = Rc::new(Rc::new(RefCell::new(r2)));
    let mut p = ProjectLoader::default()
        .path("/bin/ls")
        .source(Rc::clone(&src))
        .module_loader(ModuleLoader::default()
            .source(Rc::clone(&src))
            .function_loader(FunctionLoader::default().include_defaults()))
        .load();

}
