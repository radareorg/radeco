//! Defines structs and methods to deal with imports and dynamic linking

use crate::frontend::radeco_containers::RadecoFunction;
use std::borrow::Cow;
use std::cell::RefCell;

use std::sync::Arc;

#[derive(Debug)]
pub struct ImportInfo {
    pub plt: u64,
    pub name: Cow<'static, str>,
    pub rfn: Arc<RefCell<RadecoFunction>>,
}

impl ImportInfo {
    pub fn new_stub(plt: u64, name: Cow<'static, str>) -> ImportInfo {
        let mut rfn = RadecoFunction::default();
        rfn.name = name.clone();
        ImportInfo {
            plt: plt,
            name: name,
            rfn: Arc::new(RefCell::new(rfn)),
        }
    }
}
