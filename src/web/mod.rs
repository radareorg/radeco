extern crate iron;

use std::any::Any;
use std::any::TypeId;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use rustc_serialize::json::Json;
use self::iron::prelude::*;
use self::iron::middleware::Handler;

// pub mod graphserve;
pub mod ui;

use self::ui::UI;

trait Data: Send + Sync {
	fn describe_interface(&self) -> Json;
	fn rpc(&mut self, request: Json) -> Json;
}

struct WorkspaceInner {
	named_data: HashMap<String, Arc<Data>>,
	constructors: HashMap<TypeId, (String, Box<UI>)>,
}

pub struct Workspace {
	inner: Mutex<WorkspaceInner>,
}

impl Workspace {
	pub fn new() -> Workspace {
		Workspace {
			inner: Mutex::new(WorkspaceInner{
				named_data: HashMap::new(),
				constructors: HashMap::new(),
			})
		}
	}

	pub fn expose<T: UI + Default + Any>(&mut self, name: &str) {
		self.inner.lock().unwrap().constructors.insert(TypeId::of::<T>(), (
			name.to_owned(),
			T::new_boxed()
		));
	}

	pub fn add<T>(&mut self, name: &str, thing: T) {
		//
	}
}

impl Handler for Workspace {
	fn handle(&self, r: &mut Request) -> IronResult<Response> {
		Ok(Response::new())
	}
}
