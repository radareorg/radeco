// move UI out of here eventually

use std::default::Default;

pub trait UI: Send {
	fn new_boxed() -> Box<UI> where Self: Default + Sized + 'static {
		Box::new(Self::default())
	}
}

use frontend::r2::R2;
impl UI for R2 {}

/*

//Implement here or in pipeline?

use pipeline::Pipeline;
impl UI for Pipeline {}
*/
