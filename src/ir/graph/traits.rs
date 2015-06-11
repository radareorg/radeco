use std::ops::Add;

pub trait Navigation<T> {
	fn uses_of(&self, T) -> Vec<T>;
	fn args_of(&self, T) -> Vec<T>;
}

trait NavigationInternal<T> {
	fn add_uses_to(&self, T, &Vec<T>);
	fn add_args_to(&self, T, &Vec<T>);
}

impl<T> Navigation<T> for NavigationInternal<T> {
	fn uses_of(&self, t: T) -> Vec<T> {
		let r: Vec<T>;
		self.add_uses_to(t, r);
		r
	}
	fn args_of(&self, t: T) -> Vec<T> {
		let r: Vec<T>;
		self.add_args_to(t, r);
		r
	}
}

pub trait InstructionType { fn is_phi(&self) -> bool; } 

pub trait InnerIndexType : Clone + Copy + Add<Output=Self> {
	// replace with zero trait once stable
	fn zero() -> Self;
}
impl InnerIndexType for i8 { fn zero() -> i8 { 0 } }
impl InnerIndexType for i16 { fn zero() -> i16 { 0 } }
impl InnerIndexType for i32 { fn zero() -> i32 { 0 } }
impl InnerIndexType for i64 { fn zero() -> i64 { 0 } }
impl InnerIndexType for isize { fn zero() -> isize { 0 } }
