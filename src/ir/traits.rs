pub trait Manipulation<T, U> {
	fn arg_ins(&mut self, T, U, T) -> T;
	fn arg_mod(&mut self, T, U, T) -> T;
}

pub trait Navigation<T> {
	fn uses_of(&self, T) -> Vec<T>;
	fn args_of(&self, T) -> Vec<T>;
}

pub trait NavigationInternal<T> {
	fn add_uses_to(&self, T, &mut Vec<T>);
	fn add_args_to(&self, T, &mut Vec<T>);
}

impl<T> Navigation<T> for NavigationInternal<T> {
	fn uses_of(&self, t: T) -> Vec<T> {
		let mut r = Vec::<T>::new();
		self.add_uses_to(t, &mut r);
		return r
	}
	fn args_of(&self, t: T) -> Vec<T> {
		let mut r = Vec::<T>::new();
		self.add_args_to(t, &mut r);
		return r
	}
}

pub trait InstructionType {
	type PhiType: Copy + Clone;
	fn make_phi(Self::PhiType) -> Self;
	fn is_phi(&self) -> bool;
}

pub trait Accessible<T, U, V> {
	fn lookup(&self, T) -> LookupResult<U, V>;
}

pub enum LookupResult<ResultType, RedirectType> {
	Found(ResultType),
	Redirect(RedirectType),
	NotFound
}
