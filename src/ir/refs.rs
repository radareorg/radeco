trait RefTranslate<A> {
	fn translate(&self) -> A;
}

trait RefHolder<A> {
	fn translate<U>(&self, i: T, further: U::RequiredContext) -> RefHolder<B> where
		T: RefHolder<A>,
		U: RefHolder<B>,
		B: RefTranslate<A>;
}
