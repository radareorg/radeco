pub trait DataOperator<T> {
	fn join(inputs: &[T]) -> T;
}
