//! Defines traits and structs that perform the actual symbolic emulation.

#[derive(Clone, Copy, Debug)]
pub enum EngineError {
    Undefined,
    InCorrectOperand,
}

pub type EngineResult<T> = Result<T, EngineError>;

pub trait Engine: Sized {
    fn run(&mut self) -> EngineResult<()>;
}

pub trait Configure {
    type For: Engine;
    fn configure(_: &mut Self::For) -> EngineResult<()> {
        Ok(())
    }
}
