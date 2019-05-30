use crate::utils::*;

/// Defines the Argument push type in a calling convention
/// During a function call, arguments can be pushed onto the
/// call stack in different ways.
/// Floating point arguments are pushed onto a PseudoStack.
#[derive(Copy, Clone, Debug)]
pub enum ArgPushType {
    RTL, // Right to Left
    LTR, // Left to Right
    PseudoStack,
}

/// For different calling conventions, arguments are passed differently to functions
/// They might be either passed through registers or on the stack.
/// For some architectures, higher level abstractions are passed through-
/// complicated memory structures. User can use the placeholder CustomDefinition for this.
#[derive(Copy, Clone, Debug)]
pub enum ArgPlacement {
    RegisterBased(&'static [AbstractRegister]),
    CallStack(ArgPushType),
    CustomDefinition, // Undefined
}

/// A generic structure helpful for describing a value.
#[derive(Copy, Clone, Debug)]
pub enum VType {
    /// Value is stored on the stack at certain offset and with a size
    /// Represented as (offset, size)
    StackVal(i64, usize),
    /// Value could also be placed in a register.
    /// Represented as (register_name/alias, size)
    Register(AbstractRegister, usize),
}

/// Structure which can be used to describe calling conventions
/// used in different architectures.
pub trait CallingConvention {
    /////////////////
    //// Getters ////
    /////////////////

    /// Get Calling Convention name
    fn name(&self) -> Option<&String>;

    /// Defines the method used for placement of call arguments
    fn arg_placement(&self) -> Option<ArgPlacement>;

    /// Defines the method used for placement of floating point arguments
    fn fp_arg_placement(&self) -> Option<ArgPlacement>;

    /// Defines stack difference reserved for return addr
    fn stack_sp_diff(&self) -> Option<u64>;

    /// Describes the way the return value is stored for caller to retrieve
    fn return_val(&self) -> Option<VType>;

    /// Describes the way the return address is used by callee
    fn return_addr(&self) -> Option<VType>;

    /// Boolean to denote if the callee is responsible for stack cleanup in its epilogue
    fn callee_cleanup(&self) -> Option<bool>;

    /// Get register mapping for this calling convention
    fn absregmap(&self) -> Option<AbsRegMap>;

    /////////////////
    //// Setters ////
    /////////////////

    /// Get Calling Convention name
    fn set_name(&mut self, _: String);

    /// Defines the method used for placement of call arguments
    fn set_arg_placement(&mut self, _: ArgPlacement);

    /// Defines the method used for placement of floating point arguments
    fn set_fp_arg_placement(&mut self, _: ArgPlacement);

    /// Defines stack difference reserved for return addr
    fn set_stack_sp_diff(&mut self, _: u64);

    /// Describes the way the return value is stored for caller to retrieve
    fn set_return_val(&mut self, _: VType);

    /// Describes the way the return address is used by callee
    fn set_return_addr(&mut self, _: VType);

    /// Boolean to denote if the callee is responsible for stack cleanup in it's epilogue
    fn set_callee_cleanup(&mut self, _: bool);

    /// Set register mapping for this calling convention
    fn set_absregmap(&mut self, _: AbsRegMap);
}
