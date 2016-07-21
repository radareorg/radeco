//! Defines valid types.

#[derive(Clone, Debug)]
pub enum RTypes {
    Overdefined,
    Reg8,
    Reg16,
    Reg32,
    Reg64,
    Num8,
    Num16,
    Num32,
    Num64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Bool,
    Undefined,
    Ptr(Box<RTypes>),
    Code,
}

