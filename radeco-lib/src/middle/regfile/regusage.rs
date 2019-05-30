use super::RegisterId;
use fixedbitset::FixedBitSet;

/// The set of registers (possibly including the memory "register") that a
/// function reads and/or preserves.
///
/// **Note:** Instances created with [`Default::default`] *cannot be modified*
/// To create a mutable instance, use [`SubRegisterFile::new_register_usage`].
///
/// **Implementation note:** This stores which registers a function *ignores*
/// instead of what it reads. This is so the `Default` implementation of
/// "reads and clobbers everything" is safe to assign to unanalyzed functions.
#[derive(Debug, Clone, Default)]
pub struct RegisterUsage {
    /// Registers that are *not* parameters
    ignores: FixedBitSet,
    /// Callee-saved registers
    preserves: FixedBitSet,
}

impl RegisterUsage {
    pub(super) fn with_register_count(regcount: usize) -> Self {
        RegisterUsage {
            ignores: FixedBitSet::with_capacity(regcount),
            preserves: FixedBitSet::with_capacity(regcount),
        }
    }

    /// Returns `true` if a callee that adheres to `self` can be called by a
    /// caller that assumes that callee adheres to `other`.
    pub fn is_compatible_with(&self, other: &RegisterUsage) -> bool {
        // self.reads.is_subset(&other.reads) && self.preserves.is_superset(&other.preserves)
        other.ignores.ones().all(|i| self.ignores[i])
            && other.preserves.ones().all(|i| self.preserves[i])
    }

    /// Returns if this `RegisterUsage` can be modified.
    /// If this returns `false`, *calling the `set_*` functions will panic*.
    pub fn is_mutable(&self) -> bool {
        self.ignores.len() > 0 && self.preserves.len() > 0
    }

    pub fn is_ignored(&self, reg_id: RegisterId) -> bool {
        self.ignores[reg_id.to_usize()]
    }
    pub fn is_read(&self, reg_id: RegisterId) -> bool {
        !self.ignores[reg_id.to_usize()]
    }
    pub fn is_preserved(&self, reg_id: RegisterId) -> bool {
        self.preserves[reg_id.to_usize()]
    }
    pub fn is_clobbered(&self, reg_id: RegisterId) -> bool {
        !self.preserves[reg_id.to_usize()]
    }

    pub fn set_ignored(&mut self, reg_id: RegisterId) -> () {
        self.ignores.set(reg_id.to_usize(), true)
    }
    pub fn set_read(&mut self, reg_id: RegisterId) -> () {
        self.ignores.set(reg_id.to_usize(), false)
    }
    pub fn set_preserved(&mut self, reg_id: RegisterId) -> () {
        self.preserves.set(reg_id.to_usize(), true)
    }
    pub fn set_clobbered(&mut self, reg_id: RegisterId) -> () {
        self.preserves.set(reg_id.to_usize(), false)
    }

    pub fn set_all_ignored(&mut self) -> () {
        self.ignores.set_range(.., true)
    }
    pub fn set_all_read(&mut self) -> () {
        self.ignores.set_range(.., false)
    }
    pub fn set_all_preserved(&mut self) -> () {
        self.preserves.set_range(.., true)
    }
    pub fn set_all_clobbered(&mut self) -> () {
        self.preserves.set_range(.., false)
    }
}
