use petgraph::graph::NodeIndex;

use libsmt::backends::backend::SMTBackend;
use libsmt::backends::smtlib2::SMTLib2;
use libsmt::logics::qf_abv;
use libsmt::theories::{array_ex, bitvec, core};
use r2api::structs::Endian;

use super::{Memory, MemoryError, MemoryResult};

// Not using address_width/endianness
#[derive(Clone, Debug)]
pub struct QWordMemory {
    map: Option<NodeIndex>,
    address_width: usize,
    endian: Endian,
}

impl QWordMemory {
    /// Gets the address width.
    pub const fn address_width(&self) -> usize {
        self.address_width
    }

    /// Gets the endianness.
    pub const fn endian(&self) -> Endian {
        self.endian
    }
}

impl Memory for QWordMemory {
    type VarRef = NodeIndex;

    fn new(address_width: usize, endian: Endian) -> QWordMemory {
        QWordMemory {
            map: None,
            address_width,
            endian,
        }
    }

    fn init_memory(&mut self, solver: &mut SMTLib2<qf_abv::QF_ABV>) {
        let bv_array = qf_abv::array_sort(qf_abv::bv_sort(64), qf_abv::bv_sort(64));
        let idx_ = solver.new_var(Some("mem"), bv_array);
        // Set memory to all 0s
        let arr_const_ty = qf_abv::array_const(
            qf_abv::bv_sort(64),
            qf_abv::bv_sort(64),
            bitvec::OpCodes::Const(0, 64),
        );

        let const_0 = solver.new_const(arr_const_ty);
        solver.assert(core::OpCodes::Cmp, &[idx_, const_0]);
        self.map = Some(idx_);
    }

    fn read(
        &mut self,
        addr: NodeIndex,
        read_size: usize,
        solver: &mut SMTLib2<qf_abv::QF_ABV>,
    ) -> MemoryResult<NodeIndex> {
        if self.map.is_none() {
            self.init_memory(solver);
        }
        let mem = self.map.ok_or(MemoryError::MissingMap)?;
        let idx = solver.assert(array_ex::OpCodes::Select, &[mem, addr]);
        if read_size < 64 {
            Ok(solver.assert(bitvec::OpCodes::Extract((read_size - 1) as u64, 1), &[idx]))
        } else {
            Ok(idx)
        }
    }

    fn write(
        &mut self,
        addr: NodeIndex,
        data: NodeIndex,
        _write_size: usize,
        solver: &mut SMTLib2<qf_abv::QF_ABV>,
    ) -> MemoryResult<()> {
        if self.map.is_none() {
            self.init_memory(solver);
        }

        let mem = self.map.ok_or(MemoryError::MissingMap)?;
        let new_mem = solver.assert(array_ex::OpCodes::Store, &[mem, addr, data]);
        self.map = Some(new_mem);

        Ok(())
    }
}
