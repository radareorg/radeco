use crate::utils::*;
use crate::cc::calling_convention::*;

/****************************
 * cdecl calling convention *
 * *************************/

declare_cc!(pub struct Cdecl {
});

register_cc!(Cdecl);

impl Cdecl {
    pub fn new(absregmap: AbsRegMap) -> Result<Cdecl, String> {
        if let Some(&absreg) = absregmap.get(&String::from("eax")) {
            let mut cdecl: Cdecl = Default::default();
            cdecl.set_absregmap(absregmap);
            cdecl.set_return_val(VType::Register(absreg, 32));
            Ok(cdecl)
        } else {
            Err(String::from("AbstractRegister for return value was not allocated"))
        }
    }
}

impl Default for Cdecl {
    fn default() -> Cdecl {
        Cdecl {
            name: String::from("cdecl"),
            arg_placement: ArgPlacement::CallStack(ArgPushType::RTL),
            fp_arg_placement: ArgPlacement::CallStack(ArgPushType::PseudoStack),
            stack_sp_diff: 32,
            return_val: VType::Register(AbstractRegister::WILDCARD, 32),
            return_addr: VType::StackVal(0, 32),
            callee_cleanup: false,
            absregmap: None,
        }
    }
}
