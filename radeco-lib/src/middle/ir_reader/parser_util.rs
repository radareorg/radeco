pub fn str_to_u16(s: &str, radix: u32) -> u16 {
    u16::from_str_radix(s, radix).unwrap()
}

pub fn str_to_u64(s: &str, radix: u32) -> u64 {
    u64::from_str_radix(s, radix).unwrap()
}
