fn main () {
    int local_ch;
    unsigned int tmp;
    int local_8h;
    int local_4h;
    *((rsp - 8)) = rbp
    tmp = sym.imp.__isoc99_scanf(unknown, unknown, unknown, unknown, r8, r9)
    tmp = sym.imp.puts(unknown, unknown, unknown, unknown, unknown, unknown)
    if ((1 ^ ((((((local_ch as 64) | local_8h) & 4294967295) as 32) - 4) & 4294967295)) as 1) {
        goto addr_0x4005DB.0000
    }
    tmp = sym.imp.puts(unknown, unknown, unknown, unknown, unknown, unknown)
    if (!((1 ^ ((((((*((rsp - 12)) as 64) | local_4h) & 4294967295) as 32) - 4) & 4294967295)) as 1)) {
        goto addr_0x4005EC.0000
    }
addr_0x4005EC.0000:
    if (!((1 ^ (((((((*((rsp - 16)) as 64) | (18446744069414584320 & unknown)) & 0) | ((((((((*((rsp - 16)) as 64) | local_8h) & 4294967295) as 32) * ((((*((rsp - 16)) as 64) | (18446744069414584320 & unknown)) & 4294967295) as 32)) as 64) | (((*((rsp - 16)) as 64) | local_8h) & 0)) + (((((((*((rsp - 20)) as 64) | local_ch) & 4294967295) as 32) * ((((*((rsp - 20)) as 64) | (18446744069414584320 & unknown)) & 4294967295) as 32)) as 64) | (((*((rsp - 20)) as 64) | (18446744069414584320 & unknown)) & 0)))) as 32) - ((((((((*((rsp - 12)) as 64) | ((((((((*((rsp - 16)) as 64) | local_8h) & 4294967295) as 32) * ((((*((rsp - 16)) as 64) | (18446744069414584320 & unknown)) & 4294967295) as 32)) as 64) | (((*((rsp - 16)) as 64) | local_8h) & 0)) & 18446744069414584320)) & 4294967295) as 32) * ((((*((rsp - 12)) as 64) | ((((((((*((rsp - 20)) as 64) | local_ch) & 4294967295) as 32) * ((((*((rsp - 20)) as 64) | (18446744069414584320 & unknown)) & 4294967295) as 32)) as 64) | (((*((rsp - 20)) as 64) | (18446744069414584320 & unknown)) & 0)) & 18446744069414584320)) & 4294967295) as 32)) as 64) | local_4h) as 32)) & 4294967295)) as 1)) {
        goto addr_0x40061F.0000
    }
    goto addr_0x40062E.0000
addr_0x40061F.0000:
    tmp = sym.imp.puts(unknown, unknown, unknown, unknown, unknown, unknown)
    goto addr_0x40062E.0000
    tmp = sym.imp.puts(unknown, unknown, unknown, unknown, unknown, unknown)
addr_0x40062E.0000:
    goto addr_0x40062E.0000
addr_0x4005DB.0000:
    tmp = sym.imp.puts(unknown, unknown, unknown, unknown, unknown, unknown)
}
