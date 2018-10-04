# radeco

Radeco is the radare decompiler tool using the [radeco-lib](https://github.com/radareorg/radeco-lib) rust crate.

[![Build Status](https://travis-ci.org/radareorg/radeco.svg)](https://travis-ci.org/radareorg/radeco)

## Usage

```shell
$ echo '#include<stdio.h>\nint main() {printf("Hello, world.\\n"); return 0;}' | gcc -xc -
$ cargo run
>> load a.out
Cannot find function here
[*] Fixing Callee Information
>> fn_list
sym._init
sym.imp.puts
entry0
sym.deregister_tm_clones
sym.register_tm_clones
sym.__do_global_dtors_aux
entry1.init
sym.main
sym.__libc_csu_init
sym.__libc_csu_fini
sym._fini
>> analyze sym.main
[+] Analyzing: sym.main @ 0x1139
  [*] Eliminating Dead Code
  [*] Propagating Constants
  [*] Eliminating More DeadCode
  [*] Eliminating Common SubExpressions
  [*] Verifying SSA's Validity
>> decompile sym.main
fn sym.main () {
    unsigned int tmp;
    *((rsp - 8)) = rbp
    tmp = sym.imp.puts("Hello, world.", rsi, rdx, rcx, r8, r9)
}
>>
```

## Installation

Note: Nightly Rust is required. You can install it using [rustup](https://rustup.rs/).

```shell
make install
```

## License
Licensed under The BSD 3-Clause License. Please check [COPYING](https://github.com/radare/radeco/blob/master/COPYING) file for
complete license.
