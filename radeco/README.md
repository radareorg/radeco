# radeco

Radeco is the radare decompiler tool using the [radeco-lib](https://github.com/radare/radeco-lib) rust crate.

[![Build Status](https://travis-ci.org/radare/radeco.svg)](https://travis-ci.org/radare/radeco)

## Usage

Type the following commands to test radeco

	$ cargo build
	$ target/debug/radeco --shell /bin/ls

You can also run radeco from r2 like this:

	$ r2 /bin/ls
	> #!pipe target/debug/radeco --shell -
