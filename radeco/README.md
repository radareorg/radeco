# radeco

Radeco is the radare decompiler tool using the radeco-lib rust crate

## Usage

Type the following commands to test radeco

	$ cargo build --features deprecated
	$ target/debug/radeco --shell /bin/ls

Soon, it will be possible to use radeco from inside r2:

	> #!pipe radeco --shell
