# rune - symbolic execution for everyone

rune is a symbolic execution engine over ESIL. Integrated with radare2 for
your everyday use!

rune is extensible and customizable. Almost every component in rune
can have multiple implementations (each with their own tradeoffs) and still be
compatible with the existing system.

At the moment, rune is not designed to be run on an entire binary, rather it
is used to reason about smaller pieces of code iteratively and assist in
reverse engineering tasks. If you need something that can be run on an entire
binary automatically, you are better off other symbolic execution engines,
such as [angr](https://github.com/angr/angr).

__Warning__: rune is under heavy development and the API is highly unstable.
However, feel free to use rune as any comments, suggestions and feedbacks are
highly valued at this stage of the project!

__NOTE__: I am actively collecting suggestions for changes to the API. Please
open issues for the same.

## Asciinema
[![asciicast](https://asciinema.org/a/1zvz0s5wpm2gx38hp5tw6za4m.png)](https://asciinema.org/a/1zvz0s5wpm2gx38hp5tw6za4m)

## Installing
Requires:
* Standard rust toolchain. This should work with stable, but nightly is
  recommended
* Latest build of [radare2](https://github.com/radare/radare2)

Clone this repository. Then run
`cargo build`

Cargo automatically fetches the required dependencies required for this
project. To use runec, it is recommended to make a symlink to
./target/debug/runec

`ln -s ./target/debug/runec /usr/bin/runec`

## Examples
TODO

## Documentation
Documentation will be available (shortly) at: [docs]()

## Contributing
Contributing in terms of suggestions, bug-reports, blog posts and most importantly pull
requests are greatly appreciated. Unless otherwise requested by the authors,
all code will be dual-licensed under MIT and Apache Version 2.0,

To make reviews easier please ensure that the code adhers to the standard
rust style of coding.

## Project Milestones and Roadmap
Please check the [Issues](https://github.com/sushant94/rune/issues)

## License

rune is dual-licensed under:
 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

Use under either one of the above listed licenses is acceptable.
