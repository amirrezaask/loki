# Loki
Loki is an experimental programming language that I am building both to learn more about compilers and test ideas about language design.

## Disclaimer
I have many lessons learned from Loki project and I am going to use them in my future compilers but loki and the current code base is
not something I want to spent time anymore. I want to come up with a design for a specific use case and not a general purpose langauge.
## Installation

```bash
git clone https://github.com/amirrezaask/loki.git
cd loki
cargo install --path .
# if you have added cargo bins inside your path you can use loki command.
```

## Features so far
- functions ( local functions / closures WIP )
- control flows ( if , for , while )
- file loading ( in a semantic way not preprocessor shit )
- type inference ( cannot handle circular dependency but that's all go has anyway so I think good enough )
- loading C headers and using them ( look into samples/ncurses.loki that uses ncurses library )
- CPP codegen

