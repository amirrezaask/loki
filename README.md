# Loki

Loki is an experimental programming language that I am building both to learn more about compilers and test ideas about language design.

## Installation

```bash
git clone https://github.com/amirrezaask/loki.git
cd loki
cargo install --path .
# if you have added cargo bins inside your path you can use loki command.
```

## How It works

Loki compiler consists of multiple stages of compilation:

- Lexer: produce a stream of tokens for each input file.
- Parser: produces an IR structure for each file.
- Type checker and inference: type inference and checking of the IR happens here.
- internal bytecode: lowering language features into simpler constructs happen here.
    . For -> While
    . Foreach -> While
    . Initialize structs -> Declarations and assignments.
    . Initialize arrays -> Declarations and assignments.
    . Enums -> normal constants.
- backend code generation: this will produce code that gets compiled or assembled using a third party toolchain. (C compiler, LLVM, machin assembler).