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
- Bytecode: lowering language features into simpler constructs happen here.
- Backend code generation: this will produce code that gets compiled or assembled using a third party toolchain.
  * C: almost done.
  * LLVM IR: Comming soon!

## Todos

- check definitions with type hint to use correct types for defs.
- loki main should wrap user main function so we handle return int and stuff.
- default values for structs
- defer: *each function that have a defer usage, will have a label at the end (with a guard) so when user does return we first jump to defer run the instructions and then we come back and return.
- unions
- definitions of #load that will make exported symbols accessible from a namespace.
- LLVM
- if and while and for to not require parens in parser. ( Parser has ambiguity due to expr { means initialize , so we should change initialize syntax if we want this.)


## Just Ideas to investigate
- Implicit context system
- Generics
- Pattern matching

