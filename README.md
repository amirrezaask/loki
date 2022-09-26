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
  * C: Done.

## Todos

- Loki main should wrap user main function so we handle return int and stuff.
- Default values for structs
- Defer: *each function that have a defer usage, will have a label at the end (with a guard) so when user does return we first jump to defer run the instructions and then we come back and return.
- Unions
- Type table in output binary + access through #type() directive that will result in the type of the expression.
- Definitions of #load that will make exported symbols accessible from a namespace.
- LLVM
- If and while and for to not require parens in parser. ( Parser has ambiguity due to expr { means initialize , so we should change initialize syntax if we want this.)
- Generics
