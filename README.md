# Loki

Loki is an experimental programming language that I am building both to learn more about compilers and test ideas about language design.

## Disclaimer

I have discontinued this project to work on a new compiler, this compiler was modeled mostly after [Odin](https://github.com/odin-lang/odin) and [Jai](https://inductive.no/jai/) compilers. These two projects are mostly for system level work and not
general purpose work. Since most of my experience is with backend and server stuff I know backend/server domain better so I am going to design a new language for backend stuff, syntax will stay the same mostly but semantics will change drastically.

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

- Check definitions with type hint to use correct types for defs.
- Loki main should wrap user main function so we handle return int and stuff.
- Default values for structs
- Defer: *each function that have a defer usage, will have a label at the end (with a guard) so when user does return we first jump to defer run the instructions and then we come back and return.
- Unions
- Type table in output binary + access through #type() directive that will result in the type of the expression.
- Definitions of #load that will make exported symbols accessible from a namespace.
- LLVM
- If and while and for to not require parens in parser. ( Parser has ambiguity due to expr { means initialize , so we should change initialize syntax if we want this.)


## Just Ideas to investigate
- Implicit context system
- Generics
- Pattern matching
