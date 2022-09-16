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

    * For -> While

    * Foreach -> While

    * Initialize structs -> Declarations and assignments.

    * Initialize arrays -> Declarations and assignments.

    * Enums -> normal constants.
- backend code generation: this will produce code that gets compiled or assembled using a third party toolchain.

    * C: almost done.

    * Go: Comming soon!

    * LLVM IR: Comming soon!

    * WASM: Comming soon!


## Todos

- branch implementation in bytecode.
- Internal array and dynamic array implementation as a generic data structure.
- defer: *each function that have a defer usage, will have a label at the end (with a guard) so when user does return we first jump to defer run the instructions and then we come back and return.
- generics: take a look into generics.loki.notyet
- trasnform ForIn => while
- unions
- definitions of #load that will make exported symbols accessible from a namespace.
- LLVM
- dynamic array:
  * strings should be our own dynamic array impl.
  * variadic args
  * slicing arrays
  * converting a dynamic array into c variadic array.
- load files into a namespace
- memory allocators