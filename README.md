# Loki

Loki is an experimental programming language that I am building both to learn more about compilers and test ideas about language design. 

## Installation
```bash
git clone https://github.com/amirrezaask/loki.git
cd loki
zig build
```
## Compiler Flow
```mermaid
graph TD
    A[Tokenizer] --> |Produces stream of tokens| B[Parser]
    B --> |AST| C[Semantic analyze/Type inference]
    C --> |AST| D[Target Code Generator]
    D --> |Target Project| E[Target Toolchain]
```

## [Language Spec](https://github.com/amirrezaask/loki/tree/master/spec.md)

## Plans
- C Backend as a working solution for compiling to native code.
- SSA IR
- SSA Optimizations
- LLVM Integration
