# New LOKI compiler design

Lexer -> Parser -> IR(untyped) -typer-> IR(typed) -sema check-> IR(typed + semantically checked) -bytecode-builder-> bytecode -> Code Generator Backend.

- generics
- function overloading
- default values for function arguments and structs and enums
- file loads as namespace values