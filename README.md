# Loki
Loki is an experimental programming language that I am building both to learn more about compilers and test ideas about language design. 

## Compiler Flow
![compiler flow](https://github.com/amirrezaask/loki/blob/master/compiler_flow.png?raw=true)
```mermaid
graph compiler_flow;
A[Tokenize] -- Produces a token stream --> B[Parser] -- Produces an AST --> C[Semantic Analyzer] -- Produces AST --> D[Generate Typed IR];

```

## Syntax
Loki tries to be as minimal in syntax but also being verbose for readability and takes lots of inspiration from Go and Zig.

### Hello World
```
fmt = import("std/fmt"); 

main = fn() anyerror|void { // an
  return fmt::println("Hello World From Loki");
}
  
```
## Ideas
- Loki code is *ALWAYS* just set of expressions that evaluate to a value and *ALL* expressions output must be captured.
- Error handling is done through sum types, each failable function should return a union type of the output type and error type.
```
  MathErrors = enum {
    DivByZero,
  };

  div = fn(a int, b int) MathErrors|float64 {
    return if b == 0 {
      MathErrors::DivByZero
    } else {
      a / b
    }
  }
```
- Types are first class values, basically compile time constants, so types are treated just like other values.
```
  Animal = interface {
    eat()
  };
  
  Cat = struct {
    eat = fn (self: Self) void {
      
    }
  };
  
  main = fn() void {
    cat Cat = .{};
    cat.eat();
  };
```

- Interface implementaions are implicit.

## Platforms
Loki tries to give same experience on all supported platforms but it's not always possible, so in the standard library and any other loki code there 
can be checks(comptime if) so if a certain code cannot be compiled against target compiler just removes it and if you use a code which can not be used
in your target compiler will emit an error.


## Interopability
Loki has a direct interface to it's host programming languages so you can call directly to their libraries and apis. Both standard library and third party ones
```
  std = import("std");

  host = std::host
  goFmt = import("go:fmt");
  
  main = fn() void {
    if host.isGo() {
      goFmt.Println("from go code inside loki hello"); // calls golang fmt.Println
    } 
  }
  
```

## Build tool 
Loki has it's own build tool which is a wrapper and generator for it's target build tools. It will generate necessary files and set necessary env values
from your loki.yml file.
```
author: "amirrezaask"
name: "sample loki project"
deps:
  loki:
    - github.com/a1/lib1
  go:
    - github.com/lib/pq
```

## Compiler Flow
Loki does this on every call to compile
1. read a source file
2. tokenize
3. turn token stream into AST
4. optimize codes and expand syntactic sugars
5. semantic analyze and validation
6. pass shaved AST to code generation backend
7. after code generation setup env and build generated code
