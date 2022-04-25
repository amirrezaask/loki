# Loki
Loki is a programming language which is designed to be hosted from start. You can choose between multiple backends available, any one that is more
appropriate for you.

## Compiler Backends
- Zig
- Go 
- JS

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
    cat = Cat{};
    cat.eat();
  };
```

- Interface implementaions are implicit.