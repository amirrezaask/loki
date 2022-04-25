# Loki
Loki is a programming language which is designed to be hosted from start. You can choose between multiple backends available, any one that is more
appropriate for you.

## Compiler Backends
- C: Clang + Zig Build System
- Go 
- JS

## Syntax
Loki tries to be as minimal in syntax but also being verbose for readability and takes lots of inspiration from Go and Zig.

### Hello World
```
fmt = import("std/fmt"); 

main = fn() void {
  fmt::println("Hello World From Loki");
}
  
```