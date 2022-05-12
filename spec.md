# Loki Spec
# Declarations
In loki most of the syntax is around declarations, general syntax
for decls is:
```
name: type = expr;
a: int = 2;
b: float = 2.2;
c: string = "hello";
d: bool = true;
sum = fn(a: int, b: int) int { // function decls now only can be top level
    return a + b;
}
```
*NOTE:* Type inference is going to be in loki eventualy.
there is no other syntax for decls.

# Imports
Since loki is designed to be hosted on different platforms so imports have important role in here loki has 2 behaviour in terms of imports: 
- import "***.loki" => includes a loki source file in compilation,
- import "" => uses host import system to import.


## Loops
Loki has only one loop, *for*
```
  for (i=0;i<10;i++) {} // C-style for loop

  for (condition) {} // while syntax
```


## Conditionals
If conditions *only* accept boolean value, nothing else. conditionals also are expressions.
```
  b = if true {
    2
  } else {
    3
  };
```
## Types
Loki has a simple type system, mainly because we need to map these into C type system and other hosts.
```
// Numerics
int, uint, float

char, string

bool

struct {
  field: ty,
}

enum {
  var1,
  var2
}

union {
  field: ty
}

```
Loki types are just first class values in the eyes of parser so all type definitions are like other decls. 

## Interop
Since Loki is designed to be a transpiler to other languages it should have clear simple way to interact with host ecosystem. For example 
if you use Go backend:
```
import("std/target.loki");
if std.host.name == "go" {
  http = import("go:net/http");
  fmt = import("go:fmt");

  main = fn(): anyerror!void {
    try http.HandleFunc("/", fn(w http.ResponseWriter, req *http.Request) void {
      fmt.Fprintf(w, "Hello Guys"); 
    });
  
    try http.ListenAndServe(":8080", NULL);
  }
}
```

## Compile Time Code Execution
Since types are first class, we need to evaluate constant values at compile time and since we are doing that let's do 
it for everything, so if/for/fn calls that has no dependency to runtime should just be executed at compile time.
```
target = import("std/target.loki");

main = fn() void {
  // since target is known at compile time we can just simplify this block and remove dead branches
  if target.kind == target.Targets.go {
  } else {
  }
}  
```

## Error handling
Error handling looks much like Zig and Go, Errors are just values like anything else and they are not even specific type most of the times they are going
to be enums or unions.by convention failable functions return a tuple, first type is error type, second type is the function return value.
```
  errors = enum {
    div_by_zero,
  };
  
  main = fn() errors!void {
    return div(2,0).!; // .! special syntax generates an if condtion that checks first for first element of tuple if nothing then continue else return error.
  }
  
  div = fn(a int, b int) errors!float64 { // the return type is equivalent to union {errors, float64}
    if b == 0 {
      return errors.div_by_zero;
    }
    return a / b;
  }
```

