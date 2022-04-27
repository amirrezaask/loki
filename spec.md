# Loki Spec
## Declarations
Every thing in Loki is just value and values can bind to names using declarations. delarations by default are immutable unless explicitly
annotated using `mut` keyword. functions, variables, types everything is *JUST* declaration.
```
  [mut] [name]: [type] = expression 
```
```
  y = 3;

  slice = [1,2,3,4];

  // defining interface
  i = interface{
    eat()
  }; 
  
  z = if true {
    2
  } else {
    5
  };

  // defining a function
  f = fn(x int, y int) int {
    x + y
  };

  // defining struct  
  s = struct {
    a int,
    b int,
  };
```


## Loops
Loki has only one loop, *for*
```
  // for has 3 syntaxes for different scenarios
  for i=0;i<10;i++ {} // C-style for loop
  
  for (_, elem) in [array or slice] // for-each style syntax
  
  for condition {} // while syntax
  
```


## Conditionals
If conditions *only* accept boolean value or option values nothing else. It will check option value for being null. conditionals also are expressions.
```
  a: int32? = null;
  if a {
    
  } else {
    
  }
    
  b = if true {
    2
  } else {
    3
  };
```
## Types
You can see all types available in Loki down here
```
  int8, int16, int32, int64, isize

  float32, float64
  
  [size]type  // array type

  []type // slice type, which is basically a reference to somewhere in an array + size

  map[key_type]value_type // hashmap  

  (type1, type2, type3) // tuples which are basically auto generated structs.

  type1? // optional type can be null, other values cannot be null

  type1!type2 // tuple shorthand equivalent to (type1, type2)
   
  type // type itself is a type :) since types are first class values and you can work with them like any other value.
  
```
Loki also has complex data structures.
```
  struct {
    field type,
  }
  
  
  interface {
    method_name()
  }
  
  union {
    field type,
  }
  ?? can we make these two one type ?
  enum {
    
  }
```

## Module System
Each file is a module, Each directory is also module, and modules are just a special struct :).
```
  // imagine you have a math.loki file
  // or a struct called math
  // or a file called math/module.loki 
  // they are all same thing

  math = import("math");
  math = import("math.loki");
  math = struct {};
```

## Interop
Since Loki is designed to be a transpiler to other languages it should have clear simple way to interact with host ecosystem. For example 
if you use Go backend:
```
  if import("std/target").name == "go" {
    http = import("go:net/http");
    fmt = import("go:fmt");
  
    main = fn() anyerror!void {
      try http.HandleFunc("/", fn(w http.ResponseWriter, req *http.Request) void {
        fmt.Fprintf(w, "Hello Guys"); 
      });
    
      try http.ListenAndServe(":8080", NULL);
    }
  }
```
note that if conditions that have a constant compile time know condition can be reduced to the only live branch.

Or C backend can use manual memory management instructions so:
```
  c = import("c:c");
  main = fn () void {
  // psuedo code
    c.malloc();
    c.free();
  }
```

## Compile Time Code Execution
Since types are first class, we need to evaluate constant values at compile time and since we are doing that let's do 
it for everything, so if/for/fn calls that has no dependency to runtime should just be executed at compile time.
```
target = import("std/target");

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

