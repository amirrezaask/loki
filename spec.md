# Loki Spec
## Declarations
Every thing in Loki is just a declaration, functions, variables, types everything is *JUST* declaration.
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
  
  // using 
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
  type1!type2 // union type shorthand syntax

  
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

## Loops
Loki has only one loop, *for*
```
  // for has 3 syntaxes for different scenarios
  for i=0;i<10;i++ {} // C-style for loop
  
  for (_, elem) in [array or slice] // for-each style syntax
  
  for condition {} // while syntax
  
```


## Conditionals
If conditions *only* accept boolean value or option values nothing else. It will check option value for being null.
```
  a: int32? = null;
  if a {
    
  } else {
    
  }
  
```


## Interop
Since Loki is designed to be a transpiler to other languages it should have clear simple way to interact with host ecosystem. For example 
if you use Go backend:
```
  http = import("go:net/http");
  fmt = import("go:fmt");
  
  main = fn() void {
    http.HandleFunc("/", fn(w http.ResponseWriter, req *http.Request) void {
      fmt.Fprintf(w, "Hello Guys"); 
    });
    
    http.ListenAndServe(":8080", NULL);
  }
```