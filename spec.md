# Loki Spec
# Declarations
## Constants
In loki most of the syntax is around declarations, general syntax
for decls is:
```
const ident: type? = expr;
// examples
const f = 2;
const f: uint = 2; // you can also include a type between colons but it's optional
const sum = fn(a: int, b: int) int {
    return a + b;
}
```
Note that all expressions must have a compile-time known type.

## Variables
```
var ident: type? = expr;
```
# Imports
```
import "std:fmt" [as ident]; // for backends who support import namespaces
```


## Loops
Loki has only one loop.
```
  for (i=0;i<10;i++) // c style for loops
  for (condition) // while style
  for (item in collection) // collection should be either an array or a pointer to it.
```


## Conditionals
If conditions *only* accept boolean value, nothing else. conditionals also are expressions.
```
  b :: if true {
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
