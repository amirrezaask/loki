---
id: oimbf6i50eneexy8b8vsux9
title: Decls
desc: ''
updated: 1652397034372
created: 1652396798427
---

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