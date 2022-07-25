# Goals
A language to fix my problems.<br>

I use Go mainly but I want to be able to write programs for other spaces if I need like games or low level things like OS.
My problems with Go:<br>

- error handling sucks -> errors should just be normal language constructs like enums

- no low level access for when I need it -> direct memory access is a must, something like an optional GC or a high level allocator

- generics sucks ?? Generics should not have any runtime overhead -> pure monomorphization is the way

- reflection sucks ?? do we really need reflection if we have enough power at compile time ?

- type system sucks:
	- no enum/unions
    - pattern matching is not present


# Solutions

- Enums

- Unions

- Errors should be any kind of value but by practice tagged unions.

- There should be an option for manual memory management.

- Generics should be implemented through monomorphization.

- Pattern matching
