# New LOKI compiler design

Lexer -> Parser -> HIR -(Type checking/inference + Semantic analyzer)-> MIR -(lower features)-> LIR -> Code Generator Backend.

HIR: is our high level IR which is just parsed and very little type information is known at this stage.
MIR: When HIR goes through our MIR builder it will produce MIR which is fully typed (if cannot be fully typed it's an error), also MIR builder does semantic checking. MIR handles generics as well for both structs and function calls.
    - generics are similar to the previous version #cast.
LIR: When MIR goes through our LIR builder it will produce LIR which is as simple and low level as byte codes and assembly, very few constructs are available in this format.