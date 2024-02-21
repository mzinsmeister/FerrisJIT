# Beginning of an experimental copy-and-patch like compiler backend written in Rust

This is supposed to become an experimental compiler backend based on [Copy and Patch](https://fredrikbk.com/publications/copy-and-patch.pdf) to be (maybe) later used in my OxidSQL Database for a compiling query engine. It might be the first copy and patch experiments (at least in a public repo) in Rust. I will try to use LLVM directly for generating the stencils instead of going through the kind of ugly C++ templates described in the original paper. The idea would be to use the existing codegen primitives in your database system that can already generate LLVM IR and use those to generate stencils. One could then also copy and patch these stencils together using those same primitives.

As a first step i will try to generate a few simple addition stencils and patch those together. As soon as that works, the hardest part should be done and it should just be a matter of adding more stencils and patching them together correctly (famous last words maybe).

## Current state

Automatic stencil generation for integer-types and integer-operations should be working. 

Basic expressions with arbitrary integer arithmetic should almost work. The only thing still missing is a stencil
for taking two values from the stack and calling another stencil with them. For now arbitrary constant operations
are working however. The hardcoded example currently does a hardcoded addition of 10 followed by a multiplication by 2. What's still missing is a stencil for shifting a value from first register to second operand and some other "glue"-stencils like that.

## Next steps

Next steps would be automatic stencil generation for all kinds of different data types with and without operands in registers and for control flow constructs (if, if/else, loops). After that i will try to create CodeGen Primitives and use those to generate stencils, generate full llvm ir or generate copy-patched code. This should then also allow to use some kind of AST or internal IR to do this generation.