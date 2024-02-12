# Beginning of an experimental copy-and-patch like compiler backend written in Rust

This is supposed to become an experimental compiler backend based on [Copy and Patch](https://fredrikbk.com/publications/copy-and-patch.pdf) to be (maybe) later used in my OxidSQL Database for a compiling query engine. It might be the first copy and patch experiments (at least in a public repo) in Rust. I will try to use LLVM directly for generating the stencils instead of going through the kind of ugly C++ templates described in the original paper. The idea would be to use the existing codegen primitives in your database system that can already generate LLVM IR and use those to generate stencils. One could then also copy and patch these stencils together using those same primitives.

As a first step i will try to generate a few simple addition stencils and patch those together. As soon as that works, the hardest part should be done and it should just be a matter of adding more stencils and patching them together correctly (famous last words maybe).

## Current state

Currently i can generate functions with an arbitrary number of constant additions to a single value. The value is currently passed to the function through a stack pointer which also holds the result after execution.

## Next steps

Next steps would be automatic stencil generation for all kinds of different data types with and without operands in registers and for control flow constructs (if, if/else, loops). After that i will try to create CodeGen Primitives and use those to generate stencils, generate full llvm ir or generate copy-patched code. This should then also allow to use some kind of AST or internal IR to do this generation.