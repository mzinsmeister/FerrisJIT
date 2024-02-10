# Beginning of an experimental copy-and-patch like compiler backend written in Rust

This is supposed to become an experimental compiler backend based on [Copy and Patch](https://fredrikbk.com/publications/copy-and-patch.pdf) to be (maybe) later used in my OxidSQL Database for a compiling query engine. It might be the first copy and patch experiments (at least in a public repo) in Rust.

As a first step i will try to generate a few simple addition stencils and patch those together. As soon as that works, the hardest part should be done and it should just be a matter of adding more stencils and patching them together correctly (famous last words maybe).

## Current state

What currently works is generating a simple function using inkwell (adding a constant to a 64 bit integer passed as a function argument), reading the ELF file using goblin, patching in a simple constant for the addition and executing that. What's still missing is the actual part of patching together multiple stencils (and even having multiple stencils). This also means i still have to figure out how to do the tail call, stack and register stuff.
