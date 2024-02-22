# Beginning of an experimental copy-and-patch like compiler backend written in Rust

This is supposed to become an experimental compiler backend based on [Copy and Patch](https://fredrikbk.com/publications/copy-and-patch.pdf) to be (maybe) later used in my OxidSQL Database for a compiling query engine. It might be the first copy and patch experiments (at least in a public repo) in Rust. I will try to use LLVM directly for generating the stencils instead of going through the kind of ugly C++ templates described in the original paper. The idea would be to use the existing codegen primitives in your database system that can already generate LLVM IR and use those to generate stencils. One could then also copy and patch these stencils together using those same primitives.

As a first step i will try to generate a few simple addition stencils and patch those together. As soon as that works, the hardest part should be done and it should just be a matter of adding more stencils and patching them together correctly (famous last words maybe).

## Current state

Automatic stencil generation for integer-types and integer-operations should be working. Expression evaluation in basic cases works but there might still be bugs. Register utilization/caching could definitely still be improved but it works for now.

## Next steps

Next steps would be control flow constructs and beeing able to generate llvm ir for the entire expressions using the same code that generates the stencils.

## Useage

To run the example, just run `cargo run`. Optionally a `-c` flag with a path to a csv file can be passed to run the example with the given csv file as input. 

```bash
cargo run -- -c test.csv
```

If no file is given, the example will be run on a hardcoded sequential element array. Then you can enter expressions in lisp-like syntax. By default you will be in interactive mode and the result per input-line will just be printed. You can also pass `-b` as a flag to be in benchmark mode. Make sure to run with `--release` in this case. This will then print out the timings for compiled vs interpreted (when using generated input the input size will be 10,000,000 in this case, otherwise 10). For example:

Constant result:
```lisp
(+ 10 (* 2 3))
```

Variable result (variables are just named $0, $1, ... corresponding to the column in the csv):
```lisp
(+ 10 $0)
```

Expressions can also be nested arbitrarily.
```lisp
(+ (+ 10 2) (* 2 (+ 3 4)))
```

## Example Results

These are initial results from my Laptop:

```lisp
>> (+ 2 $0)
Interpreted: 232.118723ms
Compiled: 43.866889ms
>> (+ 2 (* 2 $0 (+ $0 (* 3 $0))))
Interpreted: 795.58322ms
Compiled: 49.456688ms
```

If you want to test against a hardcoded Rust expression you can just hack that expression into the main.rs file at the line where it says 
```                            
//--- INSERT YOUR HARDCODED EXPRESSION EVALUATION HERE ---
```
and uncomment the commented-out lines around it.

