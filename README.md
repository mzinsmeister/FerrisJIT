# Experimental Copy-and-Patch compiler backend written in Rust

<p align="center">
    <img src="ferrisjit-portable-min.svg" alt="" width="350px"></img>
</p>


This is supposed to become an experimental compiler backend based on [Copy and Patch](https://fredrikbk.com/publications/copy-and-patch.pdf) to be (maybe) later used in my OxidSQL Database for a compiling query engine. It might be the first copy and patch experiments (at least in a public repo) in Rust. I will try to use LLVM directly for generating the stencils instead of going through the kind of ugly C++ templates described in the original paper. The idea would be to use the existing codegen primitives in your database system that can already generate LLVM IR and use those to generate stencils. One could then also copy and patch these stencils together using those same primitives.

## Current state

Automatic stencil generation for integer-types and integer-operations aswell as pointers on them should be working. Even control-flow should be working now but generates a lot of stack/register movements that are somewhat unnecessary. A conditional move stencil and the corresponding abstractions around it could massively speed this up. The abstraction created is already quite nice i think. There's stuff like operator overloading so that you can add two codegen Values together and so on.

The *llvm-gen* branch contains a version that should be able to generate LLVM IR with identical semantics on the fly too. This was not merged into main because it massively slows down the codegen time and there's currently no way to measure the copy and patch compilation time separately from the LLVM IR generation time or the general code generation time. You can, however, roughly assume that between 50 and 80% of the time the tool outputs for codegen is not actually spent on the copy and patch stuff. You can get a rough idea about this by using the *mocked_out_codegen* branch.

## Next steps

Next steps would be to refactor stuff a bit and create a generic interface for backends such that you can add and remove new backends more easily and, more importantly, also decide which ones you actually want to use for a single compilation step.

## Try it!

### Setup

To run this example you need to be running on a Linux machine (i think so at least) and have llvm-17 installed. To install llvm-17 on Ubuntu you can run:

```bash
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 17
```

### Usage

To run the example, just run `cargo run`. Optionally a `-c` flag with a path to a csv file can be passed to run the example with the given csv file as input. 

```bash
cargo run -- -c test.csv
```

If no file is given, the example will be run on a hardcoded sequential element array. Then you can enter expressions in lisp-like syntax. By default you will be in interactive mode and the result per input-line will just be printed. You can also pass `-b` as a flag to be in benchmark mode. Make sure to run with `--release` in this case. This will then print out the timings for compiled vs interpreted (when using generated input the input size will be 1,000,000 in this case, otherwise 10 by default. You can set this number using the `-n` flag). 

#### Currently Supported Operations

All constants and variables must be 64 bit signed integers or boolean #t/#f for true/false.

For Expressions the following operations are permissible:

Arithmetic Operations:
* `+` Addition
* `-` Subtraction
* `*` Multiplication
* `/` Division

Integer Comparison Operations:
* `=` Equality (produces a boolean result)
* `!=` Inequality (produces a boolean result)
* `>` Greater than (produces a boolean result)
* `<` Less than (produces a boolean result)
* `>=` Greater than or equal (produces a boolean result)
* `<=` Less than or equal (produces a boolean result)

Boolean Operations:
* `=` Equality
* `!=` Inequality


To add to that there's also aggregate functions to use before the expression:

* `SUM` Sum of all results

#### Examples

The simplest thing you can do is just evaluate expressions on every row:

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

You can also filter the rows by adding `WHERE` and a boolean expression after that. For example (works on test.csv):

```lisp
(+ 2 $0) WHERE (> $1 3)
```

to add to that you can also aggregate the result (works on test.csv):

```lisp
SUM $0 where (& (> $3 100) (> $1 15))
```

or 

```lisp
SUM (+ 2 $0) where (| (> $3 100) (> $1 15))
```

or much more complex like this:

```lisp
 sum (- (+ (+ $0 4) (- 2 10)) (+ (+ (+ (* (+ 1 (+ 6 10)) $0) $0) (+ (+ (+ (* (- 8 9) $0) 3) (- 48 (- 10 7))) $0)) (* (+ (- 3 8) (+ $0 5)) (* (- $0 (- 5 4)) 3)))) where (& (< (- (+ (+ $0 4) (- 2 10)) (+ (+ (+ (* (+ 1 (+ 6 10)) $0) $0) (+ (+ (+ (* (- 8 9) $0) 3) (- 48 (- 10 7))) $0)) (* (+ (- 3 8) (+ $0 5)) (* (- $0 (- 5 4)) 3)))) 10000) (> (* (- (* 1 (- (- 6 8) $0)) $0) (+ $0 (+ 7 (+ $0 10)))) 15))
```


**Multiple aggregates and more aggregate functions coming soon**

## Example Results

These are initial results from my Laptop (they might not be the newest versions. I had to cut some of the optimizations to make control-flow work):

```lisp
>> (+ 2 $0)
Generated 44 bytes of x86-64 binary in 16.468µs
Interpreted: 75.127763ms
Compiled: 4.031871ms
Compiled is 18.63x faster
>> (+ 2 (* 2 $0 (+ $0 (* 3 $0))))
Generated 109 bytes of x86-64 binary in 23.815µs
Interpreted: 297.278796ms
Compiled: 3.938077ms
Compiled is 75.49x faster
```

### Very long examples

I'm pretty sure the majority of the time is actually not even the evaluation but just writing the input into the stack and the function calls, e.g. for the ghc-calling-convention wrapper. My experiments showed that just the two function calls (to the ghc-cc wrapper and the generated function) cost 2/3s of the time for a simple expression like `(+ 2 $0)`. Once we have control flow constructs we can just put a pointer to the input onto the stack and loop through it inside the generated code. 

For now we can just use very large expressions to get a better idea of the relative performance. Large expressions can be generated with the gen_expr.py (enter a number for complexity as first argument). This result is also not necessarily the newest version. Note that my Laptop goes into thermal throttling after a few seconds of 100% CPU usage so the real difference will be less. Although i think between 80-150x is realistic.

For example (complexity 1000; this will overflow and therefore only work on `--release` mode):

```lisp
>> (+ (+ (+ (- (* (- (+ (* 3 (* 4 1)) $0) (* (- (* (- $0 1) $0) 83) (* (* 2 1) 4))) (- (* (+ (+ (+ 2 4) (+ (- $0 5) (* $0 3))) (+ (* (* $0 5) (- (+ $0 7) (+ 7 3))) (* (+ (* $0 9) (+ (* 5 1) $0)) 1))) (* 1 9)) (- (+ (+ (+ (+ 1 9) $0) (+ (+ (+ 5 (- 8 8)) (* $0 10)) (* (* 4 8) (* $0 10)))) 7) (* (* (* (* (+ (- 10 7) $0) 3) $0) $0) 4)))) (+ (+ (- (- $0 7) (- 1 8)) (- (+ (+ (+ 1 4) (* (- $0 7) (* $0 1))) $0) (+ (- 5 1) (+ (+ $0 (- $0 (* 10 4))) (* 9 1))))) (* (+ $0 8) (+ (- (- $0 3) $0) (* (+ 1 (- (+ $0 4) 58)) (* (+ $0 10) $0)))))) (- (+ (+ (+ (+ (- (* $0 8) 18) $0) (+ (+ (+ (+ (- (* $0 8) $0) $0) $0) (- 3 4)) (- (+ (* (+ (- 9 9) (* 7 2)) (* $0 6)) 8) (+ (* 1 (+ (* $0 6) (+ 10 (- 1 2)))) (* (- $0 5) 2))))) (* (- (+ (+ (- $0 6) (+ (* 2 4) (+ (+ $0 (- $0 2)) 8))) (+ (* 7 7) 3)) $0) (* (* (- 25 (- 1 2)) $0) 3))) (+ (- (- (* (+ 6 10) (* (+ (- $0 8) (- $0 6)) 2)) (+ (- $0 8) $0)) (+ (- (+ (+ (- (+ (* 8 4) (- 10 10)) $0) $0) $0) 60) 8)) (- (+ (- 6 8) (- $0 2)) (* (* 3 1) (- 1 10))))) (+ (+ (+ $0 (+ (+ (+ (- (- (* (* 6 7) 4) (+ (- $0 (- $0 10)) $0)) (- (+ 4 2) (* 9 4))) 6) 10) (- (* (* (* $0 4) 3) (* $0 3)) 10))) (- (* (- 1 6) (- (+ (+ $0 3) (+ (+ $0 10) (+ (+ (+ (+ 10 1) (- 4 6)) (+ (* 8 7) (- $0 4))) (+ (* 8 1) (+ $0 9))))) (+ (+ $0 (+ (+ 2 5) $0)) (+ (+ 8 8) (* 10 3))))) (* (* (+ 6 3) 2) (- $0 10)))) (* (+ (- (+ (+ 10 3) (* $0 1)) (+ $0 5)) (+ (+ (+ (* (* $0 8) (- 2 3)) (* $0 2)) (- (- (- 5 2) (* 3 4)) $0)) (+ (* (- (- 4 9) (- 4 7)) 3) (+ $0 7)))) (* (* (- $0 (- $0 8)) (* $0 7)) 3))))) (- (- (- (+ 4 1) 7) (+ (* (+ (- (+ (* 7 8) 4) 9) (* (- (+ (- 3 7) (* $0 9)) $0) (- $0 (- 2 7)))) $0) 7)) (- (+ (* (+ (- (- (+ (* 4 (+ 5 10)) (+ (+ $0 10) 5)) (* 6 10)) (+ $0 9)) (+ (* (* $0 5) (+ (- $0 10) (+ (* $0 7) (+ (+ (* 9 8) 6) (+ 4 3))))) $0)) (+ (+ (- (* (* 2 (+ (* 9 5) $0)) (+ (* (* 3 3) $0) (* (* $0 2) (+ 8 4)))) (- (+ (- (- 10 10) (* (* $0 2) (+ 5 10))) (+ (+ 4 5) (* $0 (- $0 6)))) (* (+ $0 5) 4))) (* (* (+ 10 (+ 3 5)) (+ (+ (* (* 1 7) $0) $0) $0)) (+ (- (* (+ 3 5) (+ $0 7)) (+ (+ (* $0 9) (* 8 8)) (+ (* $0 4) (+ 6 7)))) (+ $0 7)))) $0)) (+ (* (- (+ (- (- 6 9) (* 5 4)) (* (+ (+ 9 (* 1 5)) 3) (* 1 (* (+ (- $0 7) 9) $0)))) (+ (+ (* (+ $0 3) (* (* (- $0 6) (* 9 3)) $0)) (* (+ 6 (+ (- 22 (+ 10 5)) (* (* 6 10) (* (* $0 3) 1)))) (+ (- $0 10) 8))) (+ 2 2))) (- (+ (+ (+ (- $0 3) (- (+ $0 3) 41)) (- 4 6)) (+ 1 (+ (- (+ $0 (+ $0 10)) 2) (+ $0 (+ $0 9))))) (- 8 1))) (* (+ (+ (+ (* 4 9) 9) (+ (+ 10 7) (- (- 5 6) $0))) (+ (+ (* (+ (- $0 1) $0) (+ (+ (* $0 9) (* 5 2)) (+ (- $0 10) $0))) (- (+ $0 8) (* (* 1 2) (+ 3 6)))) (- (* (+ (- (* (* $0 5) (- (- 8 5) 18)) 63) (- (- $0 6) (- (+ $0 8) 1))) (+ (+ (* $0 (+ 2 4)) (- 23 (* (* (- 22 (+ (- $0 4) $0)) $0) (+ $0 1)))) 4)) (- 44 (+ (+ (* 9 7) (* (+ 8 (- 26 (* $0 10))) $0)) (+ 2 (- (+ $0 3) $0))))))) (+ (- (+ (- $0 6) (* $0 4)) 72) (+ (* (+ 10 6) (- 10 9)) (- (+ 10 2) (* $0 8))))))) (* (- (- (- $0 (* (+ (* (+ 3 8) (- $0 9)) (* $0 2)) (* (+ $0 2) $0))) (- (- $0 9) 88)) 76) (+ (* (* $0 6) (+ (+ (- (* $0 7) (- $0 2)) 6) (+ $0 2))) (- (- 10 1) (* (+ 7 (- 7 10)) (+ (- (- 4 10) 82) $0)))))))) (- (+ (* (- (+ (- 29 (+ $0 1)) (+ (+ (- (* $0 8) (+ 8 7)) $0) (- (* (- $0 9) (+ (- 10 9) (+ 5 3))) (+ (* (+ $0 2) $0) 9)))) (* (+ (- (+ (+ (+ (+ 1 9) (* $0 3)) (* 5 1)) (- (+ (- 3 9) $0) $0)) $0) (* (+ (* (- (- (* 2 2) 39) (* $0 5)) (* $0 9)) (* (* $0 9) (* (+ 6 8) (- (+ (+ (* 7 6) $0) (+ (* (+ (* $0 8) 3) $0) $0)) $0)))) (- (- (* (- $0 2) 3) (- 4 2)) (+ 6 (- (* $0 3) 68))))) (+ (- (- $0 7) (- (+ 2 (- 5 10)) $0)) (+ $0 (+ (- $0 5) (+ 2 (* 4 5))))))) (* (+ $0 (+ (+ (+ (* 7 10) 7) (* (+ $0 10) (* (- (- 7 4) $0) (+ (* $0 2) $0)))) (+ (- (- (* 2 5) $0) 31) 7))) (* (* (- (- (+ $0 5) (- 9 5)) (- (* (* (+ (- $0 9) $0) (- $0 4)) (* (+ (+ (* 2 (+ (+ $0 8) (- (+ 9 2) (- 1 8)))) (* (+ $0 (* 1 8)) 2)) $0) (+ 1 (* 3 7)))) (- (+ (- 20 (* 10 9)) $0) (- $0 3)))) (* 1 (- (* 7 10) (+ 1 (+ $0 2))))) 3))) (- (- (+ (+ (- 65 (- (* 10 9) (- $0 1))) (- $0 (- (- (- 5 4) (+ (* $0 6) (+ $0 10))) (* (+ (* 2 (+ $0 5)) (* 4 8)) (- 14 (* $0 9)))))) (* (+ (- (+ (* (- 3 6) (- $0 (* (+ (- 4 10) 8) (+ $0 4)))) $0) (* 5 3)) (- (- (* (- 1 5) (- 5 8)) $0) (+ (* (+ (- 4 1) (+ 2 7)) (- (+ $0 (- (- 6 1) $0)) (+ $0 5))) (+ $0 7)))) (+ (+ (- 5 9) 2) (+ (+ $0 (+ $0 4)) 7)))) (+ (+ (+ (+ (+ (- (+ (+ (+ $0 7) 2) (- (+ (* (- 6 3) $0) (* (+ 6 8) $0)) $0)) (+ (+ $0 (+ 8 6)) (+ 5 (+ 5 1)))) (- (+ 1 1) $0)) (+ (+ (+ 4 5) (+ $0 8)) (+ (- (+ (* 1 (- (+ 5 6) (+ (- $0 8) $0))) (+ 3 (* 8 4))) (* (- $0 6) (+ (* 5 4) 7))) (+ (+ (+ $0 (- (- 8 3) (* 7 8))) (+ (- (+ (* 9 10) 6) (- (- 2 10) (- (+ (+ $0 4) $0) (+ (* 9 7) 2)))) (+ $0 (* 4 6)))) 3)))) (* (+ (+ (* (* (+ (- $0 5) (* $0 4)) (+ (* $0 6) 5)) (+ (+ (+ 10 8) (* $0 4)) $0)) (+ (- (- $0 8) 87) (* 7 6))) 2) (+ (- (+ (- $0 (* 4 9)) (- $0 (* $0 4))) (+ (+ (+ (* 4 5) (* $0 7)) (- (+ (+ $0 1) 8) $0)) (- (+ (+ (- (* $0 10) $0) 8) (+ (* $0 2) (- 1 8))) 91))) (- (+ (- (- (* (+ (+ $0 (* (+ (* 2 4) (* 2 3)) 2)) (+ 8 (- 5 5))) (- (- 1 8) $0)) (- (+ (+ (+ (+ $0 (- (* 7 1) (+ 9 5))) 10) (- (+ $0 6) (+ 8 1))) (+ 10 2)) $0)) (+ (+ (+ (* 1 (* $0 5)) (- $0 6)) (* (+ (* (+ (+ (- 3 7) (* $0 (+ $0 (+ $0 7)))) 5) (* $0 7)) 8) (* 6 5))) (+ (+ (+ (- $0 3) 7) (- 7 2)) $0))) (- (+ 1 5) (* 8 1))) $0)))) (+ (- (+ (- $0 6) (* (* (* (+ (* (+ (- (- $0 6) 73) (+ 9 (- $0 1))) (+ (- 2 5) 8)) (- (+ (- $0 (- (+ 9 9) $0)) (- 4 4)) (+ $0 5))) (+ (+ $0 7) (- $0 (- 1 8)))) (* $0 (+ $0 (- (- (+ (- $0 5) 1) $0) 100)))) (- (+ $0 3) $0))) (+ (* (- $0 3) (* $0 (+ (* $0 7) $0))) (* (+ (+ (+ (* (+ (+ $0 9) 7) $0) (- (* (+ $0 6) (- (* 9 2) 45)) (- 6 2))) (* (* (* 10 9) $0) (- 8 10))) (+ (* $0 9) (- (+ 5 7) (* $0 6)))) (+ (- 2 4) (* 4 1))))) (* (+ (* (+ (+ (* $0 7) 1) 10) (+ $0 (- 8 1))) (- (* 3 7) (* (* (+ 3 (- 6 7)) (+ 5 2)) $0))) (+ (- (+ (- $0 5) (+ 5 (+ 8 8))) $0) (* (+ (* $0 9) (+ $0 10)) $0))))) (* (* (- (+ $0 (+ 9 (- $0 (* $0 2)))) (* (* $0 4) 4)) (* 1 8)) (* (- (+ (+ (- (* $0 7) $0) (+ (+ 5 9) $0)) (+ 3 10)) (- (+ (+ $0 10) (+ (- (+ (- (+ (- $0 4) (+ 8 6)) 6) 5) (- (+ 5 (* 6 3)) (* (- $0 7) (* 8 3)))) (+ (- 4 1) (+ (+ (* $0 7) (- (- $0 (+ 6 8)) $0)) (* $0 (* 3 7)))))) (- $0 1))) (* (- $0 3) (* (+ (* (* 9 6) (+ (+ $0 9) (* $0 5))) $0) 4)))))) (* (+ 2 (* (* (- (* (+ (+ $0 (+ (- $0 10) (+ (* $0 4) $0))) (* 4 9)) (* $0 (+ (* (* (+ 9 (+ 3 (* $0 3))) 4) 2) (* (- 5 6) 4)))) (* (+ (- (* (* (- 7 4) (+ (+ (- $0 9) $0) (+ (* (- (+ $0 (- 6 2)) 85) 2) (* (* (* 5 10) (- 12 (+ (- (+ $0 1) 56) 1))) $0)))) (* (- 8 9) (- 7 3))) (* (- 78 (+ (+ $0 3) (* $0 7))) $0)) (+ (+ 2 9) (+ $0 8))) (+ (* 1 1) (* (* 1 2) (+ (+ (* 2 8) 6) (* (- $0 6) (+ 5 1))))))) (+ (+ (* $0 3) (- $0 (+ (- (+ 6 (* 7 1)) $0) (+ (- 4 1) (- $0 9))))) (+ (- (+ 2 5) (+ 5 6)) (* 1 (* 5 10))))) (+ (+ (- (+ (+ (- 5 8) (* (- $0 9) (- 10 10))) (+ (- 10 2) 10)) (* $0 5)) (+ $0 (+ 4 10))) 8))) (* (+ (- $0 (+ 3 7)) (- (+ $0 (* 6 10)) (* (- (- 6 5) $0) (+ $0 7)))) (* (* (* (+ (+ (+ $0 2) 5) (+ (+ (- $0 8) 6) 9)) (* (+ (+ $0 (* $0 1)) 8) (+ (+ (- 7 3) 1) (+ 4 (- $0 7))))) (- (* (+ (* $0 3) (- (- 6 2) (- 10 1))) 1) 11)) (+ (- 1 2) (- (* (+ (- $0 1) (* 10 1)) (+ (- (* 4 (+ 5 4)) (* $0 6)) (- 4 4))) (- (+ (* (- 5 4) 4) (* 9 9)) 97)))))))) (+ (+ (- (+ (* (- (+ (* (+ $0 5) $0) $0) 29) (- 7 4)) (+ (* (+ $0 9) (+ $0 7)) $0)) (+ (+ 10 (+ $0 (* 5 10))) (+ (* $0 8) (+ (- (* (+ (- (- $0 6) $0) 1) $0) (- 3 4)) 10)))) (* (+ $0 (- (* $0 6) (* 5 8))) (- (* (+ (+ (- (+ (* $0 1) (* 1 7)) 72) (+ (- (- 14 (* (+ 8 6) (+ 7 8))) (- $0 2)) 1)) (* (* (+ $0 2) 1) (+ 1 8))) 1) (+ (- 1 4) (+ $0 9))))) (* (+ (+ (+ 6 (+ $0 5)) 7) (+ (+ 5 8) 9)) (- (- 8 3) (- (+ (+ 9 (+ 5 (* $0 9))) (- (* $0 8) (* $0 5))) $0))))))
Parsed in 794.459µs
Generated 28057 bytes of x86-64 binary in 688.538µs
Interpreted: 57.385646451s
Compiled: 238.114079ms
Compiled is 241.00x faster
```

When i still moved stuff from/to stack and from/to registers by hand instead of using my new CodeGen abstraction, codegen/compilation was ~5x faster but i think less than a milisecond for this expression is good enough. Most database queries will be quite a lot less code than this. I think once you have more complex
structures in your code you really want the abstraction though. The code produced should be roughly the same quality as the one with hand written stack movements. Besides the fact that execution time is roughly the same, the code size is also slightly less (22297 bytes vs 22771 in the example above). Also note that once we get into the millisecond range for codegen (gen_expr.py with 1,000+ complexity) we usually spend more than 50% (for 10,000+ complexity more like 80%+) of the actual time of codegen in constant folding. Also parsing almost always takes significantly longer than codegen. Since our LISP style expressions are one of the simplest thigns to parse you can imagine, this difference should only increase WITH more complex languages like SQL. That constant folding takes such a long time for these deep trees should not really be a problem in reality since they usually only nest <10 levels deep anyway.

If you want to test against a hardcoded Rust expression you can just hack that expression into the main.rs file at the line where it says 
```                            
//--- INSERT YOUR HARDCODED EXPRESSION EVALUATION HERE ---
```
and uncomment the commented-out lines around it.


# Long term goals

Maybe once the basic stuff actually works we can think about analyzing the stencil useage and incrementally fusing stencils that often are used next to each other into one stencil in the background. This way we could maybe even eliminate the need to fully compile with LLVM `-O3` alltogether if we get large enough chunks this way. We just might need to recompile with newly generated fused stencils again for long running functions. This would be a bit like the "Incremental Fusion" stuff Benjamin Wagner is doing at the Query Plan level with [InkFuse](https://github.com/wagjamin/inkfuse) just at the codegen level. This way the system would kind of "learn" to pre generate often used table scans/filters/... and utility functions transparently in the background and optimized for the users tables.

One way to make this work could be to keep a weighted sample of a few thousand 2-stencil combinations and then always taking out the ones that appear most often (above a certain threshold) and compiling a new fused stencil for them. This would kind of be JIT compilation for JIT comipilation... JIT^2 compilation? JITception?
