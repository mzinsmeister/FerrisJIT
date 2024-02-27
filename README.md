# Beginning of an experimental copy-and-patch like compiler backend written in Rust

This is supposed to become an experimental compiler backend based on [Copy and Patch](https://fredrikbk.com/publications/copy-and-patch.pdf) to be (maybe) later used in my OxidSQL Database for a compiling query engine. It might be the first copy and patch experiments (at least in a public repo) in Rust. I will try to use LLVM directly for generating the stencils instead of going through the kind of ugly C++ templates described in the original paper. The idea would be to use the existing codegen primitives in your database system that can already generate LLVM IR and use those to generate stencils. One could then also copy and patch these stencils together using those same primitives.

As a first step i will try to generate a few simple addition stencils and patch those together. As soon as that works, the hardest part should be done and it should just be a matter of adding more stencils and patching them together correctly (famous last words maybe).

## Current state

Automatic stencil generation for integer-types and integer-operations should be working. Expression evaluation in basic cases works but there might still be bugs. Register utilization/caching could definitely still be improved but it works for now.

## Next steps

Next steps would be control flow constructs and beeing able to generate llvm ir for the entire expressions using the same code that generates the stencils.

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

If no file is given, the example will be run on a hardcoded sequential element array. Then you can enter expressions in lisp-like syntax. By default you will be in interactive mode and the result per input-line will just be printed. You can also pass `-b` as a flag to be in benchmark mode. Make sure to run with `--release` in this case. This will then print out the timings for compiled vs interpreted (when using generated input the input size will be 1,000,000 in this case, otherwise 10). 

#### Currently Supported Expressions

All constants and variables must be 64 bit signed integers.

Arithmetic Operations:
* `+` Addition
* `-` Subtraction
* `*` Multiplication
* `/` Division
* `=` Equality (produces a boolean result)

Boolean Operations:
* `=` Equality

#### Examples

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

For now we can just use very large expressions to get a better idea of the relative performance. Large expressions can be generated with the gen_expr.py (enter a number for complexity as first argument). 

For example (complexity 1000; this will overflow and therefore only work on `--release` mode):

```lisp
>> (+ (+ (+ (- (* (- (+ (* 3 (* 4 1)) $0) (* (- (* (- $0 1) $0) 83) (* (* 2 1) 4))) (- (* (+ (+ (+ 2 4) (+ (- $0 5) (* $0 3))) (+ (* (* $0 5) (- (+ $0 7) (+ 7 3))) (* (+ (* $0 9) (+ (* 5 1) $0)) 1))) (* 1 9)) (- (+ (+ (+ (+ 1 9) $0) (+ (+ (+ 5 (- 8 8)) (* $0 10)) (* (* 4 8) (* $0 10)))) 7) (* (* (* (* (+ (- 10 7) $0) 3) $0) $0) 4)))) (+ (+ (- (- $0 7) (- 1 8)) (- (+ (+ (+ 1 4) (* (- $0 7) (* $0 1))) $0) (+ (- 5 1) (+ (+ $0 (- $0 (* 10 4))) (* 9 1))))) (* (+ $0 8) (+ (- (- $0 3) $0) (* (+ 1 (- (+ $0 4) 58)) (* (+ $0 10) $0)))))) (- (+ (+ (+ (+ (- (* $0 8) 18) $0) (+ (+ (+ (+ (- (* $0 8) $0) $0) $0) (- 3 4)) (- (+ (* (+ (- 9 9) (* 7 2)) (* $0 6)) 8) (+ (* 1 (+ (* $0 6) (+ 10 (- 1 2)))) (* (- $0 5) 2))))) (* (- (+ (+ (- $0 6) (+ (* 2 4) (+ (+ $0 (- $0 2)) 8))) (+ (* 7 7) 3)) $0) (* (* (- 25 (- 1 2)) $0) 3))) (+ (- (- (* (+ 6 10) (* (+ (- $0 8) (- $0 6)) 2)) (+ (- $0 8) $0)) (+ (- (+ (+ (- (+ (* 8 4) (- 10 10)) $0) $0) $0) 60) 8)) (- (+ (- 6 8) (- $0 2)) (* (* 3 1) (- 1 10))))) (+ (+ (+ $0 (+ (+ (+ (- (- (* (* 6 7) 4) (+ (- $0 (- $0 10)) $0)) (- (+ 4 2) (* 9 4))) 6) 10) (- (* (* (* $0 4) 3) (* $0 3)) 10))) (- (* (- 1 6) (- (+ (+ $0 3) (+ (+ $0 10) (+ (+ (+ (+ 10 1) (- 4 6)) (+ (* 8 7) (- $0 4))) (+ (* 8 1) (+ $0 9))))) (+ (+ $0 (+ (+ 2 5) $0)) (+ (+ 8 8) (* 10 3))))) (* (* (+ 6 3) 2) (- $0 10)))) (* (+ (- (+ (+ 10 3) (* $0 1)) (+ $0 5)) (+ (+ (+ (* (* $0 8) (- 2 3)) (* $0 2)) (- (- (- 5 2) (* 3 4)) $0)) (+ (* (- (- 4 9) (- 4 7)) 3) (+ $0 7)))) (* (* (- $0 (- $0 8)) (* $0 7)) 3))))) (- (- (- (+ 4 1) 7) (+ (* (+ (- (+ (* 7 8) 4) 9) (* (- (+ (- 3 7) (* $0 9)) $0) (- $0 (- 2 7)))) $0) 7)) (- (+ (* (+ (- (- (+ (* 4 (+ 5 10)) (+ (+ $0 10) 5)) (* 6 10)) (+ $0 9)) (+ (* (* $0 5) (+ (- $0 10) (+ (* $0 7) (+ (+ (* 9 8) 6) (+ 4 3))))) $0)) (+ (+ (- (* (* 2 (+ (* 9 5) $0)) (+ (* (* 3 3) $0) (* (* $0 2) (+ 8 4)))) (- (+ (- (- 10 10) (* (* $0 2) (+ 5 10))) (+ (+ 4 5) (* $0 (- $0 6)))) (* (+ $0 5) 4))) (* (* (+ 10 (+ 3 5)) (+ (+ (* (* 1 7) $0) $0) $0)) (+ (- (* (+ 3 5) (+ $0 7)) (+ (+ (* $0 9) (* 8 8)) (+ (* $0 4) (+ 6 7)))) (+ $0 7)))) $0)) (+ (* (- (+ (- (- 6 9) (* 5 4)) (* (+ (+ 9 (* 1 5)) 3) (* 1 (* (+ (- $0 7) 9) $0)))) (+ (+ (* (+ $0 3) (* (* (- $0 6) (* 9 3)) $0)) (* (+ 6 (+ (- 22 (+ 10 5)) (* (* 6 10) (* (* $0 3) 1)))) (+ (- $0 10) 8))) (+ 2 2))) (- (+ (+ (+ (- $0 3) (- (+ $0 3) 41)) (- 4 6)) (+ 1 (+ (- (+ $0 (+ $0 10)) 2) (+ $0 (+ $0 9))))) (- 8 1))) (* (+ (+ (+ (* 4 9) 9) (+ (+ 10 7) (- (- 5 6) $0))) (+ (+ (* (+ (- $0 1) $0) (+ (+ (* $0 9) (* 5 2)) (+ (- $0 10) $0))) (- (+ $0 8) (* (* 1 2) (+ 3 6)))) (- (* (+ (- (* (* $0 5) (- (- 8 5) 18)) 63) (- (- $0 6) (- (+ $0 8) 1))) (+ (+ (* $0 (+ 2 4)) (- 23 (* (* (- 22 (+ (- $0 4) $0)) $0) (+ $0 1)))) 4)) (- 44 (+ (+ (* 9 7) (* (+ 8 (- 26 (* $0 10))) $0)) (+ 2 (- (+ $0 3) $0))))))) (+ (- (+ (- $0 6) (* $0 4)) 72) (+ (* (+ 10 6) (- 10 9)) (- (+ 10 2) (* $0 8))))))) (* (- (- (- $0 (* (+ (* (+ 3 8) (- $0 9)) (* $0 2)) (* (+ $0 2) $0))) (- (- $0 9) 88)) 76) (+ (* (* $0 6) (+ (+ (- (* $0 7) (- $0 2)) 6) (+ $0 2))) (- (- 10 1) (* (+ 7 (- 7 10)) (+ (- (- 4 10) 82) $0)))))))) (- (+ (* (- (+ (- 29 (+ $0 1)) (+ (+ (- (* $0 8) (+ 8 7)) $0) (- (* (- $0 9) (+ (- 10 9) (+ 5 3))) (+ (* (+ $0 2) $0) 9)))) (* (+ (- (+ (+ (+ (+ 1 9) (* $0 3)) (* 5 1)) (- (+ (- 3 9) $0) $0)) $0) (* (+ (* (- (- (* 2 2) 39) (* $0 5)) (* $0 9)) (* (* $0 9) (* (+ 6 8) (- (+ (+ (* 7 6) $0) (+ (* (+ (* $0 8) 3) $0) $0)) $0)))) (- (- (* (- $0 2) 3) (- 4 2)) (+ 6 (- (* $0 3) 68))))) (+ (- (- $0 7) (- (+ 2 (- 5 10)) $0)) (+ $0 (+ (- $0 5) (+ 2 (* 4 5))))))) (* (+ $0 (+ (+ (+ (* 7 10) 7) (* (+ $0 10) (* (- (- 7 4) $0) (+ (* $0 2) $0)))) (+ (- (- (* 2 5) $0) 31) 7))) (* (* (- (- (+ $0 5) (- 9 5)) (- (* (* (+ (- $0 9) $0) (- $0 4)) (* (+ (+ (* 2 (+ (+ $0 8) (- (+ 9 2) (- 1 8)))) (* (+ $0 (* 1 8)) 2)) $0) (+ 1 (* 3 7)))) (- (+ (- 20 (* 10 9)) $0) (- $0 3)))) (* 1 (- (* 7 10) (+ 1 (+ $0 2))))) 3))) (- (- (+ (+ (- 65 (- (* 10 9) (- $0 1))) (- $0 (- (- (- 5 4) (+ (* $0 6) (+ $0 10))) (* (+ (* 2 (+ $0 5)) (* 4 8)) (- 14 (* $0 9)))))) (* (+ (- (+ (* (- 3 6) (- $0 (* (+ (- 4 10) 8) (+ $0 4)))) $0) (* 5 3)) (- (- (* (- 1 5) (- 5 8)) $0) (+ (* (+ (- 4 1) (+ 2 7)) (- (+ $0 (- (- 6 1) $0)) (+ $0 5))) (+ $0 7)))) (+ (+ (- 5 9) 2) (+ (+ $0 (+ $0 4)) 7)))) (+ (+ (+ (+ (+ (- (+ (+ (+ $0 7) 2) (- (+ (* (- 6 3) $0) (* (+ 6 8) $0)) $0)) (+ (+ $0 (+ 8 6)) (+ 5 (+ 5 1)))) (- (+ 1 1) $0)) (+ (+ (+ 4 5) (+ $0 8)) (+ (- (+ (* 1 (- (+ 5 6) (+ (- $0 8) $0))) (+ 3 (* 8 4))) (* (- $0 6) (+ (* 5 4) 7))) (+ (+ (+ $0 (- (- 8 3) (* 7 8))) (+ (- (+ (* 9 10) 6) (- (- 2 10) (- (+ (+ $0 4) $0) (+ (* 9 7) 2)))) (+ $0 (* 4 6)))) 3)))) (* (+ (+ (* (* (+ (- $0 5) (* $0 4)) (+ (* $0 6) 5)) (+ (+ (+ 10 8) (* $0 4)) $0)) (+ (- (- $0 8) 87) (* 7 6))) 2) (+ (- (+ (- $0 (* 4 9)) (- $0 (* $0 4))) (+ (+ (+ (* 4 5) (* $0 7)) (- (+ (+ $0 1) 8) $0)) (- (+ (+ (- (* $0 10) $0) 8) (+ (* $0 2) (- 1 8))) 91))) (- (+ (- (- (* (+ (+ $0 (* (+ (* 2 4) (* 2 3)) 2)) (+ 8 (- 5 5))) (- (- 1 8) $0)) (- (+ (+ (+ (+ $0 (- (* 7 1) (+ 9 5))) 10) (- (+ $0 6) (+ 8 1))) (+ 10 2)) $0)) (+ (+ (+ (* 1 (* $0 5)) (- $0 6)) (* (+ (* (+ (+ (- 3 7) (* $0 (+ $0 (+ $0 7)))) 5) (* $0 7)) 8) (* 6 5))) (+ (+ (+ (- $0 3) 7) (- 7 2)) $0))) (- (+ 1 5) (* 8 1))) $0)))) (+ (- (+ (- $0 6) (* (* (* (+ (* (+ (- (- $0 6) 73) (+ 9 (- $0 1))) (+ (- 2 5) 8)) (- (+ (- $0 (- (+ 9 9) $0)) (- 4 4)) (+ $0 5))) (+ (+ $0 7) (- $0 (- 1 8)))) (* $0 (+ $0 (- (- (+ (- $0 5) 1) $0) 100)))) (- (+ $0 3) $0))) (+ (* (- $0 3) (* $0 (+ (* $0 7) $0))) (* (+ (+ (+ (* (+ (+ $0 9) 7) $0) (- (* (+ $0 6) (- (* 9 2) 45)) (- 6 2))) (* (* (* 10 9) $0) (- 8 10))) (+ (* $0 9) (- (+ 5 7) (* $0 6)))) (+ (- 2 4) (* 4 1))))) (* (+ (* (+ (+ (* $0 7) 1) 10) (+ $0 (- 8 1))) (- (* 3 7) (* (* (+ 3 (- 6 7)) (+ 5 2)) $0))) (+ (- (+ (- $0 5) (+ 5 (+ 8 8))) $0) (* (+ (* $0 9) (+ $0 10)) $0))))) (* (* (- (+ $0 (+ 9 (- $0 (* $0 2)))) (* (* $0 4) 4)) (* 1 8)) (* (- (+ (+ (- (* $0 7) $0) (+ (+ 5 9) $0)) (+ 3 10)) (- (+ (+ $0 10) (+ (- (+ (- (+ (- $0 4) (+ 8 6)) 6) 5) (- (+ 5 (* 6 3)) (* (- $0 7) (* 8 3)))) (+ (- 4 1) (+ (+ (* $0 7) (- (- $0 (+ 6 8)) $0)) (* $0 (* 3 7)))))) (- $0 1))) (* (- $0 3) (* (+ (* (* 9 6) (+ (+ $0 9) (* $0 5))) $0) 4)))))) (* (+ 2 (* (* (- (* (+ (+ $0 (+ (- $0 10) (+ (* $0 4) $0))) (* 4 9)) (* $0 (+ (* (* (+ 9 (+ 3 (* $0 3))) 4) 2) (* (- 5 6) 4)))) (* (+ (- (* (* (- 7 4) (+ (+ (- $0 9) $0) (+ (* (- (+ $0 (- 6 2)) 85) 2) (* (* (* 5 10) (- 12 (+ (- (+ $0 1) 56) 1))) $0)))) (* (- 8 9) (- 7 3))) (* (- 78 (+ (+ $0 3) (* $0 7))) $0)) (+ (+ 2 9) (+ $0 8))) (+ (* 1 1) (* (* 1 2) (+ (+ (* 2 8) 6) (* (- $0 6) (+ 5 1))))))) (+ (+ (* $0 3) (- $0 (+ (- (+ 6 (* 7 1)) $0) (+ (- 4 1) (- $0 9))))) (+ (- (+ 2 5) (+ 5 6)) (* 1 (* 5 10))))) (+ (+ (- (+ (+ (- 5 8) (* (- $0 9) (- 10 10))) (+ (- 10 2) 10)) (* $0 5)) (+ $0 (+ 4 10))) 8))) (* (+ (- $0 (+ 3 7)) (- (+ $0 (* 6 10)) (* (- (- 6 5) $0) (+ $0 7)))) (* (* (* (+ (+ (+ $0 2) 5) (+ (+ (- $0 8) 6) 9)) (* (+ (+ $0 (* $0 1)) 8) (+ (+ (- 7 3) 1) (+ 4 (- $0 7))))) (- (* (+ (* $0 3) (- (- 6 2) (- 10 1))) 1) 11)) (+ (- 1 2) (- (* (+ (- $0 1) (* 10 1)) (+ (- (* 4 (+ 5 4)) (* $0 6)) (- 4 4))) (- (+ (* (- 5 4) 4) (* 9 9)) 97)))))))) (+ (+ (- (+ (* (- (+ (* (+ $0 5) $0) $0) 29) (- 7 4)) (+ (* (+ $0 9) (+ $0 7)) $0)) (+ (+ 10 (+ $0 (* 5 10))) (+ (* $0 8) (+ (- (* (+ (- (- $0 6) $0) 1) $0) (- 3 4)) 10)))) (* (+ $0 (- (* $0 6) (* 5 8))) (- (* (+ (+ (- (+ (* $0 1) (* 1 7)) 72) (+ (- (- 14 (* (+ 8 6) (+ 7 8))) (- $0 2)) 1)) (* (* (+ $0 2) 1) (+ 1 8))) 1) (+ (- 1 4) (+ $0 9))))) (* (+ (+ (+ 6 (+ $0 5)) 7) (+ (+ 5 8) 9)) (- (- 8 3) (- (+ (+ 9 (+ 5 (* $0 9))) (- (* $0 8) (* $0 5))) $0))))))
Generated 29305 bytes of x86-64 binary in 725.881µs
Interpreted: 63.780450476s
Compiled: 302.423033ms
Compiled is 210.90x faster
```

When i still moved stuff from/to stack and from/to registers by hand instead of using my new CodeGen abstraction, codegen/compilation was ~5x faster but i think less than a milisecond for this expression is good enough. Most database queries will be quite a lot less code than this. I think once you have more complex
structures in your code you really want the abstraction though. Also it seems like our register allocation or stack movement generation is not ideal yet. The old variant produced significantly less code (about 1/4 less) and ran about 40% faster. I don't think this is fundamentally because of the new abstraction but more because the way we decide when to move stuff to and from registers is not ideal yet.

If you want to test against a hardcoded Rust expression you can just hack that expression into the main.rs file at the line where it says 
```                            
//--- INSERT YOUR HARDCODED EXPRESSION EVALUATION HERE ---
```
and uncomment the commented-out lines around it.


# Long term goals

Maybe once the basic stuff actually works we can think about analyzing the stencil useage and incrementally fusing stencils that often are used next to each other into one stencil in the background. This way we could maybe even eliminate the need to fully compile with LLVM `-O3` alltogether if we get large enough chunks this way. We just might need to recompile with newly generated fused stencils again for long running functions. This would be a bit like the "Incremental Fusion" stuff Benjamin Wagner is doing at the Query Plan level with [InkFuse](https://github.com/wagjamin/inkfuse) just at the codegen level. This way the system would kind of "learn" to pre generate often used table scans/filters/... and utility functions transparently in the background and optimized for the users tables.

One way to make this work could be to keep a weighted sample of a few thousand 2-stencil combinations and then always taking out the ones that appear most often (above a certain threshold) and compiling a new fused stencil for them. This would kind of be JIT compilation for JIT comipilation... JIT^2 compilation? JITception?