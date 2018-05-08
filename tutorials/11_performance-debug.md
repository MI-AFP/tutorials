# Performance and Debugging

During this tutorial we will take a look how to improve performance of Haskell program and how to debug it. We will use very simple example everywhere - [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number).

```haskell
import System.Environment

-- | Naive recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main = do
    args <- getArgs
    show . fibonacci . read . head $ args
```

## Measuring time and memory 

When you want to check performance of program and compare two programs or algorithms in terms of time or memory consumption, you need to measure it.

### Basic `time`

The `time` command is one of the well-known Linux commands for programmers. It can be used to show how long a command takes to run. That makes it Very useful if you are a developer and you want to test the performance of your program or script. Especially to compare time of programs written in other languages "from outside". For basic usage, you will get three numbers:

- `real` = total time is taken to run the command (the same as if you use your normal stopwatch)
- `user` = amount of time that was spent in user mode
- `sys` = amount of time spent in kernel mode

Then `user`+`sys` gives information how much actual CPU time your process used - in total on all cores. This number can be then higher than `real` if your program uses multiple threads.

```
% /usr/bin/time -p runhaskell FibonacciNaive.hs 25
75025
real 0.33
user 0.31
sys 0.01
```

But `time` can do a bit more, you can tell how output should look like with additional "numbers" - number of page faults, average total memory use of the process in kilobytes, number of signals delivered to the process, number of socket messages received/send by the process, exit status of the command, and many others.

```
% /usr/bin/time -f "Elapsed Time: %E\nExit Status: %X\nPage Faults: %F" runhaskell FibonacciNaive.hs 25
75025
Elapsed Time: 0:00.34
Exit Status: 0
Page Faults: 0
```

### Benchmarking with Criterion

If you are interested in such optimizations and improving your application or comparing various algorithms or their implementations, then you might find interesting to use benchmarking library. In Haskell is the most used one called [Criterion](http://www.serpentine.com/criterion/). It provides a powerful but simple way to measure software performance. It provides both a framework for executing and analysing benchmarks and a set of driver functions that makes it easy to build and run benchmarks, and to analyse their results.

For simple usage, you just need to work with the `defaultMain` from [Criterion.Main](https://hackage.haskell.org/package/criterion/docs/Criterion-Main.html) as they show in their example:

```haskell
import Criterion.Main

-- | Naive recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main = defaultMain [
  bgroup "fib" [ bench " 5" $ whnf fibonacci 5
               , bench "10" $ whnf fibonacci 10
               , bench "25" $ whnf fibonacci 25
               ]
  ]
```

It has very nice outputs with form of interactive HTML pages with charts and comparisons and have many options to use.

```
% runhaskell FibonacciNaiveCriterion.hs
benchmarking fib/ 5
time                 7.319 μs   (6.980 μs .. 7.821 μs)
                     0.966 R²   (0.934 R² .. 0.995 R²)
mean                 7.248 μs   (6.966 μs .. 7.847 μs)
std dev              1.321 μs   (805.8 ns .. 2.043 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking fib/10
time                 81.34 μs   (81.15 μs .. 81.54 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 81.58 μs   (81.38 μs .. 81.85 μs)
std dev              811.3 ns   (577.5 ns .. 1.191 μs)

benchmarking fib/25
time                 111.6 ms   (110.5 ms .. 112.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 112.1 ms   (111.7 ms .. 112.9 ms)
std dev              853.6 μs   (534.0 μs .. 1.215 ms)
variance introduced by outliers: 11% (moderately inflated)

runhaskell FibonacciNaiveCriterion.hs  15.98s user 0.04s system 99% cpu 16.055 total
```

### Measure allocations with Weigh

The package [weigh](https://hackage.haskell.org/package/weigh) provides simple interface to mesure the memory usage of a Haskell value or function.

```haskell
import Weigh

-- | Naive recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main = mainWith $ do
        func "fib  5" fibonacci 5
        func "fib 10" fibonacci 10
        func "fib 25" fibonacci 25
```

It provides nice output as plain text table, but it is also possible to change format to markdown.

```
% ./FibonacciNaiveWeigh

Case     Allocated  GCs
fib  5       1,968    0
fib 10      24,304    0
fib 25  33,509,936   63
```

## Performance

Now we are able to measure something and compare algorithms, but how to improve the numbers we get if we really need it?

### Basic ideas

When you are not satisfied with the perfomance of your application, then before any sophisticated optimization steps by using strictness, unboxed types, calling FFI, etc., you should consider if you prefer faster application over better readability. Then another important thing to think about is design, if it is not slow by using "naive" algorithm, using inappropriate data structure (List instead of Set or Map), etc.

**Always** rethink your own code before using other optimization techniques!

```haskell
import System.Environment

-- | Improved recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci n = fib 0 1 n
  where
    fib x _ 0 = x
    fib x y rem = fib y (x+y) (rem-1)    -- just "one-way" recursion!

main = do
    args <- getArgs
    print . fibonacci . read . head $ args
```

Just a very simple re-thinking can have some impact:

```
% /usr/bin/time -p runhaskell FibonacciBetter.hs 25
75025
real 0.24
user 0.22
sys 0.02
```

```
% runhaskell FibonacciBetterCriterion.hs
benchmarking fib/ 5
time                 3.412 μs   (3.235 μs .. 3.591 μs)
                     0.988 R²   (0.983 R² .. 0.998 R²)
mean                 3.191 μs   (3.129 μs .. 3.277 μs)
std dev              253.6 ns   (168.4 ns .. 360.9 ns)
variance introduced by outliers: 82% (severely inflated)

benchmarking fib/10
time                 5.930 μs   (5.871 μs .. 6.013 μs)
                     0.997 R²   (0.994 R² .. 0.998 R²)
mean                 6.209 μs   (6.075 μs .. 6.464 μs)
std dev              625.6 ns   (377.2 ns .. 1.088 μs)
variance introduced by outliers: 87% (severely inflated)

benchmarking fib/25
time                 14.53 μs   (14.31 μs .. 14.90 μs)
                     0.990 R²   (0.972 R² .. 0.999 R²)
mean                 14.78 μs   (14.40 μs .. 15.89 μs)
std dev              1.953 μs   (712.6 ns .. 4.110 μs)
variance introduced by outliers: 91% (severely inflated)

runhaskell FibonacciBetterCriterion.hs  15.90s user 0.07s system 100% cpu 15.954 total
```

```
% ./FibonacciBetterWeigh

Case    Allocated  GCs
fib  5        872    0
fib 10      1,712    0
fib 25     37,000    0
```

### Boxed vs. Unboxed types

Now, we are going to briefly mention is the difference between boxed and unboxed types. Although it is a low-level concern and with regular Haskell programming, you can avoid these terms, it is good to know what is it about when you see it in other's code or in a documentation.

To support laziness, parametric polymorphism, and other properties, by default Haskell data types are represented uniformly as a pointer to a closure on the heap. These are "boxed" values. An unboxed is represented directly by raw value (i.e., without any indirection). Using unboxed types can lead to time/space optimizations. Having always pointers to a heap-allocated object is fairly slow, so compilers attempt to replace these boxed values with unboxed raw values when possible. Unboxed values are a feature of some compilers that allow directly manipulating these low-level values. Since they behave differently than normal Haskell types, generally the type system is extended to type these unboxed values.

In [GHC], unboxed values have a hash mark as a suffix to their name. For instance, the unboxed representation of 42 is 42#. However, you can't pass them to polymorphic functions (like `show` for instance). To allow that, you need to use constructor `I#` that takes an unboxed integer and returns the `Int` (wraps). You can observe [kind](https://wiki.haskell.org/Kind) (*kind of type*, we will look again at kinds with typeclasses) of boxed and unboxed types:

* By default, kind of type is `*` (try in GHCi: `:kind Int`)
* Kind of unboxed type is `#` (try in GHCi: `:kind Int#`, first do `:set -fglasgow-exts`)

```haskell
import GHC.Exts

showUnboxedInt   :: Int# -> String
showUnboxedInt n = "Unboxed: " ++ (show $ I# n) ++ "#"
```

### Strictness with types

In the previous lessons, we touched the topic of enforcing strictness with `!` in patterns ([bang patterns](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html)) and in function application with `$!` operator. Similarly, we can use `!` with type fields like this:

```haskell
data MyType = MyConstr Int !Int

data MyRec = MyRecConstr { xA ::  Int
                         , xB :: !Int
                         }
```

For both cases it means that when data constructor is evaluated, it must fully evaluate ([weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)) the second parameter, but the first one will stay unevaluated in a lazy way. All depends on language implementation in the used compiler.

#### Unpacking strict fields

One of the most used optimization techniques when talking about unboxed types and strictness with [GHC] is [unpacking strict fields](https://wiki.haskell.org/Performance/Data_types#Unpacking_strict_fields). When a constructor field is marked strict, and it is a single-constructor type, then it is possible to ask GHC to unpack the contents of the field directly in its parent with `{-# UNPACK #-}` pragma:

```haskell
data T1 = T1 {-# UNPACK #-} !(Int, Float)  -- => T1 Int Float
data T2 = T2 Double {-# UNPACK #-} !Int    -- => T2 Double Int#
```

We mention this just because of differences in performance of types we are going to describe now. You don't need to use strict or unboxed types within your work if you don't need to have time/space optimizations...

### GHC optimization flags

If you know optimization with GCC, then you won't be surprised how it works with GHC:

* `-O0` = turn off all optimisation
* `-O` or `-O1` = generate good-quality code without taking too long about it
* `-O2` = apply every non-dangerous optimisation, even if it means significantly longer compile times (in most cases, there is no significant difference between `-O1` and `-O2`)

Then there are also `-f*`  platform-independent flags, that allows you turn on and off individual optimizations. For more information, please visit [GHC documentation](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-optimisation.html). 


### Concurrency

Just like with GCC, you can use optimization flags with GHC. You can also drill deeper in your source code and optimize it by hand, use FFI, parallelism or concurrency, and so on in order to achieve faster computation. Good resource for that is [wiki.haskell.org/Performance](https://wiki.haskell.org/Performance) where you can look up hints for specific parts of you app and/or your compiler.

For parallelism and concurrency visit [wiki.haskell.org/Parallel](https://wiki.haskell.org/Parallel). You can both:

* run parallel threads with Control.Parallel,
* run simultaneous IO actions with forks.

It is also possible to do distributed computations on clusters but it is far beyond the scope of this course.

```haskell
--TODO: Control.Parallel simple example
```

## Debugging

Even if you are a good Haskell programmer, things can go wrong and especially in big projects it is a nontrivial challenge to find out where you did some mistake. Going thru the code in multiple functions, inner functions, various modules, etc. can be painful. Luckilly there are some ways how to debug Haskell program and some are pretty easy and similar to well-known.

### Tracing with `Debug.Trace`

You should already know how to use GHC and GHCi to compile, link and examine Haskell programs. The simplest tool to use for debugging is the `trace` from [Debug.Trace](https://hackage.haskell.org/package/base.0/docs/Debug-Trace.html) which outputs the trace message given as its first argument, before returning the second argument as its result.

For example:

```haskell
func a b = trace ("func " ++ show a ++ " " ++ show b) undefined
```

If you need more than just that, you can use [GHCi debugger](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/ghci-debugger.html) (other compilers, such as Hugs, have some different), which allows:

* setting breakpoints and stepping,
* inspecting variables,
* tracing,
* working with exceptions,
* and so on.

### `debug` package

### GHCi debugger

## Further reading

* [Haskell - Debugging](https://wiki.haskell.org/Debugging)
* [Haskell - Performance](https://wiki.haskell.org/Performance)
* [Haskell - Concurrency](https://wiki.haskell.org/Concurrency)
* [GHC - Concurrent and Parallel Haskell](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/lang-parallel.html)

