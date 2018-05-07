# Performance and Debugging

## Measuring time and memory 

### Benchmarking with Criterion

If you are interested in such optimizations and improving your application or comparing various algorithms or their implementations, then you might find interesting to use benchmarking library. In Haskell is the most used one called [Criterion](http://www.serpentine.com/criterion/). It provides a powerful but simple way to measure software performance. It provides both a framework for executing and analysing benchmarks and a set of driver functions that makes it easy to build and run benchmarks, and to analyse their results.

For simple usage, you just need to work with the `defaultMain` from [Criterion.Main](https://hackage.haskell.org/package/criterion/docs/Criterion-Main.html) as they show in their example:

```haskell
import Criterion.Main

fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "11" $ whnf fib 11
               ]
  ]
```
It has very nice outputs with form of interactive HTML pages with charts and comparisons and have many options to use.

## Performance

### Basic ideas

### Boxed and unboxed types

### Strictness

### GHC optimization flags

### Other optimization techniques

### Concurrency and optimizations

Just like with GCC, you can use optimization flags with GHC. You can also drill deeper in your source code and optimize it by hand, use FFI, parallelism or concurrency, and so on in order to achieve faster computation. Good resource for that is [wiki.haskell.org/Performance](https://wiki.haskell.org/Performance) where you can look up hints for specific parts of you app and/or your compiler.

For parallelism and concurrency visit [wiki.haskell.org/Parallel](https://wiki.haskell.org/Parallel). You can both:

* run parallel threads with Control.Parallel,
* run simultaneous IO actions with forks.

It is also possible to do distributed computations on clusters but it is far beyond the scope of this course.

```haskell
--TODO: Control.Parallel simple example
```

## Debugging

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

## Further reading

* [Haskell - Debugging](https://wiki.haskell.org/Debugging)
* [Haskell - Performance](https://wiki.haskell.org/Performance)
* [Haskell - Concurrency](https://wiki.haskell.org/Concurrency)
* [GHC - Concurrent and Parallel Haskell](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/lang-parallel.html)

