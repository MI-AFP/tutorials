# Performance and Debugging

During this tutorial we will take a look how to improve performance of Haskell program and how to debug it. We will use very simple example everywhere - [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number).

## Measuring time and memory 

When you want to check performance of program and compare two programs or algorithms in terms of time or memory consumption, you need to measure it.

### Basic `time`

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

### Measure allocations with Weigh


## Performance

Now we are able to measure something and compare algorithms, but how to improve the numbers we get if we really need it?

### Basic ideas

When you are not satisfied with the perfomance of your application, then before any sophisticated optimization steps by using strictness, unboxed types, calling FFI, etc., you should consider if you prefer faster application over better readability. Then another important thing to think about is design, if it is not slow by using "naive" algorithm, using inappropriate data structure (List instead of Set or Map), etc.

**Always** rethink your own code before using other optimization techniques!

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

(If you find kinds interesting, try to examine `:kind Maybe` and `:kind Either`.)

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

