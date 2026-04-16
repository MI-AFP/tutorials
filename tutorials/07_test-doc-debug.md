# Tests, Documentation, Debugging and Performance

In this session, we will take a look at testing, documentation, debugging and performance of Haskell programs. We will cover the most popular testing frameworks and libraries for Haskell, we will see how to write documentation with Haddock, and we will also briefly mention some techniques for improving the performance of Haskell programs. This is a very important topic for every developer, so we will try to cover it in a comprehensive way. As always, there are many other tools and techniques that we will not cover, but we will try to give you a good overview of the most important ones.

## Testing

Haskell's type system catches a lot of mistakes early, but it can't guarantee your program matches the intended behaviour in all cases. Tests are still valuable for:

* specifying behaviour (especially edge cases),
* protecting refactors,
* validating assumptions that are not in types (IO behaviour, partial functions, invariants),
* giving fast feedback while learning or exploring. 


This chapter introduces three common testing approaches in Haskell, starting from *small and direct* and moving toward *higher-level and expressive*:

* **HUnit** for straightforward unit tests,
* **QuickCheck** for property-based testing,
* **Hspec** as a readable test framework that can integrate both.

### HUnit

[HUnit](https://hackage.haskell.org/package/HUnit) is the Haskell version of the xUnit family of test frameworks. It's small, direct, and good for *given input X, expect output Y* style tests.

```haskell
import Test.HUnit

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)

tests = TestList [ TestLabel "test1" test1
                 , TestLabel "test2" test2
                 ]
```

It is very simple, there are set of assertions (with many operator aliases) and three variants of tests that can be composed: `TestCase`, `TestLabel`, and `TestList`.  It is sufficient for a simple unit testing, but tests are not that nicely readable as with `hspec` (see below).

```
GHCi> runTestTT tests
Cases: 2  Tried: 2  Errors: 0  Failures: 0
```

HUnit gives you:

* `assertEqual` for equality checks,
* `assertBool` for predicate checks,
* `TestCase` for defining a single test case,
* `TestList` / `TestLabel` for structuring suites. 

When to use HUnit:

* you want simple, explicit unit tests,
* you don't need fancy reporting or BDD-style structure,
* you're testing concrete scenarios (parsers, pure functions, small modules).

### QuickCheck

[QuickCheck](https://hackage.haskell.org/package/QuickCheck) is built around the idea that instead of writing many example tests, you write properties your code should satisfy — and QuickCheck tries to falsify them using randomly generated inputs. All from a simple example to complex tutorials of such definitions are explained in the [manual](https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).

When to use QuickCheck:

* you can express correctness as laws (associativity, identity, round-trip encode/decode),
* you want broad coverage across many inputs,
* you want counterexamples quickly (it prints failing cases).

#### Basic properties

With QuickCheck you can, for example, check if some function is associative or commutative:

```haskell
import Test.QuickCheck

prop_ReverseReverse :: String -> Bool
prop_ReverseReverse xs = reverse (reverse xs) == xs

prop_AddAssociative :: Int -> Int -> Int -> Bool
prop_AddAssociative x y z = (x + y) + z == x + (y + z)

prop_AddCommutative :: Int -> Int -> Bool
prop_AddCommutative x y = x + y == y + x

main = do
         quickCheck prop_ReverseReverse
         quickCheck prop_AddAssociative
         quickCheck prop_AddCommutative
         quickCheck (withMaxSuccess 100000 prop_AddCommutative)
```

QuickCheck generates automatically randomized values (it tries to start with corner cases) and it tries to find a counterexample. There is some default behaviour that you can override, for example, to request more random values.

```
% runhaskell qctests.hs
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100000 tests.
```

#### Own datatypes and `Arbitrary`

If you have your own types, you need to make them an instance of the typeclass `Arbitrary`. Then QuickCheck can generate examples for them, too:

```haskell
import Test.QuickCheck
import Control.Monad

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)

mature   :: Person -> Person
mature p = p { age = 18 }

instance Arbitrary Person where
   arbitrary = do
     nameLength <- choose (1, 30)
     randomName <- replicateM nameLength (elements ['a'..'z'])
     randomAge  <- choose (0, 100)
     return Person { name = randomName
                   , age  = randomAge
                   }

prop_Mature       :: Person -> Bool
prop_Mature p
    | age p < 18  = mature p == p { age = 18 }
    | otherwise   = mature p == p

main :: IO ()
main = quickCheck prop_Mature
```

### Hspec

[Hspec](https://hackage.haskell.org/package/hspec) is a popular testing framework inspired by RSpec. It gives you a clean structure (describe, it), human-friendly output, and it integrates with HUnit and QuickCheck so you can mix unit tests and properties in one place. Some of Hspec's distinctive features are:

* a friendly DSL for defining tests,
* integration with QuickCheck, SmallCheck, and HUnit,
* parallel test execution,
* automatic discovery of test files.

Tests written in Hspec are very readable, intuitive and powerful. It allows integration with HUnit as well as with QuickCheck so it is sort of an "ultimate testing framework for Haskell".

```haskell
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
```

#### Expectations

There are many predefined expectation functions that are typically written in infix notation to improve readability of specs. They are in separated packages and projects: https://github.com/hspec/hspec-expectations#readme

* `shouldBe` = equality test
* `shouldNotBe` = inequality test
* `shouldSatisfy` = test if result satisfies given property as function of type `a -> Bool`
* `shouldStartWith` = test if list has a given prefix
* `shouldEndWith` = test if list has a given suffix
* `shouldContain` = test if list contains sublist
* `shouldMatchList` = test if lists have same elements (can be different order)
* `shouldReturn` = test for actions (where is `return` used)
* `shouldThrow` = test if throwing some exception or error (`evaluate` from `Control.Exception` must be used, then there are predefined expectations: `anyException`, `anyErrorCall`, `errorCall "message"`, `anyIOException`, and `anyArithException`.

#### Property check

The integration of hspec with QuickCheck is really great, all you need to do is to write a `property`.

```haskell
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

prop_ReverseReverse :: String -> Bool
prop_ReverseReverse xs = reverse (reverse xs) == xs

prop_AddAssociative :: Int -> Int -> Int -> Bool
prop_AddAssociative x y z = (x + y) + z == x + (y + z)

prop_AddCommutative :: Int -> Int -> Bool
prop_AddCommutative x y = x + y == y + x

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    context "when used with ints" $ do
      modifyMaxSuccess (const 20000) $ do
        it "is associative" $
          property prop_AddAssociative
        it "is commutative" $
          property prop_AddCommutative
  describe "reverse" $ do
    context "when used with string" $ do
      it "negates when used twice" $
        property prop_ReverseReverse
```

```
% runhaskell Spec.hs

addition
  when used with ints
    is associative
    is commutative
reverse
  when used with string
    negates when used twice

Finished in 0.2299 seconds
3 examples, 0 failures
```

#### Complex test suites

It is a good practice to separate specs according to your modules including the directories.

```haskell
import Test.Hspec

import qualified FooSpec
import qualified Foo.BarSpec
import qualified BazSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Foo"     FooSpec.spec
  describe "Foo.Bar" Foo.BarSpec.spec
  describe "Baz"     BazSpec.spec
```

This may become quite elaborate for big projects, so [hspec-discover](https://hackage.haskell.org/package/hspec-discover) provides an automatic spec discovery. All you need to do in the main spec file is this:

```haskell
-- file test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Another useful thing for bigger projects is [Test.Hspec.Formatters](https://hackage.haskell.org/package/hspec/docs/Test-Hspec-Formatters.html) module. In the following example, `main` is in a different module than `spec` created by automatic discovery:

```haskell
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

prop_ReverseReverse :: String -> Bool
prop_ReverseReverse xs = reverse (reverse xs) == xs

prop_AddAssociative :: Int -> Int -> Int -> Bool
prop_AddAssociative x y z = (x + y) + z == x + (y + z)

prop_AddCommutative :: Int -> Int -> Bool
prop_AddCommutative x y = x + y == y + x

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} $ do
  describe "addition" $ do
    context "when used with ints" $ do
      modifyMaxSuccess (const 20000) $ do
        it "is associative" $
          property prop_AddAssociative
        it "is commutative" $
          property prop_AddCommutative
  describe "reverse" $ do
    context "when used with string" $ do
      it "negates when used twice" $
        property prop_ReverseReverse
```

```
% runhaskell playground.hs
...
Finished in 0.2056 seconds
3 examples, 0 failures
```

## Haddock (documentation)

Good documentation in Haskell is not about repeating types — the type signature already says a lot. Instead, documentation should explain:

* what the function means, not what it does mechanically,
* important invariants or assumptions,
* edge cases and failure modes,
* performance characteristics when relevant,
* how pieces fit together at the module level.

Haskell uses [Haddock](https://www.haskell.org/haddock/), a documentation system that extracts specially formatted comments and generates HTML documentation.

### Documenting modules

Module documentation appears at the top of a file, before the module declaration. It should give an overview of the module's purpose, key types and functions, and any important design decisions or usage notes.

```haskell
{-|
Module      : Data.Queue
Description : A simple FIFO queue implementation.
Copyright   : (c) 2026 Your Name
License     : MIT
Maintainer  : you@example.com
Stability   : experimental
Portability : portable

This module provides a purely functional FIFO queue.

The implementation is based on two lists and guarantees:

* Amortized O(1) enqueue
* Amortized O(1) dequeue

Example:

>>> let q = enqueue 1 empty
>>> dequeue q
Just (1, empty)
-}
module Data.Queue
  ( Queue
  , empty
  , enqueue
  , dequeue
  ) where
-- ...
```

A good module doc should cover:

* A short one-line summary.
* A longer description (what problem it solves).
* Performance guarantees (if relevant).
* Small usage example.
* Any important invariants.

You can leverage the documentation also to explain design decisions, trade-offs, and how the module fits into the larger system. It can help you actually structure your code better and make it more maintainable.

### Documenting functions

Haddock comments use `-- |` for single-line documentation and `{-| ... -}` for multi-line blocks. For functions, you should document:

```haskell
-- | Computes the arithmetic mean of a non-empty list.
--
-- Throws an error if the list is empty.
--
-- >>> mean [1,2,3,4]
-- 2.5
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)
```

Focus on:

* Meaning – what concept does this represent?
* Preconditions – does it assume non-empty list?
* Failure behavior – does it throw? return `Nothing`?
* Complexity – if not obvious.
* Examples – short `>>>` examples are excellent for understanding.

You should always document the *behaviour* and not *mechanics* (*implementation details*) of the function. The type signature already says a lot about mechanics, so documentation should focus on meaning, assumptions, edge cases, and examples.

```haskell
-- | Adds 1 to the input.
increment x = x + 1

-- versus:

-- | Successor function for integers.
--
-- Equivalent to @(+1)@.
increment :: Int -> Int
increment x = x + 1
```

### Documenting types

You can also document data types and their fields. This is especially important for complex types where the meaning of fields may not be obvious, or where there are important invariants to maintain.

```haskell
-- | Result of parsing an expression.
data ParseResult
  = ParseSuccess Expr
    -- ^ Successfully parsed expression.
  | ParseError String
    -- ^ Human-readable error message.

-- | User account information.
data User = User
  { userId   :: UserId     -- ^ Unique identifier.
  , userName :: String     -- ^ Display name.
  , userAge  :: Int        -- ^ Age in years (must be >= 0).
  }
```

### Documenting typeclasses and instances

Type classes describe behaviour contracts and should clearly explain:

* What the class represents.
* Any laws instances should satisfy.

```haskell
-- | Types that can be converted to JSON.
--
-- Instances should satisfy:
--
-- prop_roundtrip x = decode (encode x) == Just x
class ToJSON a where
  encode :: a -> String


instance Show User where
  -- | Displays the user in a human-readable form.
  show user = userName user
```

### Generating documentation

For building the documentation within a *stack project*, you can use `stack haddock` command, which generates `index.html` file.

### Publish your project

Publishing a Haskell project means more than just pushing code to GitHub. A well-prepared project is easier to use, easier to maintain, and more likely to be adopted.

This section provides a short checklist of what should be done before publishing:

1. **Clean and structure the project** = Make sure your project has a clear structure, without any unused modules or dead code. Follow common conventions for directory layout (e.g., `src/` for source code, `test/` for tests). Separate clearly internal API from public API.
2. **Write proper documentation** = Use Haddock to document your modules, functions, types, and type classes. Focus on explaining the meaning, assumptions, edge cases, and examples. Make sure the generated documentation is clear and complete.
3. **Provide good README** = Your README should explain what the project does, how to install it, how to use it, and where to find documentation. Include examples and links to the generated Haddock docs.
4. **Select a LICENSE** = Choose an appropriate open-source license for your project and include it in the repository. This clarifies how others can use, modify, and distribute your code.
5. **Ensure buildability** = Make sure your project builds correctly with `stack build` and that all tests pass with `stack test`. This is crucial for users who want to use or contribute to your project.
6. **Add tests** = Include a comprehensive test suite to ensure your code works as expected and to give confidence to users and contributors. Ideally, use CI tools to run tests automatically on each commit or pull request.
7. **Setup versioning and releases** = Use semantic versioning (or other widely used schema) and consider tagging releases in Git. This helps users understand the stability and compatibility of different versions of your project.
8. **Prepare for contributions** = If you want others to contribute, provide clear guidelines for contributing (e.g., in a CONTRIBUTING.md file) and consider setting up issue templates and pull request templates.
9. **Publish to Hackage** = If your project is a library, consider publishing it to Hackage so that others can easily install it with `cabal` or `stack`. Follow the guidelines for packaging and uploading to Hackage.

## Debugging

Even in a strongly typed language, bugs happen: wrong assumptions, incorrect edge cases, unexpected IO behaviour, and logic errors that are perfectly type-correct. In Haskell, debugging also has one extra twist: laziness can delay evaluation, so “where the error originates” and “where it shows up” can differ.

A useful workflow is:

1. Get visibility (print/trace/log key values)
2. Reproduce deterministically (small input, minimal program)
3. Use the right tool:

  * `Debug.Trace` for quick *debugging via printing*
  * GHCi debugger for stepping + inspecting
  * IDE (VS Code + HIE/HLS) for navigation, types, and quick feedback

### Tracing with `Debug.Trace`

The simplest debugging tool is `trace` from [`Debug.Trace`](https://hackage-content.haskell.org/package/base/docs/Debug-Trace.html): it prints a message and then returns the value you provide (so you can insert it into pure code). There are many useful variants such as traceShow, traceId, traceStack, traceIO, traceM, etc.

```haskell
import Debug.Trace

func a b = trace ("func " ++ show a ++ " " ++ show b) undefined
```

Trace tips that save time:

* Prefer `traceShowId` when you just want to peek at a value while keeping the expression readable:

```haskell
import Debug.Trace (traceShowId)

foo xs = traceShowId (take 5 xs) ++ "..."
```
* Use `traceM`/`traceIO` when you're in monadic code (`IO`, `StateT`, etc.). The file already points out `traceIO` and `traceM` as standard options. 
* Laziness gotcha: sometimes nothing prints because the value isn't demanded yet. If you expect a trace and don't see it, ensure the expression is actually evaluated (e.g., by printing it, using `evaluate`, or by restructuring so it's forced).
* Keep traces out of releases: a common pattern is to guard them behind a CPP flag, or keep them local and remove them once fixed.

### GHCi debugger

When tracing isn't enough, GHCi's debugger can do real debugging: breakpoints, stepping, inspecting variables, and tracing. 

A basic session looks like this (setting a breakpoint, calling the function, inspecting bindings, continuing): 

Key commands to know:

* `:break <line>` = set a breakpoint (or use `:break <module> <line>`)
* `:continue` = continue until the next breakpoint
* `:step` / `:steplocal` = step into evaluation (useful but can be confusing with laziness)
* `:show breaks` = list breakpoints 
* `:print <name>` / `:force <name>` = inspect or force evaluation of a binding
* `:abandon` = abandon the current evaluation and return to the prompt

If you're using Stack or Cabal, load modules through the tool so flags match your project:

```
stack ghci / cabal repl
```

Compile with debug-friendly settings when needed:

* -O0 while debugging (optimizations can rearrange code and make stepping harder)
* ensure you have symbols / not stripping (usually default in dev builds)

### IDE (VS Code)

For day-to-day development, the fastest *debugging* often happens before runtime:

* hover to see inferred types,
* jump-to-definition across modules,
* rename symbols safely,
* see warnings/errors inline,
* get import/code actions.

You need the Haskell Language Server (HLS) for using these features in VS Code (or other IDEs). It provides real-time feedback on types, errors, and warnings as you write code. This can catch many issues before you even run the program.

Moreover, there are ongoing efforts on improving debugging in IDEs such as [haskell-debugger](https://github.com/well-typed/haskell-debugger).

Another alternative was [debug](https://hackage.haskell.org/package/debug) package, but it is not maintained anymore and it is not compatible with GHC 9.10 and later.

## Performance and Optimization

Performance work in Haskell (and in general) should always start with two questions:

1. **How slow is the program really?**
2. **Is the problem caused by the implementation, or by the algorithm itself?**

We will explore these questions using a simple example: computing Fibonacci numbers.  
This example is not chosen because it is practical, but because it clearly demonstrates the difference between:

* a poor algorithm,
* a reasonable algorithm,
* and low-level optimizations.

### Case 0: Naive Fibonacci

```haskell
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

This definition is elegant and closely follows the mathematical definition. However, it is very inefficient.

Each call to `fibonacci n` recursively computes both `fibonacci (n-1)` and `fibonacci (n-2)`, which in turn recompute the same values again and again. As a result, the number of function calls grows exponentially.

This leads to **exponential time complexity**, making even moderately large inputs very slow.

### Measuring performance

Before optimizing, we should always measure performance:

* to verify that there is actually a problem,
* to understand how severe it is, and
* to establish a **baseline** for comparison.

There are several ways to measure performance in Haskell, each useful at a different stage.

#### Basic wall-clock time

The simplest way to measure performance is using an external tool such as time:

```
/usr/bin/time -p runhaskell FibonacciNaive.hs 25
```

This (by default) reports:

* `real` — total elapsed (wall-clock) time
* `user` — time spent executing your program
* `sys` — time spent in system calls

This approach is:

* quick and easy,
* useful for a rough estimate,
* but not very precise or reproducible.

It is a good first check, but not suitable for detailed benchmarking. There are also other possible options of `time` command and you can observe more details e.g. with various flags. There are similar alternatives such as `hyperfine` that provide more features and better output formatting.

#### In-Haskell time measurement

You can also measure time directly inside your program using getCurrentTime:

```haskell
import Data.Time.Clock

main :: IO ()
main = do
  start <- getCurrentTime
  print (fibonacci 25)
  end <- getCurrentTime
  print (diffUTCTime end start)
```

This approach:

* is simple and portable,
* allows measuring specific parts of a program,
* but is still affected by noise (GC, scheduling, etc.).

It is useful for quick experiments, but not for reliable comparisons.

#### Benchmarking with Criterion

For accurate and reproducible measurements in Haskell (incl. detailed reporting), we use the [criterion](https://hackage.haskell.org/package/criterion) library:

```haskell
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bench ("fibonacci 15") $ whnf fibonacci 15
  , bench ("fibonacci 20") $ whnf fibonacci 20
  , bench ("fibonacci 25") $ whnf fibonacci 25
  ]
```

Criterion:

* runs benchmarks many times,
* uses statistical analysis,
* reports mean time, variance, and confidence intervals.

This makes it much more reliable than manual timing. You can also generate reports and compare different implementations side by side:

```bash
stack run -- --output report.html
```

There are also many options for configuring benchmarks, such as controlling the number of iterations, the time limit, and the output format. Criterion is the gold standard for benchmarking in Haskell and is widely used in the community.

```bash
stack run -- --help
```

> Important: benchmarking should be done on compiled code, not interpreted code (e.g. via `runhaskell`).

#### Measuring allocations with Weigh

Execution time is only one aspect of performance. Memory usage is equally important.

The [weigh](https://hackage.haskell.org/package/weigh) library measures allocations:

```haskell
import Weigh

main :: IO ()
main = mainWith $ do
  func "fibonacci 15" fibonacci 15
  func "fibonacci 20" fibonacci 20
  func "fibonacci 25" fibonacci 25
```

It reports:

* total bytes allocated,
* number of garbage collections.

This is especially useful in Haskell, where performance is often affected by:

* allocation of thunks,
* laziness,
* and data representation.

### Profiling

Benchmarking tells us *how slow a program is*. Profiling helps us understand *why it is slow*.

In Haskell, profiling is especially useful for distinguishing between:

* a bad algorithm,
* bad evaluation behavior, and
* excessive allocation or memory retention.


To enable profiling with Stack, build your project with (will install own extra dependencies):

```bash
stack build --profile
```

Then, we can run the program with profiling enabled (which generates a `.prof` file):

```bash
stack exec --profile fib -- +RTS -p
```

In the resulting `.prof` file, we can see which functions are taking the most time and allocating the most memory. This can help us identify bottlenecks and understand where to focus our optimization efforts. In our case, it should show us that the `fibonacci` function is taking most of the time and allocating a lot of memory due to the exponential growth of calls.

> Profiling (nor benchmarking) does not fix the problem — it helps us locate it.

Various flags for GHC profiling:

* `-prof` (enabled under the hood by [Stack `--profile`](https://docs.haskellstack.org/en/stable/commands/build_command/#-profile-flag)) builds the program with profiling support
* `-fprof-auto` automatically inserts cost centres into top-level functions allows profiling to attribute time and memory to functions (also enabled by Stack `--profile`)
* `+RTS -p` enables runtime profiling, produces a `.prof` report with time and allocation data

Without cost centres, the profiling output is often too coarse to be useful.

To further analyze memory usage, you can use `+RTS -hc` to generate a heap profile, which shows how memory is allocated across different parts of the program. Then, you can visualize the heap profile using tools like `hp2ps`:

```bash
stack exec --profile fib -- +RTS -hc
hp2ps fib.hp
ps2pdf fib.ps
```

Heap profiling helps detect:

* space leaks,
* unexpected memory growth,
* large retained data structures.

For our case, it can be empty due to laziness and thunks piling up from the exponential calls.

More interesting profiling exercise can be done with a comparison of lazy vs strict `foldl`:

```haskell
sumBad :: [Int] -> Int
sumBad = foldl (+) 0

sumGood :: [Int] -> Int
sumGood = foldl' (+) 0

main :: IO ()
main = do
  print (sumBad [1..10000000])
  print (sumGood [1..10000000])
```

### Case 1: The biggest optimization: a better algorithm

The main problem with the naive Fibonacci implementation is not laziness, boxing, or the choice of numeric type. The real problem is the **algorithm itself**.

A much better approach is to compute the sequence iteratively using accumulators:

```haskell
fibonacciAcc :: Integer -> Integer
fibonacciAcc n = go 0 1 n
  where
    go a _ 0 = a
    go a b k = go b (a + b) (k - 1)
```

Alternatively, we can lazy list with `zipWith`:

```haskell
fibonacciList :: [Integer]
fibonacciList = 0 : 1 : zipWith (+) fibonacciList (tail fibonacciList)

fibonacciFromList :: Integer -> Integer
fibonacciFromList n = fibonacciList !! fromIntegral n
```

Now we can benchmark and profile these implementations to see the dramatic improvement. The time complexity is now linear, and the memory usage is constant (for the accumulator version) or linear (for the list version, due to thunks).

> When a program is slow, the first question should often be: **Can we solve the problem in a better way?**

Low-level optimizations can improve a good implementation, but they rarely compensate for a poor algorithm.

### Case 2: Strict evaluation

Even after choosing a much better algorithm, evaluation strategy still matters.

The accumulator-based Fibonacci function above is structurally efficient, but because Haskell is lazy, the intermediate values may still be represented as unevaluated expressions (thunks). For large inputs, this can increase memory usage and sometimes reduce performance.

We can force the accumulators to be evaluated eagerly:

```haskell
{-# LANGUAGE BangPatterns #-}

fibonacciAccBang :: Integer -> Integer
fibonacciAccBang n = go 0 1 n
  where
    go !a !_ 0 = a
    go !a !b k = go b (a + b) (k - 1)
```

Here the `!` patterns make the arguments strict. Instead of building up deferred computations, the function evaluates the accumulator values as it goes.

This often improves performance by:

* reducing thunk allocation,
* lowering memory usage,
* avoiding space leaks.

The important lesson is that strictness is usually a secondary optimization:

* first choose a better algorithm, then
* improve evaluation behavior if needed.

Strictness can make a good implementation better, but it does not change the fundamental complexity of the problem.

### Case 3: Choosing the right types

The next question is whether we are using the most appropriate data type.

So far, we have used `Integer`... but what if we switch to `Int` or `Double`?

* `Int` is a fixed-size integer type (usually 64 bits) that can overflow, but is more efficient for small numbers.
* `Double` is a floating-point type that can represent very large numbers, but with limited precision.

```haskell
fibonacciInt :: Int -> Int
fibonacciInt 0 = 0
fibonacciInt 1 = 1
fibonacciInt n = fibonacciInt (n - 1) + fibonacciInt (n - 2)

fibonacciDouble :: Double -> Double
fibonacciDouble 0 = 0
fibonacciDouble 1 = 1
fibonacciDouble n = fibonacciDouble (n - 1) + fibonacciDouble (n - 2)
```

Compared with `Integer`, `Int` is usually:

* faster,
* smaller, and
* more efficient in memory.

However, `Int` has a fixed size and can overflow. That means it is only appropriate when the range of values is known to be safe. `Double` is on the other hand, not suitable for counting Fibonacci numbers due to precision issues and complexity of floating-point arithmetic.

Choosing the right type will usually not matter as much as choosing the right algorithm, but it can still provide a noticeable improvement once the algorithm is already good. This is of course different when it comes to **types of data structures**, where the choice of representation can have a significant impact on performance.

### Case 4: Boxed vs unboxed values (advanced)

Another source of overhead in Haskell is how values are represented in memory.

By default, Haskell uses **boxed values**.

* A boxed value is stored on the heap.
* Variables contain a pointer to that value.
* This allows laziness and uniform representation.

For example, an `Int` is typically represented as a pointer to a heap object.

This indirection introduces overhead:

* extra memory allocation,
* pointer dereferencing, and
* more pressure on the garbage collector.

#### Unboxed values

In some situations, GHC can use **unboxed values**, which are:

* stored directly (not on the heap),
* not wrapped in a pointer,
* represented as raw machine values.

Unboxed values can be much more efficient because they avoid allocation, reduce indirection, can significantly improve performance in tight loops.

However, they come with limitations:

* they cannot be lazy,
* they cannot be used polymorphically,
* they make code less flexible and harder to read.

#### When does this matter?

In practice, you usually do **not** write unboxed code directly.

Instead, GHC already helps you by:

- optimizing strict code,
- unboxing fields when possible,
- specializing functions.

This means that writing strict code (`!` patterns) with concrete types (`Int`, `Double`), often allows GHC to generate efficient, unboxed machine code automatically.

> Unboxed values are a **fine-tuning tool**, not a primary optimization strategy.

#### Example of explicit unboxing

You can use so-called magic hash to work with unboxed types directly, but this is an advanced topic and usually not necessary for most Haskell programming and you should be careful when doing it:

```haskell
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

-- Unboxed naive Fibonacci
fibonacci# :: Int# -> Int#
fibonacci# 0# = 0#
fibonacci# 1# = 1#
fibonacci# n  =
  fibonacci# (n -# 1#) +# fibonacci# (n -# 2#)

-- Boxed wrapper (so we can use normal Int in main)
fibonacciUnboxed :: Int -> Int
fibonacciUnboxed (I# n) = I# (fibonacci# n)

main :: IO ()
main = print (fibonacciUnboxed 30)
```

Note that we are using `Int` and cannot use `Integer` here, because `Integer` is a boxed type and cannot be unboxed. This code is more efficient than the boxed version, but it is also more complex and less flexible. It is generally recommended to let GHC handle unboxing for you by writing clear, strict code with appropriate types.

### Case 5: Compiler optimizations

So far, all improvements were made by changing the source code. However, the compiler itself can significantly improve performance.

#### Optimization levels

GHC provides different optimization levels:

```bash
ghc -O0 Main.hs   # no optimization
ghc -O1 Main.hs   # moderate optimization
ghc -O2 Main.hs   # aggressive optimization
```

This can be used also with Stack:

```bash
stack build --ghc-options="-O2"
```

Or in `package.yaml`:

```yaml
ghc-options:
  - -O2
```

### Case 6: Parallelism and concurrency

Another possible optimization is to use multiple CPU cores. This can help when:

* the computation is large enough,
* the work can be split into independent parts, and
* the overhead of coordination is small compared with the useful work.

However, parallelism is not automatically beneficial. A poor parallel design can easily be slower than a sequential program.

#### Not-so-good parallel Fibonacci

A first idea might be to evaluate both recursive calls in parallel:

```haskell
import Control.Parallel

fibonacciParBad :: Int -> Int
fibonacciParBad 0 = 0
fibonacciParBad 1 = 1
fibonacciParBad n =
  let x = fibonacciParBad (n - 1)
      y = fibonacciParBad (n - 2)
  in x `par` (y `pseq` (x + y))
```

Here:

* `par` sparks the computation of `x` in parallel,
* `pseq` forces the evaluation of `y` before returning the result.

This looks attractive: the two recursive branches are independent, so why not evaluate them in parallel?

The problem is that this creates far too many tiny parallel tasks.

For naive Fibonacci, the recursion tree grows exponentially. If we try to spark parallel work at every node, we quickly get:

* huge scheduling overhead,
* too many sparks,
* contention between threads,
* and often worse performance than the sequential version.

In other words, this version parallelizes too eagerly.

> Creating parallel work is not free. If each task is too small, the overhead dominates the useful computation.

#### Better parallel Fibonacci with cutoff

A simple improvement is to create parallel work only for sufficiently large subproblems:

```haskell
import Control.Parallel

cutoff :: Int
cutoff = 30

fibonacciParCutoff :: Int -> Int
fibonacciParCutoff 0 = 0
fibonacciParCutoff 1 = 1
fibonacciParCutoff n
  | n <= cutoff = fibonacciParCutoffSeq n
  | otherwise =
      let x = fibonacciParCutoff (n - 1)
          y = fibonacciParCutoff (n - 2)
      in x `par` (y `pseq` (x + y))

fibonacciParCutoffSeq :: Int -> Int
fibonacciParCutoffSeq 0 = 0
fibonacciParCutoffSeq 1 = 1
fibonacciParCutoffSeq n =
  fibonacciParCutoffSeq (n - 1) + fibonacciParCutoffSeq (n - 2)
```

This version is still naive Fibonacci, but it avoids creating sparks for very small tasks.

That usually works better because:

* large tasks are worth parallelizing,
* small tasks are computed sequentially,
* overhead is reduced.

> Good parallel programs need a reasonable task granularity.

#### Master-worker task parallelism

The previous example still follows the recursive structure of naive Fibonacci.
A more structured approach is to use a master-worker design.

The idea is:

* the master creates a fixed number of larger tasks,
* the workers compute those tasks independently,
* the master combines the results.

For Fibonacci itself, this is somewhat artificial, because the algorithm is still bad.
But it demonstrates an important design principle: parallel work should be coarse enough to justify the overhead.

One way to do this is to split the top of the recursion tree into a few large subproblems, and let each worker solve one subtree.

```haskell
import Control.Concurrent.Async

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacciParTop :: Int -> IO Int
fibonacciParTop 0 = pure 0
fibonacciParTop 1 = pure 1
fibonacciParTop n = do
  ax <- async (pure $ fibonacci (n - 1))
  ay <- async (pure $ fibonacci (n - 2))
  x <- wait ax
  y <- wait ay
  pure (x + y)
```

This version creates only two top-level tasks instead of parallelizing every recursive call. That is much more controlled:

* only a small number of threads is created,
* each task is large enough to matter, and
* the structure is easier to reason about.

This is a simple example of **task parallelism**: *different workers perform different independent tasks*.

#### Data parallel viewpoint

Another way to think about parallelism is data parallelism, where the same computation is applied to multiple independent pieces of data.

Naive Fibonacci is not a natural data-parallel algorithm for a single input, but it becomes data-parallel if we want to compute many Fibonacci values:

```haskell
import Control.Concurrent.Async

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacciManyPar :: [Int] -> IO [Int]
fibonacciManyPar xs = mapConcurrently (pure . fibonacci) xs

main :: IO ()
main = do
  results <- fibonacciManyPar [35, 36, 37, 38]
  print results
```

Here:

* the data is the list of inputs,
* each worker computes one result, and
* the same operation is applied independently to all items.

This is often a much more realistic parallel pattern than recursive parallel Fibonacci.

#### Concurrency vs parallelism

It is also useful to distinguish two related ideas:

1) Concurrency is about structuring a program so that multiple activities can make progress independently.
2) Parallelism is about actually executing work simultaneously on multiple CPU cores.

For pure numerical computations like Fibonacci, we are mainly interested in parallelism.

Libraries such as async are often used for both purposes:

* concurrency for organizing tasks,
* parallelism when those tasks run on multiple cores.

#### Running parallel programs

To run a parallel Haskell program, you need to enable the threaded runtime and specify the number of cores to use:

```bash
stack exec -- fib-par -- +RTS -N
stack exec -- fib-par -- +RTS -N4
```

Here, `-N` uses all available cores, while `-N4` limits it to 4 cores. Without this option, the program may still use concurrency abstractions, but it will not execute pure computations in parallel across multiple cores.

### Case 7: Foreign Function Interface (FFI)

Another way to improve performance, or to reuse existing code, is to call functions written in another language.

Haskell supports this through the **Foreign Function Interface (FFI)**.

The FFI allows a Haskell program to:

- call functions implemented in C,
- pass data between Haskell and foreign code,
- and combine high-level Haskell code with low-level libraries.

This is useful when:

- a performance-critical function already exists in another language,
- we want to use a mature external library,
- or we need to integrate with low-level system APIs.

However, FFI should be treated as a tool, not as the default solution.

> Rewriting a poor algorithm in another language does not make it a good algorithm.

To keep the example consistent with the rest of this chapter, we will use the naive Fibonacci function again.

#### C implementation

First, let us implement naive Fibonacci in C:

```c
#include <stdint.h>

int64_t fibonacci(int64_t n) {
    if (n == 0) return 0;
    if (n == 1) return 1;
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

This can be saved in a file such as `fibonacci.c`.

This implementation has the same algorithmic complexity as the naive Haskell version: it is still exponential. The difference is only the language and runtime representation.

#### Haskell FFI binding

Now we declare (bind) the C function in Haskell:

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Int (Int64)

foreign import ccall "fibonacci"
  c_fibonacci :: Int64 -> Int64

main :: IO ()
main = print (c_fibonacci 25)
```

In C, the function uses `int64_t`, so in Haskell we use `Int64` from `Data.Int`. It is important that the types on both sides correspond correctly. When using FFI, type mismatches can lead to incorrect results or crashes.

Without Stack project, you can simply compile the C code and link it with the Haskell code:

```bash
gcc -c fibonacci.c -o fibonacci.o
ghc Main.hs fibonacci.o -o fib-ffi
./fib-ffi
```

With Stack, you can specify the C source file in `package.yaml`:

```yaml
executables:
  fib:
    main: Main.hs
    source-dirs: app
    ghc-options:
      - -O2
    c-sources:
      - c-src/fibonacci.c
```

Of course, the best way is to have also optimized C code, without the naive algorithm, but the point here is to demonstrate the FFI mechanism itself.

```c
// Fibonacci number with O(n) time complexity using iteration
#include <stdint.h>

int64_t fibonacci(int64_t n) {
    if (n <= 1) return n;

    int64_t a = 0, b = 1;

    for (int64_t i = 2; i <= n; i++) {
        int64_t tmp = a + b;
        a = b;
        b = tmp;
    }

    return b;
}
```

or even more:

```c
// Fibonacci number with O(log n) time complexity using fast doubling
#include <stdint.h>

static void fib_fast_doubling(int64_t n, int64_t* a, int64_t* b) {
    if (n == 0) {
        *a = 0;
        *b = 1;
        return;
    }

    int64_t x, y;
    fib_fast_doubling(n / 2, &x, &y);

    int64_t c = x * (2 * y - x);
    int64_t d = x * x + y * y;

    if (n % 2 == 0) {
        *a = c;
        *b = d;
    } else {
        *a = d;
        *b = c + d;
    }
}

int64_t fibonacci(int64_t n) {
    int64_t a, b;
    fib_fast_doubling(n, &a, &b);
    return a;
}
```

#### Important FFI caveat

When discussing FFI in a performance chapter, it is easy to give the wrong impression.

The point is **not**:

> “If Haskell is slow, rewrite it in C.”

The real lesson is:

* first choose a better algorithm,
* then profile the program, and
* only then consider whether FFI is justified.

Often, a well-written Haskell implementation with a better algorithm will outperform a foreign implementation of a poor algorithm.

#### Why FFI is useful beyond performance

FFI is not only about speed. A major practical use of FFI is interoperability with existing libraries.

For example, a Haskell program may call C code to use:

* system libraries,
* graphics libraries,
* database libraries,
* scientific computing libraries, or
* operating system APIs.

In such cases, FFI is valuable even when performance is not the primary concern.

Moreover, FFI can be naturally used with other languages that can expose a C-compatible API, such as C++, Rust, or Fortran. This allows Haskell to interoperate with a wide range of ecosystems and leverage existing codebases.

The most common and direct FFI in Haskell is with C, because GHC has strong support for it and many system libraries expose a C interface. However, interoperability is not limited to C.

Depending on the tooling and ecosystem, Haskell can also interoperate with:

* C++ (often through C-compatible wrappers),
* Rust (typically by exposing a C ABI),
* Fortran, and
* other languages that can provide C-callable functions.

In practice, many languages use C as a common interoperability layer.

That means the typical pattern is:

1) write foreign code in another language,
2) expose a C-compatible API,
3) call that API from Haskell via FFI.

Interoperability also works in the other direction. Not only can Haskell call foreign code, but foreign programs can also call Haskell code. This is useful when:

* embedding Haskell into a larger system,
* exposing Haskell functionality to another application, or
* building mixed-language systems.

So FFI is not just *Haskell calling C* — it is a bridge between language runtimes.

### Performance considerations

In this chapter, we explored several ways to improve performance in Haskell programs.  
These techniques operate at different levels, and it is important to understand their relative impact.

---

#### Where does performance come from?

A program can be slow for several fundamentally different reasons:

- **Algorithmic complexity**  
  The program is doing too much work.

- **Evaluation strategy**  
  The program builds unnecessary thunks or retains too much data.

- **Data representation**  
  The program uses inefficient types or memory layouts.

- **Execution model**  
  The program does not take advantage of available parallelism.

Understanding which of these is the real issue is the key to effective optimization.

#### The order of optimization

The most important lesson of this chapter is that not all optimizations are equally important.

A useful rule of thumb is:

1. **Measure and profile first**
2. **Improve the algorithm or data structure**
3. **Fix evaluation issues (strictness, space leaks)**
4. **Choose appropriate types (`Int` vs `Integer`, etc.)**
5. **Rely on compiler optimizations (`-O2`)**
6. **Consider parallelism for large independent work**
7. **Use FFI only when justified**

Each step typically gives diminishing returns compared to the previous one.

> The biggest improvements usually come from changing the algorithm, not from low-level tuning.

#### Avoid premature optimization

It is tempting to optimize code early, especially in a language like Haskell where evaluation behavior is subtle.

However:

- optimization without measurement can waste time,
- and may make code more complex without real benefit.

Instead:

- write clear and correct code first,
- measure and profile,
- and only then optimize the parts that matter.

#### Balance clarity and performance

Many optimizations (such as strictness annotations or unboxed values) make code less abstract and harder to read.

This creates a trade-off:

- **clear, declarative code** is easier to understand and maintain,
- **low-level optimized code** can be faster but more complex.

In most cases:

- start with clear code,
- optimize only where necessary,
- and keep optimized parts small and well-documented.

#### Parallelism is not a shortcut

Parallelism can improve performance, but only under the right conditions:

- tasks must be large enough,
- overhead must be small,
- and the work must be independent.

It is **not a replacement for a good algorithm**.

#### FFI is a tool, not a solution

Calling foreign code can be useful, but:

- it introduces complexity,
- it reduces portability,
- and it does not solve fundamental design problems.

Use FFI when it provides clear value, not as a default optimization strategy.

#### Final takeaway

Effective performance work is not about applying every available technique.   It is about choosing the *right* technique for the problem.

In practice, this means:

> Measure → Understand → Improve → Repeat

and always remembering:

> **Algorithm > evaluation > representation > low-level tuning**

This hierarchy is the key to writing efficient Haskell programs.

## Task assignment

The homework to practice IO (again), testing, and writing project documentation is in repository [MI-AFP/hw07](https://github.com/MI-AFP/hw07).

## Further reading

* [A Gentle Introduction to Haskell: Input/Output](https://www.haskell.org/tutorial/io.html)
* [Haskell: Simple input and output](https://en.wikibooks.org/wiki/Haskell/Simple_input_and_output)
* [WikiBooks Haskell: Testing](https://en.wikibooks.org/wiki/Haskell/Testing)
* [Haddock User Guide](https://www.haskell.org/haddock/doc/html/index.html)
* [QuickCheck and Magic of Testing](https://www.fpcomplete.com/blog/2017/01/quickcheck)
* [Haskell: Debugging](https://wiki.haskell.org/Debugging)
* [Haskell: Performance](https://wiki.haskell.org/Performance)
* [Haskell: Concurrency](https://wiki.haskell.org/Concurrency)
* [GHC: Concurrent and Parallel Haskell](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/lang-parallel.html)
