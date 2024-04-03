# Tests, Documentation, Debugging and Performance

## Testing

Haskellers sometimes say that "When the programme compiles, it is correct!" There is a lot of truth to it, as you may have already experienced: the strong static type system does not allow you to make many errors, especially the most common (and insidious) "stupid" ones. At the same time, this saying is obviously exaggerated and there is still quite some space for a programme to be buggy. This is why traditional unit testing has its place in Haskell. Moreover, Haskell also offers an even more powerful types of testing such as property testing and mutation testing.

### HUnit

[HUnit](https://hackage.haskell.org/package/HUnit) is a unit testing framework for Haskell, inspired by the JUnit tool for Java and similar ones. For developers familiar with unit testing, this framework is very simple to use. First, you define several test cases that you put in a test list (instead of test class as in Java). A single test case is composed optionally of some data preparation and assertions. The result of running tests consists of four numbers: cases, tried, errors and failures.

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

### QuickCheck

A different approach to testing is provided by [QuickCheck](https://hackage.haskell.org/package/QuickCheck). It is a library for random testing of program properties. You can specify some "laws" in your application and this library will check with a given number of randomly (but smartly) generated instances if there is not some counterexample violating the laws. Such laws or specifications are expressed in Haskell, using combinators defined in the QuickCheck library. QuickCheck provides combinators to define properties, to observe the distribution of test data, and to define test data generators. All from a simple example to complex tutorials of such definitions are explained in the [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).

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

[Hspec](https://hackage.haskell.org/package/hspec) is a testing framework for Haskell. It is inspired by the Ruby library RSpec. Some of Hspec's distinctive features are:

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
* `shouldSatisfy` = test if result satisfies given property as function `:: a -> Bool`
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

### MuCheck

Mutation Testing is a special type of software testing where certain statements in a source code are mutated (changed by mutation operators) and then we check if the test cases recognize the errors. In Haskell, there is [MuCheck](https://hackage.haskell.org/package/MuCheck).

```haskell
-- https://github.com/vrthra/mucheck
import Test.MuCheck.TestAdapter.AssertCheck

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
    where l = filter (< x) xs
          r = filter (>= x) xs

{-# ANN sortEmpty "Test" #-}
sortEmpty = assertCheck $ qsort [] == []

{-# ANN sortSorted "Test" #-}
sortSorted = assertCheck $ qsort [1,2,3,4] == [1,2,3,4]

{-# ANN sortRev "Test" #-}
sortRev = assertCheck $ qsort [4,3,2,1] == [1,2,3,4]

{-# ANN sortSame "Test" #-}
sortSame = assertCheck $ qsort [1,1,1,1] == [1,1,1,1]

{-# ANN sortNeg "Test" #-}
sortNeg = assertCheck $ qsort [-1,-2,3] == [-2,-1,3]

main = do
         assertCheckResult sortEmpty
         assertCheckResult sortSorted
         assertCheckResult sortRev
         assertCheckResult sortSame
         assertCheckResult sortNeg
```

```
% cabal run sample-test
% cabal run mucheck -- -tix sample-test.tix Examples/AssertCheckTest.hs
Total mutants: 19 (basis for %)
        Covered: 13
        Sampled: 13
        Errors: 0  (0%)
        Alive: 1/19
        Killed: 12/19 (63%)
```

Sadly this interesting project with a [paper](https://www.researchgate.net/publication/266659188_MuCheck_An_extensible_tool_for_mutation_testing_of_haskell_programs) published is dead for some years. Hopefully, someone will take over its maintenance or at least fork it and contribute to it (:wink: term project). It also has some interesting integrations like [MuCheck-QuickCheck](https://hackage.haskell.org/package/MuCheck-QuickCheck), [MuCheck-HUnit](https://hackage.haskell.org/package/MuCheck-HUnit), or [MuCheck-Hspec](https://hackage.haskell.org/package/MuCheck-Hspec).

## Haddock (documentation)

Haskell projects, like any other projects, should have good documentation of source code. In Haskell,  the tool for documentation is called [Haddock](https://www.haskell.org/haddock/) and works similarly to JavaDoc or JSDoc or other XYDoc by annotating the code using comments with special meaning:

```haskell
{-|
Module      : W
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module W where

-- | The 'square' function squares an integer.
-- It takes one argument, of type 'Int'.
square :: Int -> Int
square x = x * x

class C a where
   -- | This is the documentation for the 'f' method
   f :: a -> Int
   -- | This is the documentation for the 'g' method
   g :: Int -> a

data R a b =
  R { -- | This is the documentation for the 'a' field
      a :: a,
      -- | This is the documentation for the 'b' field
      b :: b
    }

data S a b =
  S { a :: a  -- ^ This is the documentation for the 'a' field
    , b :: b  -- ^ This is the documentation for the 'b' field
    }
```

For more information about using Haddock and writing the documentation of source code in Haskell check http://haskell-haddock.readthedocs.io/en/latest/index.html or https://www.haskell.org/haddock/doc/html/ (the examples above are from this documentation).

For building the documentation within a *stack project*, you can use `stack haddock` command, which generates `index.html` file.

### Publish your project

If you think that other people might be interested in your project and want to use it standalone or as part of their project (as a dependency), you can publish your project on GitHub and also on Hackage:

* [GitHub - create a repo](https://help.github.com/articles/create-a-repo/)
* [Hackage - upload](https://hackage.haskell.org/upload)

Your project should be:
* tested (write tests for your project so you can prove that it is working properly),
* documented (try to describe everything in your code to "strangers" with low Haskell knowledge),
* licensed (pick a suitable license - https://choosealicense.com can help you).

Another advantage of publishing is that your project can get attention and community can help you improve it -- they create issues, forks and pull requests.

### Using CI

When you are developing a project and sharing it with a community, you want to show that it is working well and you also want to check if contributions to your code are not breaking it. For that, you can use CI tools (continuous integration) which allows you to run tests (or other scripts) automatically. There are many CI tools these days: Travis CI, Jenkins, Circle CI, Appveyor, Semaphore, GitLab CI, GitHub Actions, etc.

All (well, almost all) CIs need some specification what they should do with your project. If you are using GitHub, then Travis CI is one of the good choices for you. Just create `.travis.yml` in your repository and register project in Travis CI.

```yaml
# https://docs.haskellstack.org/en/stable/travis_ci/
sudo: false
# Not Haskell with cabal but with stack tool
language: c
cache:
  directories:
    - ~/.stack
addons:
  apt:
    packages:
      - libgmp-dev
before_install:
  - mkdir -p ~/.local/bin
  - export PATH=$HOME/.local/bin:$PATH
  - travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
install:
  - stack --no-terminal --install-ghc test --only-dependencies
script:
  - stack --no-terminal test --haddock --no-haddock-deps
```

For Haskell, you can use `.travis.yml` above or read the [documentation](https://docs.travis-ci.com/user/languages/haskell/).

Another example shows a similar configuration but for GitLab CI (notice caching and use of system GHC from the used base image):

```yaml
image: haskell:9.4.8

variables:
  STACK_ROOT: "${CI_PROJECT_DIR}/.stack"

cache:
  paths:
    - .stack
    - .stack-work
    - target

stages:
  - test

test:
  stage: test
  script:
    - stack test --system-ghc
```

For GitHub Actions in a more complex project (multiple Stack packages, components and Docker images), you can check [ds-wizard/engine-backend](https://github.com/ds-wizard/engine-backend/blob/develop/.github/workflows/build.yml). 

## Performance and Debugging

During this tutorial, we will also take a look how to improve the performance of a Haskell program and how to debug it. We will use very simple example - [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number).

```haskell
import System.Environment

-- | Naive recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main :: IO ()
main = do
    args <- getArgs
    print . fibonacci . read . head $ args
```

### Measuring time and memory

When you want to check the performance of a program and compare two programs or algorithms in terms of time or memory consumption, you need to measure it.

#### Basic `time`

The `time` command is one of the well-known Linux commands for programmers. It can be used to show how long a command takes to run. That makes it Very useful if you are a developer and you want to test the performance of your program or script. Especially to compare the time of programs written in other languages "from outside". For basic usage, you will get three numbers:

- `real` = total time is taken to run the command (the same as if you use your normal stopwatch)
- `user` = amount of time that was spent in user mode
- `sys` = amount of time spent in kernel mode

Then `user`+`sys` gives information how much actual CPU time your process used - in total on all cores. This number can be then higher than `real` if your program uses multiple threads.

```console
% /usr/bin/time -p runhaskell FibonacciNaive.hs 25
75025
real 0.33
user 0.31
sys 0.01
```

But `time` can do a bit more, you can tell how output should look like with additional "numbers" - number of page faults, average total memory use of the process in kilobytes, number of signals delivered to the process, number of socket messages received/sent by the process, exit status of the command, and many others.

```
% /usr/bin/time -f "Elapsed Time: %E\nExit Status: %X\nPage Faults: %F" runhaskell FibonacciNaive.hs 25
75025
Elapsed Time: 0:00.34
Exit Status: 0
Page Faults: 0
```

#### Benchmarking with Criterion

If you are interested in such optimizations and improving your application or comparing various algorithms or their implementations, then you might find interesting to use a benchmarking library. In Haskell is the most used one called [Criterion](http://www.serpentine.com/criterion/). It provides a powerful but simple way to measure software performance. It provides both a framework for executing and analyzing benchmarks and a set of driver functions that makes it easy to build and run benchmarks and to analyze their results.

For simple usage, you just need to work with the `defaultMain` from [Criterion.Main](https://hackage.haskell.org/package/criterion/docs/Criterion-Main.html) as they show in their example:

```haskell
import Criterion.Main

-- | Naive recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench " 5" $ whnf fibonacci 5
               , bench "10" $ whnf fibonacci 10
               , bench "25" $ whnf fibonacci 25
               ]
  ]
```

It has very nice outputs with a form of interactive HTML pages with charts and comparisons and has many options to use.

```console
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

#### Measure allocations with Weigh

The package [weigh](https://hackage.haskell.org/package/weigh) provides a simple interface to measure the memory usage of a Haskell value or function.

```haskell
import Weigh

-- | Naive recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main :: IO ()
main = mainWith $ do
        func "fib  5" fibonacci 5
        func "fib 10" fibonacci 10
        func "fib 25" fibonacci 25
```

It provides a nice output as plain text table, but it is also possible to change the format to markdown.

```console
% ./FibonacciNaiveWeigh

Case     Allocated  GCs
fib  5       1,968    0
fib 10      24,304    0
fib 25  33,509,936   63
```

### Performance

Now we are able to measure something and compare algorithms, but how to improve the numbers we get if we really need it?

#### Basic ideas

When you are not satisfied with the performance of your application, then before any sophisticated optimization steps by using strictness, unboxed types, calling FFI, etc., you should consider if you prefer faster application over better readability. Then another important thing to think about is design if it is not slow by using "naive" algorithm, using an inappropriate data structure (List instead of Set or Map), etc.

**Always** rethink your own code before using other optimization techniques!

```haskell
import System.Environment

-- | Improved recursive algorithm for n-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci = fib 0 1
  where
    fib x _ 0 = x
    fib x y n = fib y (x+y) (n-1)    -- just "one-way" recursion!

main :: IO ()
main = do
    args <- getArgs
    print . fibonacci . read . head $ args
```

Just a very simple re-thinking can have some impact:

```console
% /usr/bin/time -p runhaskell FibonacciBetter.hs 25
75025
real 0.24
user 0.22
sys 0.02
```

```console
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

```console
% ./FibonacciBetterWeigh

Case    Allocated  GCs
fib  5        872    0
fib 10      1,712    0
fib 25     37,000    0
```

#### Boxed vs. Unboxed types

Now, we are going to briefly mention is the difference between boxed and unboxed types. Although it is a low-level concern and with regular Haskell programming, you can avoid these terms, it is good to know what is it about when you see it in other's code or in a documentation.

To support laziness, parametric polymorphism, and other properties, by default Haskell data types are represented uniformly as a pointer to a closure on the heap. These are "boxed" values. An unboxed is represented directly by raw value (i.e., without any indirection). Using unboxed types can lead to time/space optimizations. Having always pointers to a heap-allocated object is fairly slow, so compilers attempt to replace these boxed values with unboxed raw values when possible. Unboxed values are a feature of some compilers that allow directly manipulating these low-level values. Since they behave differently than normal Haskell types, generally the type system is extended to type these unboxed values.

In GHC, unboxed values have a hash mark as a suffix to their name. For instance, the unboxed representation of 42 is 42#. However, you can't pass them to polymorphic functions (like `show` for instance). To allow that, you need to use constructor `I#` that takes an unboxed integer and returns the `Int` (wraps). You can observe [kind](https://wiki.haskell.org/Kind) (*kind of type*, we will look again at kinds with typeclasses) of boxed and unboxed types:

* By default, kind of type is `*` (try in GHCi: `:kind Int`)
* Kind of unboxed type is `#` (try in GHCi: `:kind Int#`)

```haskell
{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts

-- | Naive recursive algorithm for n-th Fibonacci number with
-- unboxed Int types
fibonacci :: Int# -> Int#
fibonacci 0# = 0#
fibonacci 1# = 1#
fibonacci n  = fibonacci (n -# 1#) +# fibonacci (n -# 2#)

main :: IO ()
main = print (I# (fibonacci 25#))
```

```console
% /usr/bin/time -p runhaskell FibonacciUnboxed.hs
75025
real 0.30
user 0.27
sys 0.03
```

For more information, visit [GHC.Exts]() and [GHC.Prim]().

#### Strictness with types

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

We mention this just because of differences in performance of types we are going to describe now. You don't need to use strict or unboxed types within your work if you don't need to have time/space optimizations and if yes, consider reading [Haskell High Performance Programming](https://github.com/TechBookHunter/Free-Haskell-Books/blob/master/book/Haskell%20High%20Performance%20Programming.pdf).

#### GHC optimization flags

If you know optimization with GCC, then you won't be surprised how it works with GHC:

* `-O0` = turn off all optimization
* `-O` or `-O1` = generate good-quality code without taking too long about it
* `-O2` = apply every non-dangerous optimization, even if it means significantly longer compile times (in most cases, there is no significant difference between `-O1` and `-O2`)

Then there are also `-f*`  platform-independent flags, that allows you to turn on and off individual optimizations. For more information, please visit [GHC documentation](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-optimisation.html).

#### Concurrency and Parallelism

Haskell (of course) supports parallelism or concurrency in order to achieve faster and efficient computation. For parallelism and concurrency visit [wiki.haskell.org/Parallel](https://wiki.haskell.org/Parallel). You can both:

* run parallel threads with [Control.Parallel](http://hackage.haskell.org/package/parallel/docs/Control-Parallel.html),
* run simultaneous IO actions with forks.

It is also possible to do distributed computations on clusters but it is far beyond the scope of this course.

```haskell
import Control.Parallel

parfib 0 = return 1
parfib 1 = return 1
parfib n = do
              n1 <- parfib (n - 1)
              n2 <- parfib (n - 2)
              n3 <- (n1 `par` (n2 `seq` (return (n1 + n2 + 1))))
              return n3

main = do x <- parfib 30; print x
```

GHC supports running programs in parallel on an SMP (symmetric multiprocessor) or multi-core machine. Just compile your program using the `-threaded` switch and then run it with RTS option `-N <x>` (where `<x>` is the number of simultaneous threads). See [GHC docs](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/parallel.html) for more information.

#### FFI

As with many other programming languages, Haskell supports [FFI (Foreign Function Interface)](https://wiki.haskell.org/Foreign_Function_Interface) that allows co-operating with programs written with other languages. We've already could see that in the example of Haste in DS Wizard where there were some JS bindings. But you can also use it to call some functions from C++ or Rust:

```cpp
extern "C"{   // need to expose with extern, could use header file .h or .hpp for that
    extern int fib(int n) {
        if(n < 0) return -1;
        int x = 0, y = 1, tmp;
        while(n-- > 0) {
            tmp = x;
            x = y;
            y = tmp + x;
        }
        return x;
    }
}
```

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import System.Environment

foreign import ccall "fib" cfib :: CInt -> CInt

main :: IO ()
main = do
    args <- getArgs
    print . cfib . read . head $ args
```

```console
% gcc -c -o fib.o fib.cpp
% ghc --make -o ffi_fib FibonacciFFI.hs fib.o
Linking ffi_fib ...
% /usr/bin/time -p ./ffi_fib 25
75025
real 0.00
user 0.00
sys 0.00
```

Similarly, there is `foreign export` to expose some Haskell functions to other FFIs. Nice example is here: [jarrett/cpphs](https://github.com/jarrett/cpphs).

### Debugging

Even if you are a good Haskell programmer, things can go wrong and especially in big projects it is a nontrivial challenge to find out where you did some mistake. Going thru the code in multiple functions, inner functions, various modules, etc. can be painful. Luckilly, there are some ways how to debug Haskell program and some are pretty easy and similar to well-known.

#### Tracing with `Debug.Trace`

You should already know how to use GHC and GHCi to compile, link and examine Haskell programs. The simplest tool to use for debugging is the `trace` from [Debug.Trace](https://hackage.haskell.org/package/base.0/docs/Debug-Trace.html) which outputs the trace message given as its first argument, before returning the second argument as its result. There are many more *traces* defined for different cases: `traceShow`, `traceId`, `traceStack`, `traceIO`, `traceM`, etc. So you can use it for custom debugging output anywhere in the code.

For example:

```haskell
func a b = trace ("func " ++ show a ++ " " ++ show b) undefined
```

Or better usage with our example of Fibonacci numbers to see the calls:

```haskell
module Main where

import Debug.Trace

-- | Naive recursive algorithm for n-th Fibonacci number
fib1 :: Integer -> Integer
fib1 0 = trace "fib1 0" 0
fib1 1 = trace "fib1 1" 1
fib1 n = trace ("fib1 " ++ show n) (fib1 (n-1) + fib1 (n-2))

-- | Improved recursive algorithm for n-th Fibonacci number
fib2 :: Integer -> Integer
fib2 = fib 0 1
  where
    fib x _ 0 = trace "fib2 0" x
    fib x y n = trace ("fib2 " ++ show n) (fib y (x+y) (n-1))

main :: IO ()
main = do
    print (fib1 4)
    putStrLn "------------"
    print (fib2 4)
```

```console
% runhaskell FibonacciTrace.hs
fib1 4
fib1 2
fib1 0
fib1 1
fib1 3
fib1 1
fib1 2
fib1 0
fib1 1
3
------------
fib2 4
fib2 3
fib2 2
fib2 1
fib2 0
3
```

#### GHCi debugger

If you need a better debugger, you can use [GHCi debugger](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/ghci-debugger.html) (other compilers, such as Hugs, have some different), which allows:

* setting breakpoints and stepping,
* inspecting variables,
* tracing,
* working with exceptions,
* and so on.

```
Prelude> :l FibonacciNaive.hs
[1 of 1] Compiling Main             ( FibonacciNaive.hs, interpreted )
Ok, modules loaded: Main.
*Main> :break 9
Breakpoint 0 activated at FibonacciNaive.hs:9:10-60
*Main> fib1 5
Stopped in Main.fib1, FibonacciNaive.hs:9:10-60
_result :: Integer = _
n :: Integer = 5
[FibonacciNaive.hs:9:10-60] *Main> :continue
fib1 5
Stopped in Main.fib1, FibonacciNaive.hs:9:10-60
_result :: Integer = _
n :: Integer = 3
[FibonacciNaive.hs:9:28-33] *Main> :show breaks
[0] Main FibonacciNaive.hs:9:10-60
[FibonacciNaive.hs:9:28-33] *Main> :abandon
*Main>
```

#### `debug` package

An interesting solution brings also the [debug](https://hackage.haskell.org/package/debug) package (and related extensions). It uses *Template Haskell* to examine the code and algorithms.

```haskell
{-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module QuickSort(quicksort) where
import Data.List
import Debug

debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]
```

## Task assignment

The homework to practice IO (again), testing, and writing project documentation is in repository [MI-AFP/hw07](https://github.com/MI-AFP/hw07).

## Further reading

* [A Gentle Introduction to Haskell - Input/Output](https://www.haskell.org/tutorial/io.html)
* [Haskell - Simple input and output](https://en.wikibooks.org/wiki/Haskell/Simple_input_and_output)
* [Real World Haskell - Testing and quality assurance](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)
* [WikiBooks - Haskell: Testing](https://en.wikibooks.org/wiki/Haskell/Testing)
* [Haddock User Guide](https://www.haskell.org/haddock/doc/html/index.html)
* [QuickCheck and Magic of Testing](https://www.fpcomplete.com/blog/2017/01/quickcheck)
* [Haskell - Debugging](https://wiki.haskell.org/Debugging)
* [Haskell - Performance](https://wiki.haskell.org/Performance)
* [Haskell - Concurrency](https://wiki.haskell.org/Concurrency)
* [Real World Haskell - Concurrent and Multicore Programming](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html)
* [GHC - Concurrent and Parallel Haskell](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/lang-parallel.html)
