# Basic IO, tests, and documentation

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

## Publish your project

If you think that other people might be interested in your project and want to use it standalone or as part of their project (as a dependency), you can publish your project on GitHub and also on Hackage:

* [GitHub - create a repo](https://help.github.com/articles/create-a-repo/)
* [Hackage - upload](https://hackage.haskell.org/upload)

Your project should be:
* tested (write tests for your project so you can prove that it is working properly),
* documented (try to describe everything in your code to "strangers" with low Haskell knowledge),
* licensed (pick a suitable license - https://choosealicense.com can help you).

Another advantage of publishing is that your project can get attention and community can help you improve it -- they create issues, forks and pull requests.

## Using CI (Travis CI)

When you are developing a project and sharing it with a community, you want to show that it is working well and you also want to check if contributions to your code are not breaking it. For that, you can use CI tools (continuous integration) which allows you to run tests (or other scripts) automatically. There are many CI tools these days: Travis CI, Jenkins, Circle CI, Appveyor, Semaphore, GitLab CI, etc.

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

## Task assignment

The homework to practice IO basics, testing, and writing project documentation is in repository [MI-AFP/hw06](https://github.com/MI-AFP/hw06).

## Further reading

* [A Gentle Introduction to Haskell - Input/Output](https://www.haskell.org/tutorial/io.html)
* [Haskell - Simple input and output](https://en.wikibooks.org/wiki/Haskell/Simple_input_and_output)
* [Real World Haskell - Testing and quality assurance](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)
* [WikiBooks - Haskell: Testing](https://en.wikibooks.org/wiki/Haskell/Testing)
* [Haddock User Guide](https://www.haskell.org/haddock/doc/html/index.html)
* [QuickCheck and Magic of Testing](https://www.fpcomplete.com/blog/2017/01/quickcheck)
