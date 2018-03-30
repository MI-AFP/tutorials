# Basic IO, tests, and documentation

So far, we were working with pure functions (without side effects). You should be able to build complex libraries, use standard containers and other data types, write clean Haskell code, and understand most of basic programs written by Haskellers. This time we will take a look at basics of working with user (or other) input/output, writing tests and documentation.

## Basic IO

When you need to incorporate input and output (CLI, files, sockets, etc.), you bring impureness into your program. Obviously, IO brings side effects (it interacts with environment and changes the global state). It can be a bit complicated and so we won't go deep into theory this time and instead we will just show how to use it. Theoretical part will be covered in the future.

### The main and gets + puts

If you know C/C++, Python, or other programming languages, you should be familiar with "main". As in other languages, `main` is defined to be the entry point of a Haskell program. For Stack projects, it is located in file inside `app` directory and can be defined in `package.yaml` in `executables` section (it is possible to have multiple entrypoints per program). The type of `main` is `IO ()` - can do something (some actions) with `IO` but actually nothing is contained `()` for further work. You might wonder why it is not `IO Int` (with return code). It is because giving a return code is also IO action and you can do it from main with functions from `System.Exit`.

Now, let's take a look at basic IO examples:

```haskell
main1 :: IO ()
main1 = putStr "Hello, Haskeller!"     -- putStr :: String -> IO ()

main2 :: IO ()
main2 = putStrLn "Hello, Haskeller!"   -- putStrLn :: String -> IO ()

main3 :: IO ()
main3 = do
          putStr "Haskell "
          putChar 'F'                   -- putChar :: Char -> IO ()
          putChar 'T'
          putChar 'W'
          putStrLn "! Don't you think?!"

-- pure function
sayHello :: String -> String
sayHello name = "Hello, " ++ name ++ "!"

main4 :: IO ()
main4 = do
          putStrLn "Enter your name:"
          name <- getLine                -- getLine :: IO String, see getChar & getContents
          putStrLn . sayHello $ name

-- custom IO action
promptInt :: IO Int
promptInt = do
              putStr "Enter single integer: "
              inpt <- getLine       -- unwraps from IO (inpt :: String)
              return (read inpt)    -- return wraps with IO, read :: String -> Int

compute x y = 50 * x + y

main5 :: IO ()
main5 = do
          intA <- promptInt
          intB <- promptInt
          putStrLn ("Result: ++ show . compute $ intA intB)

main6 :: IO ()
main6 = print 1254                  -- print = putStrLn . show
```

### What does `do` do?

It doesn't look so weird if you recall how imperative programming works... But we are in functional world now, so what is going on? Haskell provides [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation), which is just syntactic sugar for chaining the actions and bindings (not just IO, in general!) in simple manner instead of using `>>` (*then*) and `>>=` (*bind*) operators of typeclass `Monad`. 

When you use binding operator `<-`, it means that result of binded action can be used in following actions. In the example with `main4`, IO action `getLine` is of type `IO String` and you want to use the wrapped `String` - you *bind* the result to name `name` and then use it in combination with pure function `sayHello` for next action which will do the output. The `do` block consists of actions and bindings and binding cannot be the last one!

You might have noticed the `return` in custom `promptInt` action. It is not a keyword but just a function of typeclass `Monad` which is used for wrapping back something, in this case `return :: String -> IO String`. We will look at all the `Monad` properties in detail next time.

### Be `interact`ive

Very interesting action for building a simple CLI is `interact :: (String -> String) -> IO ()`. The interact function takes a function of type `String -> String` as its argument. The **entire** input from the standard input device is passed to this function as its argument, and the resulting string is output on the standard output device.

```haskell
import Data.Char

main1 :: IO ()
main1 = interact (map toUpper)

main2 :: IO ()
main2 = interact (show . length)

main3 :: IO ()
main3 = interact reverse
```

As is emphasized, it work with entire input. If you've tried the examples above, you could observe a difference made by lazy evaluation in the first case. If you need to interact by lines or by works, you can create helper functions for that easily.

```haskell

eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . f . lines

eachWord :: (String -> String) -> (String -> String)
eachWord f = unwords . f . words

main5 :: IO ()
main5 = interact (eachLine reverse)

main6 :: IO ()
main6 = interact (eachWord reverse)

chatBot "Hello" = "Hi, how are you?"
chatBot "Fine" = "Lucky you... bye!"
chatBot "Bad" = "Me too!"
chatBot _ = "Sorry, I'm too dumb to understand this..."

main7 :: IO ()
main7 = interact (eachLine chatBot)
```

### IO with files

Working with files is very similar to working with console IO. As you might already know, most of IO for console is build by using IO for files with system "file" stdin and stdout. Such thing is called a `Handle` in Haskell and it is well described in [System.IO](http://hackage.haskell.org/package/base/docs/System-IO.html#t:Handle).

```haskell
main1 :: IO ()
main1 = withFile "test.txt" ReadMode $ \handle -> do
           fileSize <- hFileSize handle
           print fileSize
           xs <- getlines handle
           sequence_ $ map (putStrLn . reverse) xs

main2 :: IO ()
main2 = do
          handle <- openFile  "test.txt" ReadMode    -- :: IO Handle
          fileSize <- hFileSize handle
          print fileSize
          hClose handle
```

In similar manner you can work with binary files (you would use `ByteString`s) and temporary files. To work with sockets (network communication), you can use some library like [network](hackage.haskell.org/package/network/) or specifically for HTTP [wreq](https://hackage.haskell.org/package/wreq) and [req](https://hackage.haskell.org/package/req). 

For some well-known file formats are of course prepared libraries so you don't have to work with them over and over again just with functions from `Prelude`:

* JSON = [aeson](https://hackage.haskell.org/package/aeson)
* YAML = [yaml](https://hackage.haskell.org/package/yaml)
* CSV = [cassava](https://hackage.haskell.org/package/cassava) or [csv](https://hackage.haskell.org/package/csv/docs/Text-CSV.html)
* INI = [ini](https://hackage.haskell.org/package/ini)

... and so on. Also you probably know fabulous [pandoc](https://hackage.haskell.org/package/pandoc) writen in Haskell - you can use its API anytime.

### Arguments and env variables

Last basic way how to interact with program is via arguments and environment variables. Again, there is a little bit clumsy but simple way from [System.Environment](https://hackage.haskell.org/package/base/docs/System-Environment.html) and then some libraries that can help you with more complex cases...

```haskell
main :: IO ()
main = do
         progName <- getProgName    -- IO String
         print progName
         path <- getExecutablePath  -- IO String
         print path
         args <- getArgs            -- :: IO [String]
         print args
         user <- lookupEnv "USER"   -- :: IO (Maybe String), vs. getEnv :: IO String
         print user
         env <- getEnvironment      -- :: IO [(String, String)]
         print env
```

The most used library for [command line option parser](https://wiki.haskell.org/Command_line_option_parsers) is [cmdargs](http://hackage.haskell.org/package/cmdargs):

```
{-# LANGUAGE DeriveDataTypeable #-}
module Sample where
import System.Console.CmdArgs

data Sample = Hello {whom :: String}
            | Goodbye
              deriving (Show, Data, Typeable)

hello = Hello{whom = def}
goodbye = Goodbye

main = do
         args <- cmdArgs (modes [hello, goodbye])
         print args
```

For more complex example, visit their documentation - for example, hlint or diffy use this one.

## Testing

Testing is very important for keeping code on the straight-and-narrow path. The main testing mechanisms in Haskell are traditional unit testing and its more powerful descendant: type-based “property” testing.

### HUnit

[HUnit](https://hackage.haskell.org/package/HUnit) is a unit testing framework for Haskell, inspired by the JUnit tool for Java. For people familiar with unit testing this framework is very simple to use. First you define several test cases which you put in test list (instead of test class as in Java). Single test case is composed optionally of some data preparation and asserts. Result of running tests are four numbers - cases, tried, errors and failures.

```haskell

```

### QuickCheck

Different approach of testing is provided by [QuickCheck](https://hackage.haskell.org/package/QuickCheck). It is a library for random testing of program properties. You can specify some "laws" in your application and this library will check with given number of randomly (but smartly) generated instances if there is not some counterexample violating the laws. Such laws or specifications are expressed in Haskell, using combinators defined in the QuickCheck library. QuickCheck provides combinators to define properties, observe the distribution of test data, and define test data generators. All from simple example to complex tutorials of such definitions is explained in the [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).

With QuickCheck you can for example check if you instance of
`Monoid` is compliant with its laws.

```haskell

```

#### Basic properties

#### Generating values 

#### Own datatypes and `Arbitrary`

### Hspec

[Hspec](https://hackage.haskell.org/package/hspec) is a testing framework for Haskell. It is inspired by the Ruby library RSpec. Some of Hspec's distinctive features are:

* a friendly DSL for defining tests,
* integration with QuickCheck, SmallCheck, and HUnit,
* parallel test execution,
* automatic discovery of test files.

Tests written in Hspec are very readable, intuitive and powerful. It allows integration with HUnit as well as with QuickCheck so it is sort of ultimate testing framework in Haskell.

```haskell

```

#### Expectations

#### Property check

#### Complex test suites

### MuCheck

## Haddock (documentation)

Haskell projects, as any other project, should have good documentation of source code. In Haskell is the tool for documentation called [Haddock](https://www.haskell.org/haddock/).

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

-- |The 'square' function squares an integer.
-- It takes one argument, of type 'Int'.
square :: Int -> Int
square x = x * x

class C a where
   -- | This is the documentation for the 'f' method
   f :: a -> Int
   -- | This is the documentation for the 'g' method
   g :: Int -> a

data R a b =
  C { -- | This is the documentation for the 'a' field
      a :: a,
      -- | This is the documentation for the 'b' field
      b :: b
    }

data R a b =
  C { a :: a  -- ^ This is the documentation for the 'a' field
    , b :: b  -- ^ This is the documentation for the 'b' field
    }
```

For more information about using Haddock and writing the documentation of source code in Haskell check http://haskell-haddock.readthedocs.io/en/latest/index.html (examples above are from this documentation).

## Publish project

If you think that other people might be interested in your project and want to use it standalone or as part of their project (as dependency), you can publish your project on GitHub and also on Hackage! Stack will help you to make nice projects and then just follow:

* [GitHub - create a repo](https://help.github.com/articles/create-a-repo/)
* [Hackage - upload](https://hackage.haskell.org/upload)

Your project should be:
* tested (write tests for your project so you can prove that it is working properly),
* documented (try to describe everything in your code to "strangers" with low Haskell knowledge),
* licensed (pick suitable license - https://choosealicense.com can help you).

Another advantage of publishing is that your project can get attention and community can help you improve it - they will create issues, forks and pull requests!

## Using CI (Travis CI)

When you are developing the project and sharing it with community, you want to show that it is working well and also check if contributions to your code are not breaking the functionality. For that you can use CI tools (continuous integration) which allows you to run tests (or other scripts) automatically. There are many CI tools these days: Travis CI, Circle CI, Appveyor, Semaphore, GitLab CI, etc.

All (almost) CIs need some specification what they should do with your project. If you are using GitHub, then Travis CI is one of good choices for you. Just create `.travis.yml` in your repository and register project in Travis CI.

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

For Haskell you can use `.travis.yml` above or read documentation.

## Task assignment

* Write tests and documentation for given project.
* Publish on GitHub with use of Travis CI for testing.

## Further reading

* [Real World Haskell - Testing and quality assurance](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)
* [WikiBooks - Haskell: Testing](https://en.wikibooks.org/wiki/Haskell/Testing)
* [Haddock User Guide](https://www.haskell.org/haddock/doc/html/index.html)
