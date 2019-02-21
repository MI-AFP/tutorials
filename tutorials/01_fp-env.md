# FP and first Haskell app

## Functional programming concepts

Functional programming is a programming paradigm (i.e. a style of building the structure and elements of computer programs) that treats computation as the evaluation of mathematical functions and avoids changing-state and mutable data. FP app is made of expressions or declarations instead of statements. Functional programming has its origins in lambda calculus, a formal system developed in the 1930s.

### Lambda calculus

Lambda calculus is good to understand (at least basics) when you want to start with FP. You should get familiar mainly with the principle of reduction of some more or less complex expression to the simplest and irreducible form. That will make it a lot easier to understand basics of functional programming and Haskell if you are used to the object-oriented world.

* [Lambda calculus (wikipedia)](https://en.wikipedia.org/wiki/Lambda_calculus)
* [Lambda calculus (Stanford)](https://plato.stanford.edu/entries/lambda-calculus/)
* [The Lambda Calculus for Absolute Dummies (like myself)](http://palmstroem.blogspot.cz/2012/05/lambda-calculus-for-absolute-dummies.html)

For FIT CTU students, there are subjects [BI-PPA](https://courses.fit.cvut.cz/BI-PPA/) and [MI-PSL](https://courses.fit.cvut.cz/MI-PSL/) which also cover basics of lambda calculus and functional programming.

### Function as first class-object

A programming language is said to have [first-class functions](https://en.wikipedia.org/wiki/First-class_function) if it treats functions as first-class citizens or objects. It means that the language support following concepts:

* passing function as argument,
* returning function as result,
* anonymous and nested functions,
* closures (and non-local variables),
* assigning functions to variables,
* and equality of functions.

Some might not be absolutely clear to you at the moment, some are familiar from some math courses, but we will see all of them in practice during the course.

### FP concepts and dictionary

* **Pure function** = function without side effect, always return the same result for the same input
* **Side effect** = modification of non-local state (global variable, input/output, raising exception, call function causing side effect, etc.)
* **Immutable variables** = after setting (binding) name to expression, you can not modify it
* **Referential transparency** = expression is said to be referentially transparent if it can be replaced with its corresponding value without changing the program's behavior
* **Recursion** = recursion occurs when a thing is defined in terms of itself or of its type (applies for functions, for example, factorial, and for types, like tree structure)

Principles [[jdegoes](https://twitter.com/jdegoes/status/974045822424776704?s=09)]:

1. Orthogonal Composability: composable blocks should address a single concern.
2. Maximum Polymorphism: data types & functions should require minimum structure necessary.
3. Maximum Deferment: defer types, decisions, effects, evaluation to the last moment.

## Haskell - the programming language

[Haskell] is a pure functional programming language with strong static typing and non-strict evaluation. It is also standardized (actual standard is [Haskell 2010] and 2020 is under development). Although it is language with academic and strong math background, it is being used in [research][haskell_research], [education][haskell_education] as well as in [industry][haskell_industry] for various projects. It was created as one common language based on many previous functional languages during the 1990s. Main language implementation is [Glasgow Haskell Compiler (GHC)][GHC], which we will use extensively in this course.

[haskell_research]: https://wiki.haskell.org/Haskell_in_research
[haskell_education]: https://wiki.haskell.org/Haskell_in_education
[haskell_industry]: https://wiki.haskell.org/Haskell_in_industry

## The tools we use

* [GHC] = the compiler. There are some more (Hugs, NHC, JHC, UHC, etc.), but this is classified as most widely used.
* [Cabal] = system for building and packaging.
* [Stack] = managing Haskell projects, works with [Cabal] for you.

:point_right: Please, install these (or check if installed already) - you can follow instruction on official websites one by one or (better) install [Haskell Platform], which includes all of those and also most common packages.

### Editors and IDEs

There are several editors you may use for writing Haskell programs, most probably there is some extension for your favorite editor. We recommend one of those:

* [Vim with plugins](https://wiki.haskell.org/Vim)
* [IntelliJ IDEA with HaskForce](http://haskforce.com) (or visit their [GitHub repo](https://github.com/carymrobbins/intellij-haskforce))
* [Atom with plugins](https://atom-haskell.github.io/overview/)

Most probably you will need following stuff:

* [ghc-mod] = connector to [GHC] API for various stuff
* [hlint] = source code suggestion
* [hindent] = indenter, pretty print
* [stylish-haskell] = code prettifier ("good style")

Install those with [Cabal] (by default it will install just for you to your profile, ensure that you have `~/.cabal/bin` in your `PATH`) or with [Stack]. The installation might take a while - it has a lot of dependencies and needs to build them from Haskell source code. If you want to install something with [Cabal] to all users, use `--global` flag.

```console
$ cabal update
$ cabal install hlint stylish-haskell hindent ghc-mod
$ stack install hlint stylish-haskell hindent ghc-mod
```

### Sites for searing

* [Hoogle] = "Google" for Haskell world
* [Hackage] = package archive, there are packages which can you install and use standalone or as modules for your projects (similar to PyPI for Python, RubyGems for Ruby, etc.)
* [Stackage] = package archive, alternative to [Hackage], only stable packages

:point_right: Take a look at them...

### Haskell, JavaScript and new languages

If you like to build (frontend/backend) JavaScript applications you can do that nicely with Haskell or similar language. There are multiple options, most known are:

* [GHCJS]
* [Haste]
* [PureScript]
* [Elm] (will be covered in later lectures)

This is a nice example of practical usage of Haskell for web projects! It is so much easier (and safer) to write JavaScript in Haskell than just plain JavaScript. Some of those are not just Haskell dialects or libraries but new languages deeply inspired by Haskell. For more information, read about [The JavaScript Problem](https://wiki.haskell.org/The_JavaScript_Problem). We will slightly look at this at the end of this course.

## Try to be interactive

Now you should have [GHC] installed from package or via Stack (and others as well, but we won't need an editor for this time), you can test it out with the following command.

```console
% ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.0.2
% stack exec -- ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.0.2
```

First, let's try the interactive environment and evaluate some basic expression in Haskell for the first time.

```
% ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> "Hello, world!"
"Hello, world!"
Prelude> putStrLn "Hello, world!"
Hello, world!
```

In prompt you see (by default) imported modules, in this case just `Prelude` - the most basic one (try to lookup Prelude module in [Hoogle] and check the content of it). Then you can write and evaluate Haskell expression. In the example above we wrote a string using double quotes and result was the string (no surprise, nothing to do more with that). We then used a function `putStrLn` to print a string to output (notice that there are no quotes around).

### Basic math & logics

In Haskell you can use math operators as you are used to.

```
Prelude> 5 + 5
10
Prelude> 5 + 5 * 3
20
Prelude> (5 + 5) * 3
30
Prelude> 2 ^ 8
256
Prelude> 2 / 3
0.6666666666666666
```

Integer division and modulo are done by functions. You can call functions in prefix notation (no brackets and no commas):

```
Prelude> div 7 2
3
Prelude> mod 7 2
1
```

Same goes for logic and comparison (you might be used to `!=` or `<>` for not-equal, but `!` and `<>` are used for something else in Haskell we will find out during the course):

```
Prelude> 5 > 7
False
Prelude> 5 == 7
False
Prelude> 5 /= 7
True
Prelude> not (5 /= 7)
False
Prelude> False || True
True
Prelude> False && True
False
Prelude> not False && True
True
```

### Types

A very useful thing in GHCi is that you can check the type of an expression.

```
Prelude> :type 2 ^ 8
2 ^ 8 :: Num a => a
Prelude> :type 2 / 3
2 / 3 :: Fractional a => a
```

The double semicolon `::` means "is of type" and you can use it for explicitly stating the type of your expressions. But this is not typecasting as you might know, you must conform the restriction, in this case, `Fractional a` (typeclasses will be covered deeply in next lessons).

```
Prelude> (2 / 3) :: Double
0.6666666666666666
Prelude> (2 / 3) :: Float
0.6666667
Prelude> (2 / 3) :: Int

<interactive>:2:2: error:
    • No instance for (Fractional Int) arising from a use of ‘/’
    • In the expression: (2 / 3) :: Int
      In an equation for ‘it’: it = (2 / 3) :: Int
```

You can see that error message exactly tells us what is wrong! `Int` is not an instance of `Fractional` which we need when using `/` in the expression `(2 / 3) :: Int`. This is kind of obvious but when you will have a bigger project with source files, you will find GHC error messages very useful.

If you need to change type, find a suitable function. Some of them are in `Prelude`: `toInteger`, `fromInteger`, `toRational`, etc. Another quite important is `show` for showing anything as `String`. How these work will be covered later on in more detail as we get to typeclasses and polymorphism!

```
Prelude> :t toInteger (7::Int)
toInteger (7::Int) :: Integer
Prelude> show (2/3)
"0.6666666666666666"
Prelude> toRational (2/16)
1 % 8
```

Similarly you can do such thing with functions, because we are in functional language! Function `abs` (absolute value) takes a number and returns a number. That means it doesn't work for strings...

The type signature is very math-like... Instance (type) of `Num` is for example `Integer` and you know functions from math which have type `Integer -> Integer` (domain and co-domain).

```
Prelude> :type abs
abs :: Num a => a -> a
Prelude> abs (-5)
5
Prelude> abs (-10.65)
10.65
Prelude> abs "z"

<interactive>:26:1: error:
    • No instance for (Num [Char]) arising from a use of ‘abs’
    • In the expression: abs "z"
      In an equation for ‘it’: it = abs "z"
```

The operators are functions as well - Haskell is functional language. All you need to do is put it in brackets. Plus takes two numbers and returns a number. You can then use `(+)` as a function in prefix notation and not infix.

```
Prelude> :type (+)
(+) :: Num a => a -> a -> a
Prelude> (+) 5 4
9
```

On the other hand you might want to use some functions in infix to improve readability and you need `` ` `` for that.

```
Prelude> :t div
div :: Integral a => a -> a -> a
Prelude> 5 `div` 3
1
Prelude> 5 `mod` 3
2
```

### Giving a name to an expression

In GHCi you can name an expression with `let` and assignment.

```
Prelude> let x = 5
Prelude> :type x
x :: Num t => t
Prelude> let x = 5 :: Integer
Prelude> :type x
x :: Integer
```

You can create functions as well. Notice that the type is automatically inferred. It happens every time when possible and you don't explicitly state the type.

```
Prelude> let myFunc x y = 2 * x + y
Prelude> :t myFunc
myFunc :: Num a => a -> a -> a
Prelude> myFunc 5 3
13
```

### Source file

OK, but if you close GHCi (CTRL+D/Z or `:quit`/`:q`) then you lost your code. For making it persistent you need a source file. You can name with as you like but in Haskell we use `.hs` file extension (or `.lhs` if code is part of document - [literate Haskell]).

Let's create a simple source file `01_test_haskell.hs` and create two functions there. We can also use comments (`{- -}` for multiline and `--` for single line comment).

```haskell
{-
   My first Haskell source file to be tested out with GHCi

   BTW This is multiline comment
-}

-- This is single line comment

-- First function: Linear function for Integers
linear :: Integer -> Integer -> Integer -> Integer
linear a b x = a * x + b

-- Second function: check if three lenghts form triangle
isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c = (a + b > c) && (a + c > b) && (b + c > a)
```

Now we can load it with `:load` to GHCi:

```
Prelude> :load FPCourse/files/01_test_haskell.hs
[1 of 1] Compiling Main             ( 01_test_haskell.hs, interpreted )
Ok, modules loaded: Main.
```

You can see that file is being compiled, imported as `Main` and then if everything is OK, you can use it in GHCi. Another useful command in GHCi is `:info` which can tell you where is definition located.

```
*Main> linear 5 3 7
38
*Main> 5 * 7 + 3
38
*Main> :type linear
linear :: Integer -> Integer -> Integer -> Integer
*Main> :info linear
linear :: Integer -> Integer -> Integer -> Integer
  	-- Defined at 01_test_haskell.hs:10:1
```

For browsing content of actual module (in our case `Main`) is great command `:browse` and you can specify which module you want to browse if not just the actual.

```
*Main> :browse
linear :: Integer -> Integer -> Integer -> Integer
isTriangle :: Double -> Double -> Double -> Bool
*Main> :browse Prelude
...
```

Now we can add another function to the file:

```haskell
conforms :: Integer -> Bool
conforms x = (x > 10 && x < 15) || x == 0
-- . is function composition (as in math)
notConforms = not . conforms
```

But you need to recompile the file in GHCi so the change can take effect. You can use `:load` again, but if you don't want to write the filename (and path), you can use `:reload`.

```
*Main> :reload
[1 of 1] Compiling Main             ( 01_test_haskell.hs, interpreted )
Ok, modules loaded: Main.
*Main> :browse
linear :: Integer -> Integer -> Integer -> Integer
isTriangle :: Double -> Double -> Double -> Bool
conforms :: Integer -> Bool
notConforms :: Integer -> Bool
```

## First project

We tried some basic work with the interactive environment which is nice and useful but how is it related to real-world application? As you know from other programming languages the code should be placed in source files and those should be compiled to executable (or to JavaScript as we mentioned before).

### Source file and compilation

Let's try a classic way with compilation via plain [GHC]. We can do the most basic program - the "Hello, world!". Create a file `01_hw.hs` just with function `main` as follows:

```haskell
main = putStrLn "Hello, world!"
```

Now use `ghc` compiler to compile the file:

```console
% ghc 01_hw.hs
[1 of 1] Compiling Main             ( 01_hw.hs, 01_hw.o )
Linking 01_hw ...
```

You can see some similar output as when you were loading a file in GHCi just here is not interpreting, but linking and `01_hw.o`. If you list your directory, there are now these files:

* `01_hw.hs` = source file
* `01_hw.hi` = interface file (compilation info)
* `01_hw.o` = object file (compiled file before linking)
* `01_hw` = executable (linked)

And you can run the executable:

```console
% ./01_hw
Hello, world!
```

Now, let's say we want to have logic stuff in a different source file (module). Create a file `01_hw/HWLib.hs` with:

```haskell
module HWLib where

greet :: String -> String
greet x = "Hello, " ++ x ++ "!"
```

And appropriate `01_hw/Main.hs`:

```haskell
import HWLib

main = do
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn (greet name)
```

Now how to make it work together? Do you know `Makefile` (for example, from `C/C++`)? Don't worry... GHC is a great tool and does such painful work for you (reads imports and looking up the files - you just need to have a standard naming of modules/files):

```console
% ghc --make Main.hs
[1 of 2] Compiling HWLib            ( HWLib.hs, HWLib.o )
[2 of 2] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
% ./Main
Enter your name:
Marek
Hello, Marek!
```

### Stack project instead

Compiling application made from multiple source codes is not so complicated in the end. But still for project management, having project structure nice and especially simple management of dependencies (it can be a real pain to get it working with some specific versions) we will use [Stack].

Let's do the *Hello, world!* app with [Stack]. First, verify that you have it installed.

```console
% stack --version
Version 1.9.3, Git revision 40cf7b37526b86d1676da82167ea8758a854953b (6211 commits) x86_64 hpack-0.31.1
```

Then you can create a new project with default template:

```console
% stack new HelloWorld
Downloading template "new-template" to create project "HelloWorld" in HelloWorld/ ...

The following parameters were needed by the template but not provided: author-name
You can provide them in /home/user/.stack/config.yaml, like this:
templates:
  params:
    author-name: value
Or you can pass each one as parameters like this:
stack new HelloWorld new-template -p "author-name:value"


The following parameters were needed by the template but not provided: author-email, author-name, category, copyright, github-username
You can provide them in /home/user/.stack/config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
Or you can pass each one as parameters like this:
stack new HelloWorld new-template -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"

Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- HelloWorld/

Selecting the best among 15 snapshots...

* Matches lts-13.8

Selected resolver: lts-13.8
Initialising configuration using resolver: lts-13.8
Total number of user packages considered: 1
Writing configuration to file: HelloWorld/stack.yaml
All done.
```

Now the whole project has been created for you. You should edit the `package.yaml` file which specifies the project (author, email, URL, etc.) and thanks to [hpack] is a nicer form which is then used to generate `HelloWorld.cabal`. Then use the same code from the previous example to `app/Main.hs` and `src/Lib.hs`.

```haskell
-- src/Lib.hs
module Lib
    ( greet
    ) where

greet :: String -> String
greet x = "Hello, " ++ x ++ "!"
```

```haskell
-- app/Main.hs
module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn (greet name)
```

Now you don't use GHC directly, but call it via `stack build`:

```console
% stack build
No compiler found, expected minor version match with ghc-8.0.2 (x86_64) (based on resolver setting in /home/user/.stack/global-project/stack.yaml).
To install the correct GHC into /home/user/.stack/programs/x86_64-linux/, try running "stack setup" or use the "--install-ghc" flag. To use your system GHC installation, run "stack config set system-ghc --global true", or use the "--system-ghc" flag.
```

As you see `stack` doesn't want to use system-wide installation of `ghc` but local instead by default. Just run `stack setup` so `stack` will prepare local `ghc` (it will take some time) and then try to build.

```console
% stack setup
Preparing to install GHC to an isolated location.
This will not interfere with any system-level installation.
Downloaded ghc-8.0.2.
Installed GHC.
stack will use a sandboxed GHC it installed
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec

% stack init
Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- HelloWorld/HelloWorld.cabal

Selecting the best among 11 snapshots...

* Matches lts-13.8

Selected resolver: lts-13.8
Initialising configuration using resolver: lts-9.11
Total number of user packages considered: 1
Writing configuration to file: stack.yaml
All done.

% stack build
Building all executables for `HelloWorld' once. After a successful build of all of them, only specified executables will be rebuilt.
HelloWorld-0.1.0.0: configure (lib + exe)
Configuring HelloWorld-0.1.0.0...
HelloWorld-0.1.0.0: build (lib + exe)
Preprocessing library for HelloWorld-0.1.0.0..
Building library for HelloWorld-0.1.0.0..
[1 of 2] Compiling Lib              ( src/Lib.hs, .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Lib.o )
[2 of 2] Compiling Paths_HelloWorld ( .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/autogen/Paths_HelloWorld.hs, .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paths_HelloWorld.o )
Preprocessing executable 'HelloWorld-exe' for HelloWorld-0.1.0.0..
Building executable 'HelloWorld-exe' for HelloWorld-0.1.0.0..
[1 of 2] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/HelloWorld-exe/HelloWorld-exe-tmp/Main.o )
[2 of 2] Compiling Paths_HelloWorld ( .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/HelloWorld-exe/autogen/Paths_HelloWorld.hs, .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/HelloWorld-exe/HelloWorld-exe-tmp/Paths_HelloWorld.o )
Linking .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/HelloWorld-exe/HelloWorld-exe ...
HelloWorld-0.1.0.0: copy/register
Installing library in /home/user/Projects/MI-AFP/tests/HelloWorld/.stack-work/install/x86_64-linux-tinfo6/lts-13.8/8.6.3/lib/x86_64-linux-ghc-8.6.3/HelloWorld-0.1.0.0-8b39YCi0nmn4QsoDKix2j8
Installing executable HelloWorld-exe in /home/user/Projects/MI-AFP/tests/HelloWorld/.stack-work/install/x86_64-linux-tinfo6/lts-13.8/8.6.3/bin
Registering library for HelloWorld-0.1.0.0..
stack build  6.16s user 0.94s system 96% cpu 7.329 total
```

Everything ended up OK and you are finally able to run the application (`HelloWorld-exe` is defined in `package.yaml`, thus also `HelloWorld.cabal`, and you may change it):

```console
% stack exec HelloWorld-exe
Enter your name:
Marek
Hello, Marek!
```

For debugging you can run `ghci` with project preloaded:

```console
% stack ghci
Using main module: 1. Package `HelloWorld' component exe:HelloWorld-exe with main-is file: /home/user/Projects/MI-AFP/tests/HelloWorld/app/Main.hs
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: HelloWorld
GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( /home/user/Projects/MI-AFP/tests/HelloWorld/src/Lib.hs, interpreted )
[2 of 2] Compiling Main             ( /home/user/Projects/MI-AFP/tests/HelloWorld/app/Main.hs, interpreted )
Ok, two modules loaded.
Loaded GHCi configuration from /tmp/haskell-stack-ghci/3b07e5cf/ghci-script
*Main Lib> 
 :browse
main :: IO ()
*Main Lib> :browse Lib
greet :: String -> String
```

[Stack] is a really powerful tool, you can find more by reading documentation or just with `stack --help` or read [docs](https://docs.haskellstack.org/en/stable/README/).

### Stack config files and dependencies

You might have noticed that [Stack] uses `package.yaml` to generate `.cabal` and there is some `stack.yaml`. It also somehow takes care of the needed dependencies. Let's say you need to your collection of type Set. Of course, you could implement it on your own, but reinventing the wheel is unnecessary! Use `Data.Set` which is already here (we will cover details about this and other data structures in Haskell in the future).

If you look up `Data.Set` ([Hoogle], [Stackage] or [Hackage]), you will find out that it is in package `containers` licensed under BSD with maintainer email libraries@haskell.org (see [here](http://hackage.haskell.org/package/containers-0.5.11.0/docs/Data-Set.html)). If you now try to do this in your `Lib.hs`:

```haskell
import Data.Set

namesSet = insert "Robert" (insert "Marek" empty)
```

After trying to build with `stack build` you should get this error stating that it could not find module `Data.Set`:

```console
/home/.../HelloWorld/src/Lib.hs:5:1: error:
    Could not find module ‘Data.Set’
    Perhaps you meant Data.Int (from base-4.10.1.0)
    Use -v to see a list of the files searched for.
  |
5 | import Data.Set
  | ^^^^^^^^^^^^^^^
```

All you need to do is include package `containers` (you could also specify the required version) in the dependencies of `package.yaml` file. After `stack build` the package will be downloaded and built if something else is needed (like adding `extra-deps` to `stack.yaml`, you will be informed in detail - it happens when it is not common package).

```yaml
# ...
dependencies:
- base >= 4.7 && < 5
- containers

# ...
```

Then after the build you can do for example this with `stack ghci`:

```
*Main Lib> namesSet
fromList ["Marek","Robert"]
*Main Lib> import Data.Set
*Main Lib Data.Set> member "Marek" namesSet
True
*Main Lib Data.Set> member "Martin" namesSet
False
```

Further, [Stack] also provides [dependency visualization](https://docs.haskellstack.org/en/stable/dependency_visualization/) via well-known tool Dot (GraphViz) and more detailed options...

## Task assignment

To test out the workflow check the dummy homework [MI-AFP/hw00](https://github.com/MI-AFP/hw00) where you will learn how you should get, complete, check, and submit homework (especially useful if you are not familiar with [GitHub] and [Travis CI]). By working on such homework, you might also learn new things which you encounter in tests and skeletons.

For your first assignment, visit [MI-AFP/hw01](https://github.com/MI-AFP/hw01). The task consists of writing simple expressions, looking up information with [Hoogle], [Stackage], [Hackage] and/or GHCi, and working with dependencies of project.

## Further reading

* [Why Functional Programming Matters](https://wiki.ccmi.fit.cvut.cz/_media/programming:why_functional_programming_matters.pdf)
* [Why Haskell Matters](https://dzone.com/articles/why-haskell-matters)
* [Learn You a Haskell for Great Good](http://learnyouahaskell.com)
* [School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell)
* [Stack User Guide](https://docs.haskellstack.org/en/stable/GUIDE/)
* [GHC User Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)

[Cabal]: https://www.haskell.org/cabal/
[Elm]: http://elm-lang.org
[GHC]: https://www.haskell.org/ghc/
[ghc-mod]: https://github.com/DanielG/ghc-mod
[GHCJS]: https://github.com/ghcjs/ghcjs
[GitHub]: https://github.com
[Hackage]: https://hackage.haskell.org
[Haskell]: https://www.haskell.org
[Haskell 2010]: https://www.haskell.org/onlinereport/haskell2010/
[Haskell Platform]: https://www.haskell.org/platform/
[Haste]: https://haste-lang.org
[hindent]: https://github.com/commercialhaskell/hindent
[hlint]: https://hackage.haskell.org/package/hlint
[Hoogle]: https://www.haskell.org/hoogle/
[hpack]: https://github.com/sol/hpack
[literate Haskell]: https://wiki.haskell.org/Literate_programming
[PureScript]: http://www.purescript.org
[Stack]: https://docs.haskellstack.org
[Stackage]: https://www.stackage.org
[stylish-haskell]: https://github.com/jaspervdj/stylish-haskell
[Travis CI]: https://travis-ci.org
