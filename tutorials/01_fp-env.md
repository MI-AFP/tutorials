# FP and first Haskell app

This first tutorial is focused on getting you familiar with functional programming concepts and Haskell programming language as well as the tools we will use during this course. By end of this tutorial, you should be able to create simple Haskell applications on your own and understand basic FP concepts.

## Intro to functional programming

Functional programming (FP) is a programming paradigm that is **strongly influenced by mathematics and formal logic**. At its core, it views computation as the evaluation of expressions rather than the execution of step-by-step instructions. Programs are described *declaratively*: instead of explaining how to compute something, we describe what the result should be.

Historically, functional programming grew out of **lambda calculus**, a formal system developed in the 1930s to study computation, functions, and substitution. Because of this origin, many FP concepts have precise mathematical definitions and are often presented in a theoretical way.

However, this course is about applied functional programming.

We will not focus on formal proofs, reduction rules, or theoretical machinery for its own sake. Instead:

* we will introduce theoretical ideas only when they help explain practical behavior,
* we will relate FP concepts to familiar mathematics (functions, equations, composition), and
* we will focus on how these ideas help you write clear, correct, and maintainable code.

You do not need a strong mathematical background to follow this course. When mathematical intuition is useful, we will build it gradually and concretely.

### Functional programming vs. imperative programming

In imperative and object-oriented programming, programs are usually described as a sequence of commands that change program state over time. In functional programming, programs are instead built from expressions that evaluate to values.

A rough intuition:

* imperative programming: “*do this, then do that*”
* functional programming: “*this value is defined as…*”

This difference has important consequences:

* less reliance on mutable state,
* easier reasoning about code,
* better composability of program parts.

We will see these benefits in practice very early.

### Mathematics as intuition, not as theory

Many FP ideas map directly to concepts you already know from mathematics:

* functions as mappings from inputs to outputs,
* expressions that can be replaced by their values,
* composition of functions, or
* defining things by equations.

In this course, mathematics serves as intuition and vocabulary, not as formal theory. When we say that a function is pure or that an expression is referentially transparent, we will always connect that idea to concrete code examples and practical consequences.

If you are interested in deeper theoretical foundations, the following resources provide excellent background reading:

* [Lambda calculus (Wikipedia)](https://en.wikipedia.org/wiki/Lambda_calculus)
* [Lambda calculus (Stanford Encyclopedia of Philosophy)](https://plato.stanford.edu/entries/lambda-calculus/)
* [The Lambda Calculus for Absolute Dummies](http://palmstroem.blogspot.cz/2012/05/lambda-calculus-for-absolute-dummies.html)

For FIT CTU students, related material is covered in:

* [BI-PPA (Programming Paradigms)](https://courses.fit.cvut.cz/BI-PPA/)
* [MI-PSL (Programming in Scala)](https://courses.fit.cvut.cz/MI-PSL/)

These are **optional** and not required for this course. You can always find additional resources in the "Further reading" section at the end of each tutorial.

Be aware that some FP programmers are very focused on formalism, theory, and mathematical rigor (often instead of practical application, clarity, and maintainability of the code). While these aspects are important in some contexts, this course emphasizes practical application and understanding over formal proofs.

### What “functional” will mean in this course

Throughout the course, functional programming will primarily mean working with the following ideas:

* **Pure functions** = functions without side effects, whose result depends only on their input
* **Immutability** = values do not change once defined
* **Composition** = building complex behavior by combining small functions
* **Explicit effects** = side effects are controlled and visible in types
* **Strong static typing** = using the type system to express and enforce program structure

Each of these concepts will be introduced in context, with examples in Haskell, and motivated by practical problems rather than abstract definitions.

## Haskell - the programming language

[Haskell] is a pure functional programming language with strong static typing and non-strict evaluation. It is also standardized (actual standard is [Haskell 2010] and Haskell 2020 is still not here). Although it is language with academic and strong math background, it is being used in [research][haskell_research], [education][haskell_education] as well as in [industry][haskell_industry] for various projects. It was created as one common language based on many previous functional languages during the 1990s. Main language implementation is [Glasgow Haskell Compiler (GHC)][GHC], which we will use extensively in this course.

[haskell_research]: https://wiki.haskell.org/Haskell_in_research
[haskell_education]: https://wiki.haskell.org/Haskell_in_education
[haskell_industry]: https://wiki.haskell.org/Haskell_in_industry

## Why Haskell?

There are many programming languages that support functional programming today, including multi-paradigm languages such as JavaScript, Python, Scala, Kotlin, or Rust, as well as primarily functional languages like OCaml, F#, or Clojure.

In this course, we use Haskell not because it is the only functional language, but because it is a particularly good language for learning functional programming principles clearly and systematically.

Haskell has several properties that make it well suited for this purpose:

* **Purity by default** = In Haskell, functions are pure unless stated otherwise. Side effects (such as input/output, mutation, or randomness) are explicit and controlled. This makes functional concepts visible rather than optional.
* **Strong static typing with type inference** = Haskell’s type system allows you to express rich program structure while still keeping code concise. Types serve as documentation, correctness checks, and design tools — not just as annotations.
* **Direct correspondence to mathematical intuition** = Haskell code often looks close to equations or definitions you might write on paper. This makes it easier to relate programs to mathematical ideas such as functions, composition, and abstraction.
* **Minimal hidden behavior** = Compared to multi-paradigm languages, Haskell avoids implicit mutation, implicit nulls, or ad-hoc control flow. This reduces accidental complexity and forces you to model problems explicitly.

Because of these characteristics, Haskell tends to teach you functional programming whether you want it or not. That is a feature, not a drawback, in a learning environment.

Once you understand functional programming in Haskell, **transferring these ideas to other languages becomes significantly easier**.

## The tools we use

* [GHC] = the compiler and interactive environment. There are some more (Hugs, NHC, JHC, UHC, etc.), but this is classified as most widely used. It is de-facto standard for Haskell programming.
* [Cabal] = system for building and packaging. This is the standard tool for building Haskell projects and managing dependencies.
* [Stack] = a project and dependency management tool. It uses Cabal under the hood but makes project management much easier.
* [HLS (Haskell Language Server)] = language server for Haskell, provides IDE features like autocompletion, type info, diagnostics, etc.
* [GHCup] = tool for installing GHC, Cabal, Stack, HLS, etc. and managing their versions. It simplifies the setup process and ensures you have compatible versions of the tools.

:point_right: You should install these tools before proceeding. The recommended way is to use [GHCup] which will handle installation and version management for you (no matter if you are on Linux, macOS, or Windows). Follow the instructions and install the recommended versions of GHC, Cabal, Stack, and HLS.

### Editors and IDEs

Haskell does not require a specific editor or IDE. You can use any editor you are comfortable with, as long as it supports syntax highlighting and language server integration.

Common choices include:

* **Vim** with [Haskell plugins](https://wiki.haskell.org/Vim) for command-line enthusiasts
* **Visual Studio Code** with [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) for a modern and user-friendly experience
* **IntelliJ IDEA** with [Haskell LSP](https://plugins.jetbrains.com/plugin/24123-haskell-lsp) for those who prefer JetBrains IDEs
* **Emacs** with [Haskell mods](https://wiki.haskell.org/Emacs) for Emacs lovers

The exact editor choice is not important for the course. Pick one you like and set it up to work with HLS for the best experience. If you are not sure, Visual Studio Code is a good starting point.

### Sites for searching and documentation

When working with Haskell, you will frequently look up functions, types, and libraries. The following sites are essential:

* [Hoogle] is a search engine for Haskell functions and types. You can search by name or by type, which is extremely useful once you get used to it.
* [Hackage] is the central package repository (similar to *PyPI* for Python or *npm* for JavaScript). It contains thousands of libraries and tools for Haskell including their documentation and browseable source code.
* [Stackage] is a curated set of stable packages. Stackage provides tested collections of library versions that are known to work well together. Tools like *Stack* use Stackage snapshots to ensure **reproducible builds**.

You will use these resources regularly throughout the course, so get familiar with them (maybe bookmark them). Of course, getting help via search engines like Google or Stack Overflow is also common practice. Nowadays, AI-based tools like ChatGPT or GitHub Copilot can also assist you in finding information or generating code snippets (but always verify the output and be aware of possible hallucinations).

:point_right: Take a time and investigate them a bit so you are ready later on, whenever you need to find some function or library.

### Haskell and related languages

Haskell has influenced many modern languages and ecosystems, including frontend and backend development for the web.

There are several languages and tools that are either based on Haskell or heavily inspired by it, such as:

* **PureScript** (for frontend web development)
* **Elm** (for frontend web development, covered later)
* **Idris** (dependently typed functional programming)
* (historically also tools like GHCJS or Haste)

Moreover, Haskell inspired other well-known languages even if they are not purely functional, such as **Scala**, **F#**, **Rust**, **Python**, **Swift** or **Java**.

## Try Haskell interactively with GHCi

GHCi is an interactive environment (so called *REPL* = Read-Eval-Print Loop) for Haskell. It lets you evaluate expressions, inspect their types, debug, and experiment quickly before you write full source files.

### Starting GHCi

```
% ghci
GHCi, version 9.x.x: https://www.haskell.org/ghc/  :? for help
ghci>
```

At the prompt you can write Haskell **expressions**, the REPL will read them, evaluate them, and print the result. Alternatively, you can use GHCi commands which start with `:` (colon), such as `:?` for help (those are not Haskell expressions!).

### Expressions

In Haskell, most things you write are expressions. **An expression evaluates to a value**. For example, a number like `5` is an expression that evaluates to the integer value `5`. A string like `"Hello"` is an expression that evaluates to the string value `Hello`.

```
ghci> 5
5

ghci> "Hello"
"Hello"
```

That’s the simplest possible interaction: evaluate an expression and show its result.

Now compare that to a **function** that *performs output*:

```
ghci> putStrLn "Hello"
Hello
```

Here `putStrLn "Hello"` is still an **expression** (more specially function application with `"Hello"` as argument) — but its result is an **effectful computation**, not a pure string value (we’ll later talk about IO and effects explicitly).

Note: `putStrLn` is a function with one **parameter** of type `String`. It simply prints that string to the standard output followed by a newline. It comes from the **standard module** called `Prelude` which is imported by default in GHCi and all Haskell source files.

### Every expression has a type

A key point now: **every expression has a type**, and GHC can *infer* it.

In GHCi you can ask for the type of any expression using `:type` (or `:t`) followed by the expression.

```
ghci> :type "Hello"
"Hello" :: String
```

We can read this as: the expression `"Hello"` is of type `String` (`::` means "*is of type*"). If you have older version of GHCi, you might see `[Char]` instead of `String` - they are the same thing (type synonym).

This is the first math connection worth making explicit:
* In mathematics, we say that an object has a certain type or belongs to a certain set, e.g., `5 ∈ ℤ` (alt. `5 : ℤ`) means that the number `5` is an integer.
* In Haskell, we say that an expression has a certain type, e.g., `5::Int` means that the expression `5` is of type `Int`.

Types are not decoration — they are part of the meaning of the program. The type system helps ensure correctness, guides program structure, and enables powerful abstractions. **Type inference** means that you often do not need to write types explicitly; GHC can figure them out for you. It does so by analyzing how expressions are constructed and combined. It always tries to find the most general type that fits the expression.

```
ghci> :type 5
5 :: Num p => p
```

Here `Num p => p` means that `5` can be any type `p` that is an instance of the `Num` typeclass (we will cover typeclasses later). Basically, it means that `5` can be used as *any numeric type* (like `Int`, `Integer`, `Float`, `Double`, etc.).

### Boolean expressions

Comparison produces `Bool` values (`True` or `False`), and boolean operators compose like normal expressions:

```
ghci> 5 > 7
False

ghci> 5 == 7
False

ghci> 5 /= 7
True

ghci> not (5 /= 7)
False

ghci> False || True
True

ghci> False && True
False

ghci> not False && True
True
```

### Basic arithmetic and precedence

Haskell’s arithmetic works as you expect:

```
ghci> 5 + 5
10

ghci> 5 + 5 * 3
20

ghci> (5 + 5) * 3
30

ghci> 2 ^ 8
256
```

Just, remember that `/` is for fractional division (result is fractional number):

```
ghci> 7 / 2
3.5
```

Some operations are functions, not operators.

```
ghci> div 7 2
3

ghci> mod 7 2
1
```

Here notice that `div` and `mod` are functions for integer division and modulus, respectively. **Functions are applied** by writing the function name followed by its arguments separated by spaces. There are no parentheses or commas.

```
ghci> :t div
div :: Integral a => a -> a -> a

ghci> :t mod
mod :: Integral a => a -> a -> a

ghci> :t abs
abs :: Num a => a -> a
```

You can see that both `div` and `mod` take two arguments of some integral type resulting in a value of the same type. However, `abs` takes one argument of some numeric type and returns a value of the same type. Again, in math that could be written as (see the similarities?):

* `div : ℤ → ℤ → ℤ`
* `mod : ℤ → ℤ → ℤ`
* `abs : ℝ → ℝ`

Another note on **type signatures**: `->` means "function from ... to ...". So `a -> a -> a` means "function that takes an argument of type `a` and returns a function that takes another argument of type `a` and returns a value of type `a`". In other words, it is a function with two arguments of type `a` returning a value of type `a`. Type signatures are always read from left to right, you can write brackets to make it clearer: `a -> (a -> a)`.

### Type inference and type constraints

Ask for types:

```
ghci> :type 2 ^ 8
2 ^ 8 :: Num a => a

ghci> :type 2 / 3
2 / 3 :: Fractional a => a
```

This is Haskell saying:

* This expression works for **any type** `a` that behaves like numbers (`Num a`).
* For `/`, it needs a stronger requirement though: `Fractional a` (result of `/` is a value of **any fractional type**).

You can also explicitly choose a concrete type by using a **type annotation** with `::`:

```
ghci> (2 / 3) :: Double
0.6666666666666666

ghci> (2 / 3) :: Float
0.6666667
```

But you cannot force the impossible:

```
ghci> (2 / 3) :: Int

<interactive>:2:2: error:
    • No instance for (Fractional Int) arising from a use of ‘/’
    • In the expression: (2 / 3) :: Int
      In an equation for ‘it’: it = (2 / 3) :: Int
```

This is a practical payoff of types: the compiler tells you what mathematical *contract* you violated.

You can see that error message exactly tells us what is wrong! `Int` is not an instance of `Fractional` which we need when using `/` in the expression `(2 / 3) :: Int`. This is kind of obvious but when you will have a bigger project with source files, you will find GHC error messages very useful.

If you need to change type, find a suitable function. Some of them are in `Prelude`: `toInteger`, `fromInteger`, `toRational`, etc. Another quite important is `show` for showing anything as `String`. How these work will be covered later on in more detail as we get to type classes and polymorphism!

```
ghci> :t toInteger (7::Int)
toInteger (7::Int) :: Integer
ghci> show (2/3)
"0.6666666666666666"
ghci> toRational (2/16)
1 % 8
```

Similarly you can do such thing with functions, because we are in functional language! Function `abs` (absolute value) takes a number and returns a number. That means it doesn't work for strings:

```
ghci> abs "z"

<interactive>:26:1: error:
    • No instance for (Num [Char]) arising from a use of ‘abs’
    • In the expression: abs "z"
      In an equation for ‘it’: it = abs "z"
```

### Operators are functions

In Haskell, operators are functions written infix. You can use them like normal functions by putting them in parentheses:

```
ghci> :type (+)
(+) :: Num a => a -> a -> a

ghci> 5 + 4
9

ghci> (+) 5 4
9
```

And you can use a normal function in infix form using backticks:

```
ghci> 5 `div` 3
1

ghci> 5 `mod` 3
2
```

This is often useful for readability (especially for functions that naturally read better in infix form, like `div`, `mod`, or user-defined functions).

### Naming expressions (let bindings)

In GHCi you can name an expression with `let` and assignment (for a long time now, GHCi supports naming without `let` as well, so you can just write `x = 5`).

```
ghci> let x = 5

ghci> :type x
x :: Num t => t

ghci> x = 5

ghci> :type x
x :: Num t => t
```

The word *Let* is again bringing us closer to mathematics where we often say "*Let x be ...*" when defining something.

If you want, you can add a concrete type using type annotation with `::`:

```
ghci> x = 5 :: Integer

ghci> :type x
x :: Integer
```

In Haskell, `x` is not a variable in the imperative sense. It is best described as:

* a name bound to an expression, or
* a binding.

After this definition, the name `x` refers to the value of the expression `5` (of a concrete or inferred type).

Important properties of such bindings:

* `x` cannot change = Once the name `x` is bound to `5`, it always refers to `5`. (In GHCi, you can give the name later to something else, but in source files, you cannot.)
* `x` is not a memory location like in imperative languages = There is no concept of variable assignment or mutation (updating the value of `x`).
* `x` is closer to a mathematical symbol than to a variable in C, Java, or Python.

### Defining functions

**Function definitions** work the same way:

```
ghci> myFunc x y = 2 * x + y

ghci> :t myFunc
myFunc :: Num a => a -> a -> a

ghci> myFunc 5 3
13
```

A note here, **function definition** here is `myFunc x y = 2 * x + y` which means that `myFunc` is a **function with two parameters** `x` and `y` (local bindings, again no mutable variables!) and its result is `2 * x + y`. You can see that **function application** is just writing the function name followed by its arguments separated by spaces. A possible **function declaration** (type signature) for `myFunc` would be `myFunc :: Integer -> Integer -> Integer` (we didn't write it explicitly here, but GHC inferred it for us to more general form `Num a => a -> a -> a`).

## From GHCi to source files

Working in GHCi is useful for exploration, but once you close it, everything you defined is lost.
To write real programs, we put definitions into source files.

A Haskell source file:

* contains **named definitions** (bindings),
* is saved with the `.hs` extension,
* describes *what values and functions are*, not how to execute steps.

### Creating a simple source file

Let's create a simple source file `01_test_haskell.hs` and create two functions there. We can also use comments (`{- -}` for multiline and `--` for single line comment).

```haskell
{-
   My first Haskell source file

   This is a multiline comment.
-}

-- This is a single-line comment

-- Linear function: a * x + b
linear :: Integer -> Integer -> Integer -> Integer
linear a b x = a * x + b

-- Check whether three lengths can form a triangle
isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c =
    (a + b > c) &&
    (a + c > b) &&
    (b + c > a)
```

### Definitions in source files

Each top-level definition has the form:

```haskell
name = expression
```

or, for functions:

```haskell
name parameters = expression
```

This is best read as **an equation, not an assignment**. For example:

```
linear a b x = a * x + b
```

means: *For any values a, b, and x, linear a b x is defined as a * x + b.*

There is no state, no mutation, and no execution order implied here — just definitions!

### Type signatures as contracts

You can optionally provide a **type signature** before a definition:

```haskell
linear :: Integer -> Integer -> Integer -> Integer
```

This states:

* the number of arguments,
* their types, and
* the result type.

Type signatures: 

* document intent,
* help the compiler catch mistakes,
* and make code easier to reason about.

Although Haskell can often infer types automatically, **writing type signatures is strongly recommended**, especially in source files it is considered as good practice. They serve as documentation and help catch errors early.

### Loading a source file into GHCi

You can load a source file into GHCi using `:load` (or `:l`):

```
ghci> :load 01_test_haskell.hs
[1 of 1] Compiling Main             ( 01_test_haskell.hs, interpreted )
Ok, modules loaded: Main.
```

The file is compiled and its definitions become available in GHCi (`Main` is the default module name for files without an explicit `module` declaration).

You can now use the functions:

```
*Main> linear 5 3 7
38

*Main> isTriangle 3 4 5
True
```

### Inspecting definitions

GHCi provides several commands to explore loaded code.

Check a type:

```
*Main> :type linear
linear :: Integer -> Integer -> Integer -> Integer
```

Find where a name is defined:

```
*Main> :info linear
linear :: Integer -> Integer -> Integer -> Integer
        -- Defined at 01_test_haskell.hs:7:1
```

List everything defined in the current module (or in a specific module):

```
*Main> :browse
linear :: Integer -> Integer -> Integer -> Integer
isTriangle :: Double -> Double -> Double -> Bool

*Main> :browse Prelude
...
```

### Updating code

Now we can add another functions to the file:

```haskell
conforms :: Integer -> Bool
conforms x = (x > 10 && x < 15) || x == 0
-- . is function composition (as in math)
notConforms = not . conforms

-- use of if-then-else expression
conforms' :: Integer -> String
conforms' x = if conforms x then "conforms" else "does not conform"

-- basic pattern matching
isZero :: Num a => a -> Bool
isZero 0 = True   -- pattern matching for 0
isZero _ = False  -- wildcard pattern (_ = "anything else" and not bind it)
```

If you modify the source file, GHCi does not pick up changes automatically.
To recompile and reload the file, use:

```
*Main> :reload
[1 of 1] Compiling Main             ( 01_test_haskell.hs, interpreted )
Ok, modules loaded: Main.

*Main> :browse
linear :: Integer -> Integer -> Integer -> Integer
isTriangle :: Double -> Double -> Double -> Bool
conforms :: Integer -> Bool
notConforms :: Integer -> Bool
...
```

This replaces the current module with a new version built from the file. That is different from rebinding names interactively with `let`:

* in source files, definitions are fixed until you recompile,
* you cannot redefine the same name twice at the top level of a file (try it and see what happens).

## Compiling a program with GHC

So far, we have:

* evaluated expressions in GHCi,
* written definitions in source files,
* loaded those files interactively.

That is of course not enough for real applications. Now we will take a small step further and compile a Haskell program into an **executable** using the compiler directly.

This section shows what happens *without* Stack. Later, **Stack will automate and extend this workflow**.

### A minimal executable

Create a file `01_hw.hs` with:

```haskell
main :: IO ()
main = putStrLn "Hello, world!"
```

Here, `main` is a definition with a special meaning:

* it is the entry point of the program,
* its type `IO ()` explicitly indicates that the program performs input-output effects.

Everything else in the file follows the same principles as before: `main` is still just a name bound to an expression, in this case, an effectful computation that prints a string.

### Compiling with GHC

Use the GHC compiler to compile the source file into an executable:

```
% ghc 01_hw.hs
[1 of 1] Compiling Main             ( 01_hw.hs, 01_hw.o )
Linking 01_hw ...
```

GHC performs several steps here:

1. type-checks the code,
2. compiles it into an object file (`01_hw.o`),
3. links it into an executable (`01_hw`).

It is a simple one-file program, so there is only one source file to compile into one object file. In real projects, there are usually multiple source files (modules) that GHC compiles and links together.

After compilation, you will see several new files in the directory:

* `01_hw.hs` = source file
* `01_hw.hi` = interface file (type and module information)
* `01_hw.o` = object file
* `01_hw` = executable

You can now run the program:

```
% ./01_hw
Hello, world!
```

### Multiple source files (modules)

As programs grow, code is split into multiple files and modules. Create a directory `01_hw/` with two files: `HWLib.hs` and `Main.hs`.

```haskell
-- 01_hw/HWLib.hs
module HWLib where

greet :: String -> String
greet x = "Hello, " ++ x ++ "!"
```

(Notice that the file name `HWLib.hs` matches the module name `HWLib`. Same for `Main.hs` and module `Main`. Modules and filenames must correspond like this and use PascalCase.)

```haskell
-- 01_hw/Main.hs
--
-- implicit module Main:
-- module Main where

import HWLib


main :: IO ()
main = do  -- 'do' notation for sequencing IO actions
    putStrLn "Enter your name:"
    -- 'getLine' is an IO action that reads a line from input
    -- result is bound to 'name' using <- syntax
    name <- getLine
    putStrLn (greet name)
```

Instead of compiling each file separately, you can compile the whole program at once by specifying the *main source file*. GHC will automatically find and compile the imported modules as needed (no need for complicated `Makefile`s).

```
% ghc --make Main.hs
[1 of 2] Compiling HWLib            ( HWLib.hs, HWLib.o )
[2 of 2] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
```

GHC in this case:

1) reads import declarations,
2) finds required source files,
3) compiles modules in the correct order,
4) links a final executable, that you can run:

```
% ./Main
Enter your name:
Marek
Hello, Marek!
```

## First project with Stack

In the previous section, we compiled a Haskell program directly using GHC.
While this works well for small examples, real projects quickly need more structure and automation.

This is where [Stack] comes in as a *project management tool* that builds on top of GHC and Cabal. It addresses several practical concerns at once:

* managing project structure,
* handling dependencies and their versions,
* selecting and installing the correct compiler version,
* ensuring reproducible builds across different machines.

In other words, Stack automates the manual steps we just performed, while preserving the same underlying compilation model. It is a really powerful tool, you can find more by reading documentation or just with `stack --help` or read [docs](https://docs.haskellstack.org/en/stable/README/).

### Creating a new Stack project

First, verify that Stack is installed:

```
% stack --version
```

Create a new project using a standard template:

```
% stack new HelloWorld
...
% cd HelloWorld
```

Stack generates a complete project structure for your new Haskell application. There are various templates available (and you can adjust them); the default one is a simple executable project.

### Project structure

A newly created Stack project typically looks like this:


```
HelloWorld/
├── app/
│   └── Main.hs
├── src/
│   └── Lib.hs
├── test/
│   └── Spec.hs
├── .gitignore
├── LICENSE
├── package.yaml
├── README.md
└── stack.yaml
```

You do not need to understand every file immediately. The most important parts are (check them out):

* `src/` – application logic (library code)
* `app/` – executable entry point(s), this is where `main` lives
* `test/` – test suite(s)
* `package.yaml` – project metadata and dependencies (using [hpack] format for managing Cabal files)
* `stack.yaml` – compiler version and build configuration

In larger real-world projects, a single Stack project can contain multiple packages (libraries and executables), but in this simple case we have just one executable package called `HelloWorld` with a library component (`src/Lib.hs`) and an executable component (`app/Main.hs`).

### Writing library code

```haskell
-- src/Lib.hs
module Lib
    ( greet  -- export only 'greet' function
    ) where

greet :: String -> String
greet name =
    "Hello, " ++ name ++ "!"

hiddenFunction :: String -> String
hiddenFunction secret =
    "Secret: " ++ secret
```

This module:

* defines pure functionality,
* exports only what should be visible from outside,
* contains no input/output or effects.

### Writing the entry point

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

Again, effectful operations are explicitly localized in `main`. The *pure function* `greet` is reused without modification.

**Pure function** = a function that always produces the same output for the same input and has no side effects (like modifying global state or performing I/O). It has nice mathematical properties and is easier to reason about, test, and reuse. Moreover, it is guaranteed that on the same input, it will always return the same output without any hidden behavior.

### Building the project

Now build the project simply with:

```
% stack build
```

If Stack reports that the required GHC version is not installed, it will offer to install it for you (using GHCup under the hood). Just confirm and wait until the installation finishes. In some older versions of Stack, you might need to do:

```
% stack setup
```

The `setup` command installs the correct GHC version specified in `stack.yaml`. After that, you can run `stack build` again.

### Running the application

After a successful build, run the application with:

```
% stack exec HelloWorld-exe
```

Here `HelloWorld-exe` is the name of the executable defined in `package.yaml` (you can check it there). Stack ensures that the correct environment is set up so that the application can find its dependencies. Eventually, a Stack project may contain multiple executables, so you specify which one to run.

There is also option to use `stack run` which builds (if needed) and runs the application in one step:

```
% stack run
```

Note: there are no new files created in the project directory after building with Stack. The compiled artifacts are stored in a hidden `.stack-work/` directory managed by Stack. You can explore it if you want, but usually you do not need to worry about it. There is also `stack clean` command to remove all build artifacts and `stack path` to show various important paths related to the project.

### Interactive REPL with Stack

You can also start an interactive session with the entire project loaded:

```
% stack ghci
```

This provides:

* all project modules,
* all dependencies,
* the same environment used for building.

Inside GHCi, you can explore modules as before:

```
*Main Lib> :browse Lib
greet :: String -> String
```

### Managing dependencies

As mentioned, Stack handles dependencies automatically. You specify them in `package.yaml`, and Stack takes care of downloading, building, and linking them. No need to manually manage library versions or deal with complex build scripts. In `stack.yaml`, you can adjust what resolver (set of package versions) to use, add extra dependencies, and configure build options.

First, you must find the package you need (using [Hoogle], [Hackage], or [Stackage]) and then add it to the `dependencies` section of `package.yaml`. It is good practice to check the license, documentation, and whether the package is actively maintained.

If you look up `Data.Set` ([Hoogle], [Stackage] or [Hackage]), you will find out that it is in package `containers` licensed under BSD with maintainer email libraries@haskell.org (see [here](http://hackage.haskell.org/package/containers/docs/Data-Set.html)). In Stackage, you can also see which version is included in the snapshot (resolver) you use.

However, if you now try to add the follow in your `Lib.hs`:

```haskell
import Data.Set

namesSet = insert "Robert" (insert "Marek" empty)
```

The `stack build` will fail due to missing dependency:

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

Then after the build you can also try it out with `stack ghci`:

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

For the first assignment, use the `hw01` project and follow the instructions in the `README.md` file there. It is a very basic exercise to get you familiar with Haskell syntax, GHCi, and Stack project.

## Further reading

* [Why Functional Programming Matters](https://wiki.ccmi.fit.cvut.cz/_media/programming:why_functional_programming_matters.pdf)
* [Why Haskell Matters](https://dzone.com/articles/why-haskell-matters)
* [Learn You a Haskell for Great Good](http://learnyouahaskell.com)
* [School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell)
* [Stack User Guide](https://docs.haskellstack.org/en/stable/GUIDE/)
* [GHC User Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)

[Cabal]: https://www.haskell.org/cabal/
[GHC]: https://www.haskell.org/ghc/
[GHCup]: https://www.haskell.org/ghcup/
[Hackage]: https://hackage.haskell.org
[Haskell]: https://www.haskell.org
[Haskell 2010]: https://www.haskell.org/onlinereport/haskell2010/
[Hoogle]: https://www.haskell.org/hoogle/
[hpack]: https://github.com/sol/hpack
[Stack]: https://docs.haskellstack.org
[Stackage]: https://www.stackage.org
