# Structuration, branching, and evaluation

In this tutorial we look at how **Haskell structures computations**: how a function can choose between different results, how complex expressions can be decomposed into smaller named parts, and how (and when) expressions are actually evaluated.

Unlike imperative languages, branching in Haskell is **not control flow** in the sense of *what happens next*, but a way of defining **which value an expression denotes**. Constructs such as `if-then-else`, `case-of`, guards, and patterns should therefore be understood as expressions and equations, not as commands.

This viewpoint is very close to mathematics: functions are defined by **cases**, **conditions**, and **equations**, and some expressions may be **undefined** or only partially defined. Haskell’s lazy evaluation strategy makes this explicit and allows us to work naturally with infinite structures and delayed computation.

By the end of this tutorial, you should understand how branching, local definitions, and evaluation interact, and why thinking in terms of *values rather than execution steps* is essential when programming in a functional language.

## If and case

Branching in Haskell is often the first place where programmers coming from imperative languages get confused. In languages like C, Java, or Python, branching primarily controls what happens next.
In Haskell, branching decides which value an expression denotes.

In other words: `if-then-else` and `case-of` are expressions, not control-flow statements.

This is much closer to mathematics, where functions are commonly defined by cases.

### `if-then-else` as a value selector

Conceptually, an if-then-else expression chooses between two values based on a condition. In mathematics, this corresponds to a piecewise-defined function:

```math
f(x) = \begin{cases}
    a & \text{if } P(x) \\
    b & \text{otherwise}
\end{cases}
```

In Haskell, this is expressed as:

```haskell
f x = if P x then a else b
```

As you can see, both branches (`then` and `else`) are expressions that yield values of the same type. The entire `if-then-else` expression itself has a type determined by the types of these branches (for example, if both `a` and `b` are of type `Int`, then the whole expression is of type `Int`).

### Implementing own `ifThenElse`

To make this idea explicit, let’s implement our own version of `if-then-else`.

First, consider the type:

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse condition onTrue onFalse = ...
```

* the `condition` has type `Bool`;
* both branches (`onTrue` and `onFalse`) have the same type `a`;
* the result also has type `a`.


We can implement this without using `if`, by pattern matching on `Bool` values:

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  onTrue onFalse = onTrue
ifThenElse False onTrue onFalse = onFalse
```

We can even simplify it a bit with ignoring the unneeded argument using `_`, i.e., wildcard (as you always should do in Haskell):

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  onTrue  _ = onTrue
ifThenElse False _ onFalse = onFalse
```

This already shows an important fact: **branching is just pattern matching on a value**.

We can test our implementation of `ifThenElse` in GHCi and we can see that it works pretty well:

```
ghci> x = 7

ghci> y = 15

ghci> ifThenElse (x < y) (y - x) (x - y)
8

ghci> x = 50

ghci> ifThenElse (x < y) (y - x) (x - y)
35
```

### Built-in `if-then-else` syntax

Of course, Haskell provides the usual syntax, as we already know:

```
ghci> if (x < y) then (y - x) else (x - y)
35
```

Nested conditions are written by nesting if expressions:

```haskell
if x < y then (y - x)
else if x == y then 0
else (x - y)
```

This works because each branch is itself an expression, i.e. you can use brackets:

```haskell
if x < y then (y - x)
else (if x == y then 0
      else (x - y))
```

Never forget that **type consistency is required**: both branches must have the same type, otherwise you will get a type error:

```
ghci> :type (if 8 > 5 then 5 else 3)
(if 8 > 5 then 5 else 3) :: Num a => a

ghci> :type (if 8 > 5 then 5 else "No")

<interactive>:1:16: error:
    • No instance for (Num String) arising from the literal ‘5’
    • In the expression: 5
      In the expression: if 8 > 5 then 5 else "No"
      In the expression: if 8 > 5 then 5 else "No"
```

### Branching by structure with `case-of`

While `if-else` branches on a `Bool` value, `case-of` branches on the **structure of a value** (typically described by data constructors). It is similar in spirit to `switch-case` you may know, but it is far more powerful.

```haskell
data Color = Black | White | RGB Int Int Int

describeBW :: Color -> String
describeBW c = case c of
    Black           -> "black"
    White           -> "white"
    RGB 0 0 0       -> "black"
    RGB 255 255 255 -> "white"
    _               -> "unknown"  -- "default" match
```

Here, `case-of` performs pattern matching, selecting a result based on which pattern matches the input value.

Mathematically, this is again a definition by cases, but now based on structure, not just a `Bool` condition. The wildcard `_` acts as a default case, matching anything not covered earlier (default case, *otherwise*).

### Exhaustive patterns

```haskell
badDescribeBW :: Color -> String
badDescribeBW c = case c of
    Black -> "black"
    White -> "white"
```

You need to be careful that you cover all the cases with `case-of`. If you hit some case which is not covered an exception will come up at runtime:

```
*Main> badDescribeBW (RGB 0 0 0)
"*** Exception: files/03_caseColors.hs:(4,19)-(6,33): Non-exhaustive patterns in case
```

### `case-of` with lists

Patterns in `case-of` work for all algebraic data types, including lists:

```haskell
describeList :: [a] -> String
describeList xs =
    "The given list has " ++ case xs of
        []  -> "no items."
        [_] -> "just one item."
        _   -> "more than one item."
```

This style is often clearer than nested `if` expressions and highlights the structure of the data being analyzed.

### `case-of` in function definitions

Very often, a `case-of` expression does not need to be written explicitly at all. Instead, the same pattern matching can be moved directly into the function definition itself (*syntactic sugar* again).

The `describeList` function can be rewritten as:

```haskell
describeList' :: [a] -> String
describeList' []  = "The given list has no items."
describeList' [_] = "The given list has just one item."
describeList' _   = "The given list has more than one item."
```

These two definitions `describeList` and `describeList'` are **semantically equivalent**.

```math
\text{describeList}(xs) = 
    \begin{cases}
        \text{"The given list has no items."} & \text{if } xs = [] \\
        \text{"The given list has just one item."} & \text{if } xs = [\_] \\
        \text{"The given list has more than one item."} & \text{otherwise}
    \end{cases}
```

### Order matters

Patterns are matched from top to bottom, so the order of patterns matters. The first matching pattern is selected, and subsequent patterns are ignored.

```haskell
describeList'' :: [a] -> String
describeList'' []  = "The given list has no items."
describeList'' _   = "The given list has more than one item."
describeList'' [_] = "The given list has just one item."
```

### Named patterns (as-patterns)

Sometimes, while matching a pattern, we want access both to the decomposed parts and to the whole value. This is exactly what *as-patterns* provide.

An as-pattern has the form: `name@pattern`

```haskell
duplicateFirstElement :: [a] -> [a]
duplicateFirstElement [] = []
duplicateFirstElement list@(x:_) = x : list
```

Here:

* `(x:_)` decomposes the list,
* `list` refers to the entire original list.

Without a named pattern, we would have to reconstruct the list manually:

```haskell
duplicateFirstElement :: [a] -> [a]
duplicateFirstElement [] = []
duplicateFirstElement (x:xs) = x : (x:xs)
```

## Guards and patterns

So far, we have seen two ways to define functions by cases:

* `if-then-else` – branching on a `Bool`
* pattern matching (`case-of`) – branching on the structure of values

*Guards* combine these two ideas: they allow us to branch based on `Bool` conditions, while keeping the clarity of pattern-based function definitions.

In mathematics, this corresponds to defining a function by **conditions**.

### Guards

A function with guards consists of:

* a function name and arguments,
* a sequence of Boolean conditions introduced by `|`,
* a result expression for each condition.

```haskell
myMax :: (Ord a) => a -> a -> a
myMax a b
    | a > b     = a
    | otherwise = b
```

This should be read as:

> If `a > b`, the result is `a`; otherwise, the result is `b`.

The keyword `otherwise` is just a synonym for `True` and is typically used as the default case.

Again, a similar to math even in the way how it is written:

```math
\text{myMax}(a, b) =
    \begin{cases}
        a & \text{if } a > b \\
        b & \text{otherwise}
    \end{cases}
```

### Order of guards matters

Guards are tested from top to bottom (same as for pattern matching). The first condition that evaluates to `True` determines the result:

```haskell
guardsOrder x
    | x < 5 = "x < 5"
    | x < 0 = "x < 0"
    | x > 2 = "x > 1"
    | otherwise = "otherwise"
```

Even though `x < 0` implies `x < 5`, it is never reached, because the first guard already matches. This mirrors both:

* the order of equations in function definitions
* the order of patterns in `case-of`

### `_` vs `otherwise`

It is important to distinguish between:

* `_` (also called *hole*) – a pattern that matches anything
* `otherwise` – a `Bool` expression equal to `True`

```
ghci> :t otherwise
ghci> otherwise == True
True
ghci> not otherwise
False
otherwise :: Bool
ghci> :t _

<interactive>:1:1: error:
    • Found hole: _ :: t
      Where: ‘t’ is a rigid type variable bound by
               the inferred type of it :: t at <interactive>:1:1
    • In the expression: _
```

### Combining patterns and guards

Patterns and guards are often used together. Patterns decompose values, while guards express additional conditions:

```haskell
describeList :: [a] -> String
describeList [] = "empty list"
describeList xs
    | length xs == 1 = "single element"
    | otherwise      = "longer list"
```

It can be also written like this but that introduces unnecessary repetition of pattern `xs`:

```haskell
describeList :: [a] -> String
describeList [] = "empty list"
describeList xs | length xs == 1 = "single element"
describeList xs | otherwise      = "longer list"
```

### Guards vs if-then-else

Any guard-based definition can be rewritten using `if-then-else`, but guards are usually:

* clearer
* closer to mathematical notation
* easier to extend with more cases

```haskell
absValue x =
    if x >= 0 then x else -x

-- vs

absValue x
    | x >= 0    = x
    | otherwise = -x
```

## Local definitions: `let-in` and `where`

So far, we have defined functions by equations, patterns, and conditions. Often, however, a function becomes clearer when we name intermediate expressions.

In mathematics, this is usually done by introducing auxiliary variables or helper functions. In Haskell, we do this using local bindings, most commonly via `let-in` and `where`.

### `let-in`: local bindings as expressions

The construct

```
let ... in ...
```

is itself an expression. It introduces local names that are visible only in the expression following in.

```haskell
circleArea radius =
    let pi = 3.14159
    in pi * radius^2
```

This should be read as:

> Let `π` be `3.14159`; then the result of circle area with radius `radius` is `π * radius^2`.

Because `let-in` is an expression, it can be used **anywhere** an expression is allowed.

#### Reuse and readability

Local bindings typically serve two purposes:

1. Avoid repetition (DRY – Don’t Repeat Yourself).
2. Give a name to a sub-expression, improving readability.

```haskell
cylinderVolume radius height =
    let area = circleArea radius
    in height * area
```

Bindings may also introduce local functions:

```haskell
blockVolume width height depth =
    let area a b = a * b
    in area width height * depth
```

#### Shadowing and immutability

All bindings in Haskell are **immutable**. Introducing a new name with `let` does not modify an existing one, it merely **shadows** it.

```haskell
testLetIn1 =
    let a = 7
        b = 5
        c = 10
    in a
-- testLetIn1 == 7

testLetIn2 =
    let a = 7
        b = 5
    in let a = 2 * b  -- shadows previous 'a'
       in a
-- testLetIn2 == 10

testLetIn3 =
    let a = 7
    in let a = a + 3  -- shadows previous 'a' (but on right side, old 'a' is used)
       in a
-- testLetIn3 == 10

testLetIn4 =
    let a = 7
        a = 5  -- error: Conflicting definitions for ‘a’
    in a
```

#### One-line `let`

When writing `let-in` on a single line, bindings are separated by semicolons:

```haskell
let a = 10; b = 20; c = 30 in a * b * c
```

or via pattern matching and ad-hoc tuples:

```haskell
let (a, b, c) = (10, 20, 30) in a * b * c
```

Semicolons are syntactically valid but should be used sparingly, as they often reduce readability.

### `where`: definitions after the equation

The `where` construct serves the same purpose as `let-in`, but places bindings after the main expression:

```haskell
circleArea radius = pi * radius^2
                  where pi = 3.14159
```

This style closely resembles mathematical definitions:

> The area of a circle with radius `radius` is `π * radius^2`, where `π = 3.14159`.

#### `where` with functions and recursion

`where` is especially useful for structuring more complex definitions:

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = low ++ [x] ++ high
                 where low  = quicksort (filter lqPivot xs)
                       high = quicksort (filter gtPivot xs)
                       lqPivot y = y <= x
                       gtPivot y = y >  x
```

This reads top-down:

* the main idea of the function first,
* helper definitions afterwards.

#### Patterns in `where`

Local definitions can themselves introduce further local bindings:

```haskell
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname
```

#### `where` with guards

`where` combines naturally with guards, avoiding repeated computation:

```haskell
birthYearToTitle :: Int -> String
birthYearToTitle year
    | age <= 12 = "Kid"
    | age <= 19 = "Teenager"
    | age <= 65 = "Adult"
    | otherwise = "Senior"
  where
    age = currentYear - year
    currentYear = 2018
```

This mirrors mathematical definitions with shared auxiliary values.

### `let-in` vs `where`

Both constructs introduce local bindings, but they differ in usage:

| `let-in`            | `where`                             |
|---------------------|-------------------------------------|
| is an expression    | attached to a definition            |
| can appear anywhere | appears after a function equation   |
| emphasizes locality | emphasizes readability of structure |

In practice:

* use `let-in` inside expressions,
* use `where` to structure function definitions.

## Evaluation and laziness

So far, we have focused on **what expressions mean**. *Evaluation* answers a different question:

> When is an expression actually computed?

In Haskell, evaluation is **lazy (non-strict)**, meaning that expressions are evaluated **only when their values are needed** (i.e. expression needs to be replaced by its value).

This is one of the defining characteristics of the language. It enables powerful programming techniques, such as working with infinite data structures and separating control flow from evaluation order.

### Bottom (⊥): computations without a value

To talk about evaluation, we need a name for computations that **never successfully produce a value**. In Haskell, this is commonly called **bottom**, written as ⊥.

A computation can be bottom because:

* it crashes with an error,
* it never terminates,
* it is explicitly marked as `undefined`.

```haskell
errorBottom :: a
errorBottom = error "Something went wrong"

nonterminatingBottom :: a
nonterminatingBottom = nonterminatingBottom

undefinedBottom :: a
undefinedBottom = undefined
```

A crucial property of bottom is:

> ⊥ has every type, but no value.

This allows such expressions to type-check, even though they fail at runtime.

### Demand-driven evaluation

The key rule of Haskell evaluation is:

> An expression is evaluated only if its value is demanded.

```haskell
answer :: Int
answer = 42
```

Now in GHCi:

```
ghci> x = undefined

ghci> answer
42
```

Even though `x` is `undefined`, nothing crashes — because value of `x` is never needed.

But if we demand its value:

```ghci> x
*** Exception: Prelude.undefined
```

### Laziness in function arguments

Function arguments are evaluated only when needed:

```haskell
ignoreSecond :: a -> b -> a
ignoreSecond x _ = x
```

In GHCi:

```
ghci> ignoreSecond 7 undefined
7
ghci> ignoreSecond undefined 10
*** Exception: Prelude.undefined
```

Only the arguments that are actually used are evaluated. This is a direct consequence of laziness and is impossible in strict languages without special constructs.

### When evaluation is forced

Some operations cannot proceed without actual values. For example, arithmetic requires its arguments to be evaluated:

ghci> 3 + undefined
*** Exception: Prelude.undefined

This does not contradict laziness — it simply means the *value is now required*. Laziness delays evaluation, but it does not eliminate it.

### Infinite structures

Because evaluation is demand-driven, Haskell allows us to define infinite data structures.

```haskell
ones :: [Int]
ones = 1 : ones
```

```
ghci> take 5 ones
[1,1,1,1,1]

ghci> take 10 ones
[1,1,1,1,1,1,1,1,1,1]
```

Only the needed prefix of the list is evaluated.

Trying to evaluate the whole list, however, never finishes:

```
ghci> ones
-- runs forever
```

Mathematically, this is similar to defining an infinite sequence and computing only its first *n* elements.

### Controlling evaluation (briefly)

Sometimes, laziness is not desirable — for example, for performance or memory reasons. Haskell provides tools to force evaluation, such as:

* strict application: `$!`,
* bang patterns: `!`,
* strict fields in data types.

These are advanced topics and should be used deliberately, not by default. Sometimes you see `Strict` variants of functions or data structures in libraries, which use strictness internally for efficiency (laziness is not for free and comes with overhead).

## From evaluation to list comprehensions

Laziness becomes especially powerful when combined with **list construction**.

So far, we have created lists using constructors and ranges. Next, we will look at **list comprehensions**, a concise and expressive way to describe lists — often corresponding closely to mathematical set and sequence notation.

Crucially, list comprehensions are *lazy*: even if they describe infinite lists, only the required elements are evaluated.

### List comprehensions

A list comprehension has the form:

[ expression | generators, conditions, local bindings ]


This mirrors mathematical notation:

```math
\{ f(x) \mid x \in S, P(x) \}
```

For example:

```haskell
[x | x <- [0..10], x `mod` 3 == 1]
```

This generates a list of numbers from 0 to 10 that leave a remainder of 1 when divided by 3.

```math
\{ x \mid x \in \{0, 1, \ldots, 10\}, x \mod 3 = 1 \}
```

### Multiple generators

Generators are evaluated left-to-right, producing combinations lazily.

```haskell
[(x, y) | x <- [1, 2], y <- ['a', 'b']]
```

### Laziness in list comprehensions

List comprehensions can describe infinite lists, evaluated lazily:

```
take 5 [n^2 | n <- [1..]]
[1,4,9,16,25]
```

Even though `[1..]` is infinite, only the first `5` squares are computed.

### Local bindings in comprehensions

List comprehensions can introduce local names using let:

```haskell
[(i, j) | i <- [1..5], let j = i * i]
```

This ties list comprehensions back naturally to local definitions and evaluation.

## Modules and imports

As we have seen before, Haskell program is organized into modules. A module groups related definitions (functions, types, type classes) and controls what is visible from the outside. This is essential for:

* structuring larger programs,
* avoiding name clashes,
* making dependencies explicit.

### Modules

Every Haskell source file defines exactly one module. If no module header is given, the module name defaults to `Main`.

To remind you, a module name:

* starts with a capital letter,
* consists of alphanumeric components separated by dots,
* typically mirrors the directory structure.

Example:

```haskell
module FPCourse.Lesson3.TestModule (
    myFunc1,
    myFunc3
) where

myFunc1 x = myFunc2 7 x

myFunc2 x y = x - y

myFunc3 x y z = x * y + z
```

Here:

* `myFunc1` and `myFunc3` are public,
* `myFunc2` is private to the module.

This explicit export list defines the public API of the module.

### Importing a module

To use definitions from another module, use the `import` keyword.

```haskell
import FPCourse.Lesson3.TestModule

x = myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc1 25
```

A *plain import*:

* brings all exported names into scope,
* allows both qualified and unqualified usage.

This is convenient, but can pollute the namespace in larger programs.

### Explicit imports (recommended)

A good practice is to import only what you need:

```haskell
import FPCourse.Lesson3.TestModule ( myFunc1 )

x = myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc1 25
```

Benefits:

* improves readability (“where does this come from?”),
* prevents accidental name clashes,
* documents dependencies clearly.

### Qualified imports

To avoid polluting the namespace entirely, you can import a module qualified:

```haskell
import qualified FPCourse.Lesson3.TestModule

x = FPCourse.Lesson3.TestModule.myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc2 25

myFunc1 :: Int -> Int
myFunc1 param = FPCourse.Lesson3.TestModule.myFunc1 (param + 5)
```

With a *qualified import*:

* names must be prefixed by the module name,
* it is always clear where a function comes from.

### Qualified imports with an alias

Fully qualified names can become long, so aliases are commonly used:

```haskell
import qualified FPCourse.Lesson3.TestModule as FPTM

x = FPTM.myFunc1 10
y = FPTM.myFunc2 25
```

This is the most common style when importing larger libraries.

### Avoiding name clashes

Consider this import:

```haskell
import Data.Set (map)
```

Now both `Prelude` (imported implicitly) and `Data.Set` define functions like `map`, which can lead to confusion.

A safer approach is:

```haskell
import qualified Data.Set as S
```

```
map (+2) [1,2,2,3]
S.map (+2) (S.fromList [1,2,2,3])
```

### Hiding imports

Another way to deal with name clashes is to hide specific names. With the following example, `myFunc2` from the module is not imported and can be redefined locally:

```haskell
import FPCourse.Lesson3.TestModule hiding (myFunc2)

myFunc2 :: Int -> Int -> Int
myFunc2 x y = x + y + 666
```

## Textual types

Working with text is a common task, but in Haskell it comes with some historical and conceptual baggage.
Unlike many languages that have a single “string” type, Haskell offers **several textual representations**, each optimized for different use cases.

Understanding these types—and choosing the right one—is important for both correctness and performance.

### `String`

The simplest and most familiar textual type is String.

```haskell
type String = [Char]
```

That is, a `String` is just a list of characters.

Because of this:

* all list operations work on `String`,
* pattern matching behaves exactly like with lists,
* it is very convenient for teaching and small programs.

However, this also means:

* high memory overhead,
* poor performance for large texts,
* inefficient Unicode handling in some scenarios.

For non-performance-critical code, `String` is perfectly fine. For larger or more serious applications, better alternatives exist.

### `Text`

The text package provides [Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html), a time- and space-efficient Unicode text type.

```haskell
import qualified Data.Text as T
```

You can convert between String and Text using:

```haskell
T.pack :: String -> Text
T.unpack :: Text -> String
```

Example:

```
ghci> import qualified Data.Text as T

Prelude T> txt = T.pack "my effective text"

Prelude T> :type txt
txt :: T.Text

Prelude T> T.index txt 1
'y'
```

Functions for Text often have the same names as list or String functions, which is why qualified imports are strongly recommended.

A common beginner mistake:

```
Prelude T> length txt
-- type error
Prelude T> T.length txt
17
```

There are two variants:

* **strict**: `Data.Text`
* **lazy**: `Data.Text.Lazy`

The lazy variant is useful for very large texts or streaming scenarios and integrates well with Haskell’s lazy evaluation model.

### `ByteString`

`ByteString` represents raw sequences of bytes and is provided by the `bytestring` package and its [Data.ByteString](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html) module.

```haskell
import qualified Data.ByteString as B
```

It is commonly used for:

* binary data,
* file I/O,
* network communication,
* interoperability with C libraries.

Example:

```
ghci> import qualified Data.ByteString as B

ghci> bstr = B.pack [97, 98, 99]

ghci> bstr
"abc"

ghci> B.index bstr 2
99
```

Note that this works with bytes (0–255), not characters.

### `ByteString.Char8`

For ASCII-based text, there is a convenient variant [Data.ByteString.Char8](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Char8.html):

```haskell
import qualified Data.ByteString.Char8 as C
```
```
ghci> C.pack "abc"
"abc"

ghci> C.index (C.pack "abc") 2
'c'
```

This treats bytes as characters in the range 0–255 and should only be used when Unicode is not required.

### Encoding and decoding text

To convert between textual data and raw bytes, use encoding modules:

```haskell
import qualified Data.Text.Encoding as E
```

```
ghci> E.encodeUtf8 (T.pack "život, жизнь, lífið")
```

Encoding makes it explicit:

* how characters are represented as bytes,
* what assumptions are made about character sets.

This explicitness is one of Haskell’s strengths, even if it feels verbose at first.

### `OverloadedStrings` (GHC extension)

Having to manually pack every string literal can be annoying. The language extension OverloadedStrings helps with this.

Enable it in a source file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

or in GHCi:

```
:set -XOverloadedStrings
```

Now *string literals become polymorphic*:

```
ghci> :type "abc"
"abc" :: IsString p => p
```

This allows code like the following, without explicit `pack`:

```
ghci> import qualified Data.Text as T

ghci> T.length "abc"
3
```

### Which textual type to use

A practical rule of thumb:

* `String` = teaching, small programs, quick scripts
* `Text` = most real-world Unicode text processing
* `ByteString` = binary data, I/O, networking

You can always convert between them when needed and sometimes your choice will depend on library support.

## Task assignment

For the assignment, navigate to the `hw03` project and follow the instructions in the `README.md` file there. It will test your skills in branching, local definitions, and working with modules.

## Further reading

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com) (chapters 4, 7)
* [Haskell: Pattern matching](https://en.wikibooks.org/wiki/Haskell/Pattern_matching)
* [Haskell: Control structures](https://en.wikibooks.org/wiki/Haskell/Control_structures)
* [Haskell: List comprehension](https://wiki.haskell.org/List_comprehension)
* [Haskell: Lazy evaluation](https://wiki.haskell.org/Lazy_evaluation)
* [Haskell: Laziness](https://en.wikibooks.org/wiki/Haskell/Lazines)
* [Haskell: Modules](https://en.wikibooks.org/wiki/Haskell/Modules)
* [Haskell: Import](https://wiki.haskell.org/Import)
* [Haskell: Import modules properly](https://wiki.haskell.org/Import_modules_properly).
* [24 Days of GHC Extensions: Bang Patterns](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html)
* [Oh my laziness](http://alpmestan.com/posts/2013-10-02-oh-my-laziness.html)
* [Haskell String Types](http://www.alexeyshmalko.com/2015/haskell-string-types/)
* [Untangling Haskells strings](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings)
