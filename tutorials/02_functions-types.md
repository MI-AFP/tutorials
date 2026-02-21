# Functions and basic data types

In the previous introductory tutorial, we focused on:

* expressions and values,
* naming and bindings,
* functions as mappings between values, and
* how these ideas appear in GHCi and source files.

In this tutorial, we build on that foundation and make the picture more concrete. We will look at:

* how functions are written and typed,
* how Haskell represents data, and
* how syntax reflects the underlying functional and mathematical model.

Before diving into functions and data types themselves, it is useful to clarify some basic syntactic rules and naming conventions that appear throughout Haskell code. This part is not meant to be memorized — it is a reference that will become familiar as we encounter these constructs in practice.

## Haskell keywords and symbols

Haskell has a relatively small set of [**reserved keywords and symbols**](https://wiki.haskell.org/Keywords) with special meaning. You cannot use these as ordinary names. Some do not come directly from Haskell itself but from GHC language extensions.

### Keywords for definitions and structure

These keywords introduce or organize definitions:

* `module`, `import`, `qualified`, `hiding`
* `let`, `in`, `where`
* `data`, `newtype`, `type`
* `class`, `instance`, `deriving`

### Keywords for control and pattern matching

These keywords describe *how* expressions are selected or evaluated:

- `case` ... `of`
- `if` ... `then` ... `else`
- `do` (for sequencing effectful computations)

Unlike imperative languages, these are still expressions: each of them produces a value of some type.

### Type-related symbols

- `::` = "has type" (type annotation)
- `->` = function type constructor (from ... to ...)
- `=>` = type class constraint in type signature
- `|` = alternative in data type definition (sum type) or guards in function definition

### Operators and special symbols

- `=` = definition (binding a name to an expression)
- `<-` = extracting a value from an effectful computation (used inside `do` blocks)
- `\` = lambda (anonymous function)
- `@` = as-pattern (naming a pattern while matching)
- `:` = cons operator (constructing lists)
- `,` = tuple constructor (grouping values)
- `..` = range syntax (creating lists of enumerated values)
- `_` = wildcard pattern (ignoring a value)
- `` ` `` = backticks (turning a function into an infix operator)

### Operator fixity and associativity

Operators in Haskell can have different *precedence* (fixity) and *associativity* which affect how expressions with multiple operators are parsed. Later we will cover related keywords:

* `infix`
* `infixl`
* `infixr`

### Operators and code editors

Some code editors and fonts support so-called **font ligatures**, which visually combine certain character sequences into a single symbol. For example, the sequence `->` may be displayed as a single right arrow symbol (→). Some programmers love this feature, while others find it distracting... so try it out and see what you think!

While this can enhance readability, it is important to remember that these are purely visual enhancements and do not affect the actual code. When writing or reading Haskell code, always consider the underlying characters, especially when sharing code with others who may not have the same editor settings.

## Naming rules and conventions

Haskell is strict about **what names are allowed**, and the community follows **strong naming conventions**. Following them makes code easier to read and understand.

### Capitalization rules (syntactic)

These rules are enforced by the language:

* **Value names and function names** start with a lowercase letter or underscore (`_`), followed by letters, digits, underscores, and apostrophes (`'`). Examples: `x`, `myFunction`, `value1`, `_tempVar`, `count'`.
* **Type names and data/type constructor names** start with an uppercase letter, followed by letters, digits, underscores, and apostrophes. Examples: `MyType`, `Person`, `TreeNode`, `Result'`.
* **Type variables** (used in polymorphic types) start with a lowercase letter, often a single letter like `a`, `b`, `c`, etc.

Violating these rules leads to syntax errors or different meanings.

### Conventions for naming values and functions

These are conventions, not rules, but they are widely followed:

* Use **camelCase** for function and variable names: `calculateSum`, `isValid`, `userName`.
* Use **descriptive names** that convey purpose: `totalPrice` instead of `tp`, `isEven` instead of `myFunc`.
* **Predicate functions** should start with *is*, *has*, or *can*: `isEmpty`, `hasChildren`, `canExecute`.
* Use **verbs** for functions that perform actions: `getUser`, `setAge`, `computeAverage`.
* Use **apostrophes (`'`)** to indicate a modified version of a function or variable: `filter'` might be a variant of `filter`.
* Give **meaningful** names to parameters: `calculateArea width height` is clearer than `calculateArea x y`.

### Conventions for naming types and constructors

* Use **PascalCase** for type and constructor names: `BinaryTree`, `Person`, `Color`.
* Use (singular) **nouns** for type names: `User`, `Account`, `Order`.
* Use **descriptive names** that reflect the purpose of the type: `Success`, `Failure`, `Response`.
* For types with a single constructor, the constructor name is often the same as the type name: `data Color = Color Int Int Int`.

### Module and file naming

* Module names use **PascalCase** and reflect the directory structure: `Data.List`, `Control.Monad`.
* **File names correspond to module names** with `.hs` extension: `Data/List.hs`, `Control/Monad.hs`.
* Directories use **PascalCase** to match module names: `Data/`, `Control/`.


## Types, values, and definitions

Before introducing more complex data types, it is important to clarify how Haskell talks about types, values, and functions, and how these are introduced in code.

A recurring pattern in Haskell is that things are described in two steps:

* a **declaration** (what something is, what type it has), and
* a **definition** (how it is defined, what it equals).

This applies consistently to values, functions, and types.

### Type signatures

A type signature specifies the type of a value or function. It uses the operator ::, read as “*has type*”.

```haskell
x :: Integer
y :: String
```

This means:

* `x` is a value of type `Integer`,
* `y` is a value of type `String`.

Type signatures may also be attached directly to expressions:

```haskell
z = (x + 7) :: Integer
```

Haskell uses static typing:

* every expression has exactly one type,
* the type is determined at compile time, and
* it cannot change during execution.

Because of **type inference**, type signatures are not always required, but they are **strongly recommended for clarity and documentation**.

### Value declarations and definitions

A value is introduced by:

* an optional declaration (type signature), and
* a mandatory definition (binding).

```haskell
x :: Integer   -- declaration
x = 42         -- definition
```

The definition binds the name `x` to the expression `42` (in this case a literal integer).

There is no assignment and no mutation here — once defined, `x` always refers to the same value.

If types do not match, the compiler reports an error:

```haskell
y :: String
y = x
```

```
Couldn't match type ‘Integer’ with ‘[Char]’
Expected type: String
Actual type: Integer
```

Haskell **does not perform implicit type conversions**.

### Function declarations and definitions

Functions follow exactly the same pattern.

```haskell
identity :: a -> a          -- declaration
identity x = x              -- definition
```

* The declaration says that `identity` is a function that takes an argument of type `a` and returns a value of the same type `a`.
* The definition defines how the function computes its result (for any `x`, `identity x` is defined as `x`).

The definition can be naturally more complex, involving pattern matching, recursion, and other constructs.

```haskell
fibonacci :: Word -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

Such definition can be read as (always from top to bottom):

1. `fibonacci 0` is defined as `1`,
2. `fibonacci 1` is defined as `1`,
3. for any other `n`, `fibonacci n` is defined as the sum of `fibonacci (n - 1)` and `fibonacci (n - 2)`.

### Type variables

Type variables allow functions to be **polymorphic** — they can operate on values of any type.

```haskell
identity :: a -> a
```

The lowercase `a` is a type variable, meaning:

* for any type `a` (without any constraint),
* not a specific concrete type (like `Int` or `String`).

Type variables are usually single lowercase letters: `a`, `b`, `c`, etc. Multiple type variables can be used in a single type signature:

```haskell
const :: a -> b -> a
```

### Type constraints

Sometimes a function must restrict which types it works with. This is done using **type constraints** with the `=>` operator.

```haskell
next :: Num a => a -> a
next x = x + 1
```

This reads as:

* `next` is a function that takes an argument of type `a` and returns a value of type `a`,
* where `a` is constrained to types that are instances of the `Num` type class (i.e., numeric types).

Constraints specify requirements on type variables.

Multiple constraints are also possible:

```haskell
foo :: (Show a, Eq a, Read b) => a -> b -> a
```

In this case, `a` must be an instance of both `Show` and `Eq`, while `b` must be an instance of `Read`.

We will get to type classes later in the course, as they are a powerful mechanism for abstraction in Haskell.

### Type declarations and definitions

In addition to values and functions, Haskell allows you to introduce new data types. This is done using one of three constructs.

#### Type synonym

A type synonym introduces a new name for an existing type. It does not create a new type, just an alias.

```haskell
type String = [Char]
```

#### Data type

A data type introduces a completely new type with one or more constructors.

```haskell
data Bool = True | False
```

This one creates a new type `Bool` with two constructors (possible values): `True` and `False`.

A bit terminology:

* `Bool` is the **type constructor** (follows `data`, in this case with no type parameters),
* `True` and `False` are **data constructors** (follow `=`, separated by `|`, again with no parameters/fields).

More complex data types can have parameters and recursive structure. For example, a binary tree:

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

This creates a polymorphic type `Tree a` with two constructors: `Leaf`, which takes a value of type `a`, and `Branch`, which takes two subtrees:

* `Tree a` is the type constructor with one type parameter `a` (`a` is a type variable),
* `Leaf` is a data constructor that takes one argument of type `a`,
* `Branch` is a data constructor that takes two arguments, both of type `Tree a` (recursion).

```haskell
myTree :: Tree Int
myTree = Branch (Leaf -7) (Branch (Leaf 7) (Leaf 10))
```

* Parameters of type constructors are always type variables (like `a`).
* Parameters of data constructors can be of any type, including other user-defined types, or type variables from the type constructor.

#### Newtype

A `newtype` introduces a new type that is distinct from an existing type but has exactly one constructor with one field. It is often used to improve type safety and express intent without runtime overhead.

```haskell
newtype Age = Age Int
```

This creates a new type `Age` that is distinct from `Int`. The `Age` constructor takes one `Int` argument. But there is no runtime overhead compared to using `Int` directly.

```haskell
myAge :: Age
myAge = Age 20
```

### Basic built-in data types

Haskell provides a small set of basic data types that are used throughout the standard module (`Prelude`) and user code:

* `Int` = fixed-precision integer (machine-sized)
* `Integer` = arbitrary-precision integer
* `Float` = single-precision floating point
* `Double` = double-precision floating point
* `Word` = unsigned integer
* `Char` = Unicode character
* `Bool` = logical value (`True` or `False`)
* `String` = type synonym for `[Char]` (list of characters)

You have already encountered most of these in GHCi examples. More specialized data structures (maps, sets, sequences, text) live in libraries and will be introduced when needed.

### Algebraic Data Types (ADTs)

Haskell’s data declarations are called algebraic data types because they combine two fundamental ideas:

1. **Sum types** (alternatives) using `|` to define multiple constructors (e.g., `Bool` with `True | False`).
2. **Product types** (combinations) using constructors with multiple fields (e.g., tuples or records).

Most real-world data types are combinations of these ideas, allowing for rich and expressive representations of data.

### Record types

When a data constructor contains multiple fields, using positional arguments quickly becomes unreadable.

```haskell
data Person = Person String Int String String
```

To improve clarity, Haskell provides **record syntax**, allowing you to name fields explicitly.

```haskell
data Gender = Male | Female
            deriving Show

data Person = Person
    { name   :: String
    , age    :: Int
    , gender :: Gender
    , city   :: String
    } deriving Show
```

Record syntax:

* makes code more self-documenting,
* automatically generates accessor functions,
* improves readability when constructing and updating values.

Creating and updating values:

```haskell
p :: Person
p = Person { name = "Marek", age = 25, gender = Male, city = "Prague" }

older :: Person -> Person
older person = person { age = age person + 1 }
```

Note: Haskell records are implemented as *syntactic sugar* over algebraic data types. They are powerful but somewhat limited compared to record systems in newer languages. Languages inspired by Haskell, such as Elm and PureScript, provide more advanced record systems. We mention this mainly for context — it does not affect how we use records in this course.

#### Records and name conflicts

Record field names are just functions. This means they share a global namespace.

If two record types use the same field names, conflicts arise:

```haskell
data Pet = Pet
    { name :: String
    , age  :: Int
    } deriving Show
```

There are several ways to address this:

* use more specific field names (`personName`, `petName`),
* use modules to separate namespaces,
* or enable language extensions such as `DuplicateRecordFields`.

#### DuplicateRecordFields extension

The [`DuplicateRecordFields`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html) extension allows multiple record types to use the same field names.

```haskell
{-# LANGUAGE DuplicateRecordFields #-}

data Person = Person
    { name :: String
    , age  :: Int
    }

data Pet = Pet
    { name :: String
    , age  :: Int
    }
```

With this extension:

* the record definitions are accepted, but
* field access may become ambiguous.

For example, `name x` will not typecheck on its own, because `x` could be either a `Person` or a `Pet`. You must provide additional context to resolve the ambiguity, such as type annotations or using the record in a context where the type is known:

```haskell
getPersonName :: Person -> String
getPersonName p = name p

petAge :: Pet -> Int
petAge pet = age pet

olderPet :: Pet -> Pet
olderPet pet = pet { age = age pet + 1 }
```

This extension is useful when:

* modeling multiple domain entities with overlapping attributes,
* you want to keep field names short and meaningful,
* types are clear from context.

However, overusing it can reduce readability, especially for beginners. Explicit field names or module qualification are often simpler.

### OverloadedRecordDot extension

The [`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html) extension allows using the dot operator (`.`) for record field access, similar to object-oriented languages.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
data Person = Person
    { name :: String
    , age  :: Int
    }

getName :: Person -> String
getName p = p.name
```

This comes handy as it improves readability, especially when chaining field accesses (`p.address.city`). However, it also requires careful type management to avoid ambiguity.


## Lists and tuples

Haskell provides two fundamental built-in container types for grouping values:
**tuples** and **lists**. They are used everywhere and illustrate two different
ways of structuring data.

Other containers (maps, sets, sequences, vectors, streams, …) exist in libraries,
but tuples and lists are the core building blocks.

### Tuples: fixed structure, heterogeneous types

A **tuple** groups a fixed number of values.

* The number of elements is fixed.
* Each position has its own (possibly different) type.

```haskell
myTuple :: (Int, String, Bool, Double)
myTuple = (15, "String", True, 5.24)
```

Pattern matching can extract elements from tuples:

```haskell
myFunc :: (Int, String, Bool, Double) -> (Double, String)
myFunc (a, b, c, d) = (if d then a + d else a - d, b)
```

Tuples with **two elements** are especially common and have standard helpers: `fst`, `snd`, and `swap` (from [Data.Tuple]).

```
ghci> :type fst
fst :: (a, b) -> a

ghci> fst (7, "Hello")
7

ghci> :type snd
snd :: (a, b) -> b

ghci> snd (7, "Hello")
"Hello"

ghci> import Data.Tuple
ghci> :t swap
swap :: (a, b) -> (b, a)

ghci> swap (7, "Hello")
("Hello",7)
```

#### Tuple pattern matching

Tuples can be deconstructed using pattern matching, either in function definitions or `let`/`where` bindings.

```haskell
addFirstTwo :: (Int, Int, Int) -> Int
addFirstTwo (x, y, _) = x + y
```

#### Tuples as product types

Conceptually, tuples are **product types**. A tuple `(a, b)` contains both an `a` and a `b`.

You can model tuples explicitly using `data`:

```haskell
data MyTuple2 a b = XTuple2 a b
data MyTuple3 a b c = XTuple3 a b c
-- ...

myTuple :: MyTuple3 Int String Double
myTuple = XTuple3 7 "Hi" 2.25

addFirstTwo :: MyTuple3 Int Int c -> Int
addFirstTwo (XTuple3 x y _) = x + y
```

What forms the tuple is the `,` operator keyword and used notation in the first example is just a syntactic sugar.

```haskell
myTuple = (,) 7 "Hi"
myTuple3 = (,,) 7 "Hi" 2.25
```

Note: there is a limit on the number of tuple elements (up to 62 in GHC) because each arity requires a separate type constructor. For larger collections, use lists or other data structures from libraries. It is reasonable to use tuples for small fixed-size groupings (2-4 elements), typically ad-hoc groupings of related values.

### Lists: variable length, homogeneous types

A list represents a sequence of values:

* its length can vary,
* all elements have the same type.

```haskell
myIntList :: [Int]
myIntList = [5,8,7,1]
```

Lists are recursive data types. A list is either:

* empty, or
* an element followed by another list.

A simple custom definition makes this explicit:

```haskell
data List a
    = Empty
    | NonEmpty a (List a)
```

A more traditional naming is:

```haskell
data List a
    = Nil
    | Cons a (List a)
```

#### List constructors

Lists have two (data) constructors:

* `[]` (*Nil*) represents the empty list,
* `:` (*Cons*) constructs a new list by prepending an element to an existing list.

Yes, data constructors can be symbolic operators! Basically, the following to definitions are equivalent thanks to *syntactic sugar*:

```haskell
[5, 8, 7, 1]
5 : 8 : 7 : [1]
5 : 8 : 7 : 1 : []
(:) 5 ((:) 8 ((:) 7 ((:) 1 [])))
```

Similarly, the type `[Int]` is just syntactic sugar for `[] Int`. The list type is defined as:

```haskell
data [] a = [] | (:) a ([] a)
```

Lists are simple and expressive, but not always the most efficient data structure. While GHC performs many optimizations, for large-scale or performance-critical applications, we will later explore more advanced data structures from the [containers] library.

#### List pattern matching

Pattern matching is commonly used to deconstruct lists (as any other data type):

```haskell
sumList :: [Int] -> Int
sumList []       = 0
sumList (x : xs) = x + sumList xs
```

### Strings: lists of characters

In Haskell, a `String` is **not a special primitive type**. It is simply a type synonym for a list of characters:

```haskell
type String = [Char]
```

This means that:

* all list operations work on strings,
* strings are immutable sequences of characters.
* string processing follows the same principles as list processing (thus may be inefficient for large texts).

For example:

```haskell
"hello" == ['h', 'e', 'l', 'l', 'o']  -- True
```

#### String functions

Because `String` is just list of `Char`, many functions are shared with lists. In addition, there are some standard functions specialized for textual use, such as:

* `putStr`, `putStrLn`
* `lines`, `words`
* `unlines`, `unwords`

These are provided by the standard library (see [Data.String] and related modules).

#### Simple functions for lists (and strings)

Now that we have introduced basic data types and containers, we can focus on **working with data** using functions.

Lists are a fundamental data structure in Haskell, and many useful operations are already provided by the standard library (see [Data.List]).

Try the following examples in GHCi and inspect their types:

```
ghci> let myList = [2,4,5,3,2,8,4,1]

ghci> head myList
2

ghci> tail myList
[4,5,3,2,8,4,1]

ghci> myList ++ [4,5]
[2,4,5,3,2,8,4,1,4,5]

ghci> myList !! 2
5

ghci> null myList
False

ghci> null []
True

ghci> length myList
8

ghci> reverse myList
[1,4,8,2,3,5,4,2]

ghci> take 2 myList
[2,4]

ghci> drop 2 myList
[5,3,2,8,4,1]

ghci> filter (<6) myList
[2,4,5,3,2,4,1]

ghci> takeWhile (<6) myList
[2,4,5,3,2]

ghci> dropWhile (<6) myList
[8,4,1]

ghci> elem 5 myList
True

ghci> elem 7 myList
False

ghci> zip [1,2,3] [4,5,6]
[(1,4),(2,5),(3,6)]

ghci> map (^2) myList
[4,16,25,9,4,64,16,1]

ghci> all (<6) myList
False

ghci> any (<6) myList
True

ghci> sum myList
29

ghci> or [True, False, True]
True

ghci> and [True, False, True]
False
```

#### Folding (reducing) lists

Folding (also known as reducing) is a powerful technique for processing lists. It involves recursively combining the elements of a list using a binary function and an initial accumulator value.

```
ghci> foldl (+) 0 myList
29

ghci> foldl (||) False [True, False, True]
True

ghci> foldl (&&) True [True, False, True]
False
```

Here, `foldl` (left fold) generalizes operations such as `sum`, `and`, and `or`.

We will study folds in more detail later when discussing recursion schemes (catamorphisms). For now, it is enough to recognize them as a powerful abstraction over list processing.

## Task assignment

For the second assignment, navigate to the `hw02` project and follow the instructions in the `README.md` file there. It will test your skills in using basic functions and types.

## Further reading

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com)
* [School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell)

[containers]: http://hackage.haskell.org/package/containers
[Data.List]: hackage.haskell.org/package/base/docs/Data-List.html
[Data.String]: hackage.haskell.org/package/base/docs/Data-String.html
[Data.Tuple]: hackage.haskell.org/package/base/docs/Data-Tuple.html
