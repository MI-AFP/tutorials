# Functions and basic data types

## General summary of syntax

### Haskell keywords

There are some [reserved keywords/operators] which have some special meaning in Haskell:

1. operator-like: `!`, `'`, `"`, `-`, `--`, `-<`, `-<<`, `->`, `::`, `;`, `<-`, `,`, `=`, `=>`, `>`, `?`, `#`, `*`, `@`, `\`, `_`, `` ` ``, `|`, `~`
2. brackets-like: `[| |]`, `{ }`, `{- -}`
3. keywords (common): `as`, `case of`, `class`, `data`, `deriving`, `do`, `hiding`, `if then else`, `import`, `infix infixr infixl`, `instance`, `let in`, `module`, `newtype`, `qualified`, `type`, `where`
4. keywords (not so common): `data family`, `data instance`, `default`, `deriving instance`, `forall`, `foreign`, `mdo`, `proc`, `rec`, `type family`, `type instance`

[reserved keywords/operators]: https://wiki.haskell.org/Keywords

### Type signature

As was in the previous lesson, you can specify type of any expression directly or by it's name via `::` operator-like keyword "has-type". It is mandatory only in case of ambiguity because Haskell's type system has type inference (it can analyze what type the expression should have). Type of expression cannot change - it is static typing.

```haskell
a :: Integer
b :: String

a = 42
b = a
c = (a + 7) :: Double
```

Haskell does not have implicit conversion and if you do something like it is in example above, you will get an error during compilation. Type system is very strict and helps programmers to find more bugs during compile time and not during runtime (where is more problematic to find it).

```
    Couldn`t match type ‘Integer’ with ‘[Char]’
    Expected type: String
      Actual type: Integer
    In the expression: a
    In an equation for ‘b’: b = a
```

### Type variable

As you could have notice, we are able to achieve polymophism with something strange in function types, something called type variable. Type variables must start with lowercase letter and usually are just 1 character from the beginning of alphabet:

```haskell
identity :: a -> a
identity x = x
```

Such type signature tells us that `identity` is a function from whatever type `a` to the same type `a`.

### Type constraints

In some cases (function type and others) you can see typeclass constraints like here:

```haskell
next :: Num a => a -> a
next x = x + 1
```

It says `next` has a type `a` to `a` where `a` is "from" typeclass `Num` (typeclasses will be covered later, for now it is just class or types). Such type signature can be even more complex.

```haskell
foo :: (Show a, Eq a, Read b) => a -> b -> a
```

### Function declaration

Although type can be in most cases inferred automatically by compiler, it is good practice to write it down at least in case of functions as part of code documentation. Functions can be complicated and by reading its type signature you know immediately what arguments it expects and what it returns.

Declaration of function is simple, just use the prefix notation as you would call the function and then describe what is it equal to.

```haskell
fibonacci   :: Word -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = (fibonacci n-1) + (fibonacci n-2)
```

### Type declaration

There are three basic way how to introduce your own data type:

1. type synonym = you just use different name for some existing type (for example `String` is type synonym for `[Char]` = list of `Char`)
2. new data type = declare type with type constructor (before `=`) and one ore more data constructors (after `=`, separated by `|`), you may use typeclass constraints, type variables and recursion
3. newtype = new data type with exactly one data constructor with one parameter (new type is isomorphic with the "wrapped" type and compiler can do optimizations, can be used also in other way in more advanced code)

```haskell
type String = [Char]

data Bool = True | False
data Tree a = Leaf a | Branch (Tree a) (Tree a)
            deriving (Show, Read)

newtype Age = Age Int

myTree :: Tree Int
myTree = Branch (Leaf -7) (Branch (Leaf 7) (Leaf 10))

myAge :: Age
myAge = Age 20
```

## Data types

Haskell has strong static type system which is one of the things making it so great. As we already saw, every expression in Haskell has some type and the type cannot change during runtime (that is the difference with dynamic typing). As in other programming languages can use predefined data types, get more from some libraries or introduce your own.

### Basic Data Types

* `Int` = A fixed-precision integer type with at least the range `[-2^29 .. 2^29-1]` (exact range can vary based on implementation, it can be check with `minBound` and `maxBound`)
* `Integer` = Arbitrary-precision integers (e.g. theoretically unlimited)
* `Float` = Single-precision floating point numbers
* `Double` = Double-precision floating point numbers
* `Word` = Unsigned integral number (same size as `Int`)
* `Char` = Unicode character (ISO/IEC 10646)
* `Bool` = truth value, only `True` or `False`
* `String` = literally list of characters

### Type and data constructor

Get back to creating own data types with the `data` keyword. After `data` is the type construction starting with capital letter and then there might be type parameters (type variables). Then `=` follows and after it we can specify multiple data constructors separated by `|`. Each data constructor again starts with capital letter and can be followed by data types which it takes as arguments.

```haskell
data MyType a b = MyTypeC1 a Int | MyTypeC2 String b | MyType3
                deriving Show  -- will be covered later

x :: MyType Bool Float
x = MyTypeC1 True 10
```

Usually when there is just one data constructor, the name is the same as of the type constructor (but it is not a rule).

### Record types

Imagine you want to create a type for `Person` which contains `name`, `age`, `gender`, and `city`. You would need to do:

```haskell
data Gender = Male | Female
            deriving Show

data Person = Person String Int Gender String
            deriving Show

name :: Person -> String
name (Person x _ _ _) = x

age :: Person -> Int
age (Person _ x _ _) = x

gender :: Person -> Gender
gender (Person _ _ x _) = x

city :: Person -> String
city (Person _ _ _ x) = x
```

Not very nice, right?! And we have just 4 attributes of a person. Luckily there is syntactic sugar which make it easier for us called record:

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

Now try to create a type `Pet` which also contains `name` and `age`. You will get an error which is logical, you cannot have two functions with the same name! One option is to rename it to `namePerson` and `namePet`, the second is available to you only if you have GHC 8.0.1 or higher and it is with language extension [DuplicateRecordFields](https://downloads.haskell.org/~ghc/master/users-guide/glasgow_exts.html#duplicate-record-fields):

```haskell
{-# LANGUAGE DuplicateRecordFields #-}

data Gender = Male | Female
            deriving Show

data Person = Person
            { name   :: String
            , age    :: Int
            , gender :: Gender
            , city   :: String
            } deriving Show

data Pet = Pet
         { name   :: String
         , age    :: Int
         } deriving Show

olderPet :: Pet -> Pet
olderPet pet = pet { age = (age pet + 1) }
```

You can also see that there is a shorthand for updating the value of record - creating new edited record from previous.

### Algebraic Data Types (ADTs)

We say that our datatypes are algebraic in Haskell because it allows us to create sum and product types (with `data`), type aliases (with `type`), and special types (with `newtype`).

* Sum = alternation with `|` (`A | B` -> A + B)

```haskell
data Number = I Int | D Double   -- Int + Double
```

* Product = combination of types (`A B` -> A * B)

```haskell
data Pair = P Int Double         -- Int * Double
```

## List and tuple

There are two basic container types (has ability to store multiple values) - tuples and lists. Of course there are many more such as maps, sets, vectors, streams defined in some libraries or you can create your own but these are real the basic and widely used.

### Tuple

Tuple has a fixed number of elements of fixed types. You always know how many elements are in the tuple and what type is it. Type of elements in a tuple can differ.

```haskell
myTuple :: (Int, String, Bool, Double)
myTuple = (15, "String", True, 5.24)

myFunc :: (Int, String, Bool, Double) -> (Double, String)
myFunc (a, b, c, d) = (if d then a + d else a - d, b)
```

There are basic functions for tuples with two elements: `fst`, `snd`, and `swap`.

```
Prelude> :type fst
fst :: (a, b) -> a
Prelude> fst (7, "Hello")
7
Prelude> :type snd
snd :: (a, b) -> b
Prelude> snd (7, "Hello")
"Hello"
Prelude> import Data.Tuple
Prelude Data.Tuple> :t swap
swap :: (a, b) -> (b, a)
Prelude Data.Tuple> swap (7, "Hello")
("Hello",7)
```

Good to know is how it actually works and try to implement own tuples.

```haskell
data MyTuple2 a b = XTuple2 a b
data MyTuple3 a b c = XTuple3 a b c
-- ...

myTuple :: MyTuple3 Int String Double
myTuple = XTuple3 7 "Hi" 2.25
```

What forms the tuple is the `,` operator keyword and used notation in first example is just a syntactic sugar.


```haskell
myTuple = (,) 7 "Hi"
```

### List

List is different than tuples - it has variable length (because it is recursive type) and its elements have the same type. To understand it, let's create an alternative implementation of list data type.

```haskell
data List a = Empty | NonEmpty a (List a)
```

That's it! List of type `a` it either `Empty` or `NonEmpty` which means that it has an element and then rest of the list (which can be again `Empty` or `NonEmpty`). Sometimes the following naming is used:

```haskell
data List a = Nil | Cons a (List a)
```

It is because with `Cons` you join element with the other list. List in Haskell is very similar, just for `Nil` you use empty list `[]` and for joining the infix cons operator `:`.

```haskell
myIntList :: [Int]
myIntList = [5,8,7,1]
myIntList = 5:8:7:[1]
myIntList = 5:8:[7,1]
-- ...
-- infix cons
myIntList = 5:8:7:6:1:[]
-- prefix cons
myIntList = (:) 5 ((:) 8 ((:) 7 ((:) 1 [])))
```

Actually `[5,8,7,6,1]` is a syntactic sugar for `5:8:7:6:1:[]` and even for the prefix. Same goes for the type, when you write `[Int]` it actually means `[] Int`. You can rewrite the actual list to:

```haskell
-- data [a] = [] | a:[a]
data [] a = [] | (:) a ([] a)
```

### String

String is really nothing but just list of characters `[Char]`. Only difference is that there are more functions for working specially with `String`s - like `putStr`, `lines`, `words` and more.

## Simple functions

Enough of types and containers, let's do some functions when this is functional programming course!

### Basic list functions

### Intro to patterns

### Recursion

## Task assignment

## Further reading

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com)
* [School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell)
