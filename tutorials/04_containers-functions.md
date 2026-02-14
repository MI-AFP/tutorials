# Containers and higher-order functions

In the previous lectures, we learned how to define functions, use pattern matching, guards, and work with basic types.

In this lesson, we move one level higher:

* from individual values to containers of values,
* from simple functions to functions that manipulate other functions,
* and from programming intuition to a lightweight mathematical understanding of what is happening.

The goal is not category theory though. The goal is to see that Haskell programs are built from well-structured mathematical ideas — in a way that remains practical for software engineers

## Important "base" types (revisited)

We already know the basic data types from the `base` package such as:

* [`Bool`](https://hackage.haskell.org/package/base/docs/Data-Bool.html)
* numeric types ([`Int`](https://hackage.haskell.org/package/base/docs/Data-Int.html), [`Integer`](https://hackage.haskell.org/package/base/docs/Data-Integer.html), [`Double`](https://hackage.haskell.org/package/base/docs/Data-Double.html), ...)
* [`Char`](https://hackage.haskell.org/package/base/docs/Data-Char.html)
* [lists](https://hackage.haskell.org/package/base/docs/Data-List.html) `[a]`
* [tuples](https://hackage.haskell.org/package/base/docs/Data-Tuple.html) `(a, b)`

These are concrete types.

Now we move toward something more powerful: parameterized types and algebraic data types (ADTs).

Mathematically, you can think of a type as a set of values.

* `Bool` ≅ { `True`, `False` }
* `()` ≅ { `()` } (exactly one value)
* `[Int]` ≅ the set of all finite sequences of integers

An algebraic data type builds new types from old ones using:

* **sum** types (choice: *either this or that*)
* **product** types (combination: *this and that*)

This time, we start with a fundamental example of a sum type.

### Maybe = optional value

In many programming languages, there exists a special value like: `null`, `nil`, or `None` that is used to represent the absence of a value. Unfortunately, they often lead to runtime errors like:

> Null pointer exception

Haskell avoids this design. Instead of having a universal "invalid" (or better "no") value, Haskell encodes optionality *in the type itself*.

If we were designing this idea manually, we might write:

```haskell
data IntOrNull = I Int | NullInt
data StringOrNull = S String | NullString
```

But this does not scale. So we generalize the idea:

```haskell
data ValueOrNull a = Value a | Null
```

This is a **polymorphic sum type**: for any type `a`, a value is either:

* a real value of type `a`, or
* the special constructor `Null`.

Haskell already provides this abstraction, called `Maybe`, in the [`Data.Maybe`](https://hackage.haskell.org/package/base/docs/Data-Maybe.html) module:

```haskell
data Maybe a = Nothing | Just a
```

Mathematically, you can think of it as:

```math
\text{Maybe}(A) = A \cup \{\bot\}
```

That is: all values of `A`, plus one extra distinguished value `⊥` (pronounced "bottom") that represents the absence of a value.

This means:

* `Just x` represents a valid value,
* `Nothing` represents absence of value.

Let's take a look at an example:

```haskell
saveDiv :: Int -> Int -> Maybe Int
saveDiv _ 0 = Nothing
saveDiv x y = Just (x `div` y)
```

Notice that the possibility of failure is now visible in the type signature. The caller of `saveDiv` must now handle the case of `Nothing`, which forces them to consider the possibility of failure and prevents runtime errors.

That is a key idea in functional programming:

> We encode computational effects (like failure) in types.

As you can see from the following example, use of `safeDiv` forces the caller to handle the possibility of failure:

```haskell
divString :: Int -> Int -> String
divString x y = case saveDiv x y of
    Nothing -> "Division by zero is not allowed."
    Just result -> "The result is: " ++ show result
```

### Either

`Maybe` distinguishes between "something" and "nothing". It models *absence*, but not *information about absence*. That can be sufficiant in some cases, but in others, we want to know *why* something is absent. For that, we can use the `Either` type:

```haskell
data Either a b = Left a | Right b
```

Mathematically, you can think of it as a disjoint union of two sets (sum type):

```math
\text{Either}(A, B) = A \sqcup B
```

A value of type `Either a b` is either:

* a value of type `a` wrapped in the `Left` constructor, or
* a value of type `b` wrapped in the `Right` constructor.

Importantly, the two *sides* may have different types (but don't have to). This allows us to encode more information about the nature of the value. For example, we can use `Either String Int` to represent a computation that can either fail with an error message (a `String`) or succeed with an integer result.

By convention, `Left` is often used to represent an error or failure case, while `Right` is used to represent a success case. However, this is just a convention but a very strong and common one.

Let's refine the previous simple division example to use `Either` instead of `Maybe`:

```haskell
saveDiv :: Int -> Int -> Either String Int
saveDiv _ 0 = Left "Division by zero is not allowed."
saveDiv x y = Right (x `div` y)
```

Again, the caller of `saveDiv` must now handle both cases, but they also get more information about the nature of the failure:

```haskell
divString :: Int -> Int -> String
divString x y = case saveDiv x y of
    Left errorMsg -> errorMsg
    Right result -> "The result is: " ++ show result
```

If we would like to express `Maybe` in terms of `Either`, we can do it like this:

* `Maybe a` ≅ `Either () a`
* `Nothing` ≅ `Left ()`
* `Just x` ≅ `Right x`

This brings us to the next topic: the "unit" type, in other words what is `()`.

### Unit

Earlier we said that Haskell does not have `null`, `nil`, or `None`. However, it does have a special type called **Unit**:

```haskell
data () = ()
```

It defines a type `()` that has exactly one value, also written `()`. This type is used when we want to indicate that there is no meaningful value to return. It is often used as the return type of functions that perform side effects (like printing to the console) but do not return any meaningful value.

Mathematically, you can think of it as a singleton set (a set containing exactly one element):

```math
\text{Unit} = \{()\} \cong \{\ast\}
```

This is more comparable to `void` in languages like C or Java, but it is a proper (but trivial) type with a single possible value, which allows us to use it in more flexible ways. For example, we can use `Either () String` to represent a computation that can either fail with no information (the `Left ()` case) or succeed with a string result (the `Right String` case).


## Other containers

In programming (and in mathematics), we often work not only with individual values, but with collections of values.

Haskell already gives us one extremely important container: the list `[a]`. Lists are simple, expressive, and work very well for many problems — especially when you process data sequentially. However, lists are not always the best choice.

Depending on what you need, you might prefer containers with:

* faster lookup,
* faster concatenation,
* uniqueness of elements (sets),
* key-value mapping (maps),
* tree/graph structure.

Most of the common immutable containers are provided by the package: [`containers`](https://hackage.haskell.org/package/containers). But there are more such as [`array`](https://hackage.haskell.org/package/array), [`vector`](https://hackage.haskell.org/package/vector), [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers), etc. You can always check the [Hackage](https://hackage.haskell.org/) for more.

It is very common, that such containers use similar interfaces, so you can easily switch between them without changing much of your code. We will see that the naming of functions is often the same. For example, `Data.Map` and `Data.Set` have functions like `fromList`,`insert`, `delete`, `member`, etc. that work in a similar way.

### Sequence (`Seq`)

The type `Seq a` from [`Data.Sequence`](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html) represents a **finite sequence** of values of type `a`.

You can think of it as a *better list* in situations where you need:

* fast insertion at both ends,
* efficient concatenation,
* efficient splitting, or
* random access.

Compared to lists:

* lists are optimized for prepending (x : xs)
* sequences are optimized for both ends (<| and |>)

```
ghci> import Data.Sequence

Prelude Data.Sequence> seq1 = 1 <| 2 <| 15 <| 7 <| empty

Prelude Data.Sequence> seq1
fromList [1,2,15,7]

Prelude Data.Sequence> 3 <| seq1
fromList [3,1,2,15,7]

Prelude Data.Sequence> seq1 |> 3
fromList [1,2,15,7,3]

Prelude Data.Sequence> seq1 >< fromList [2,3,4]
fromList [1,2,15,7,2,3,4]

Prelude Data.Sequence> sort seq1
fromList [1,2,7,15]
```

### Set (`Set`)

The type `Set a` from [`Data.Set`](https://hackage.haskell.org/package/containers/docs/Data-Set.html) represents a mathematical set:

* elements are unique,
* order is not important conceptually,
* membership testing is efficient.

Most operations require `Ord a`, which means that the elements must be orderable (this is because `Set` is implemented as a balanced binary tree).

```
ghci> import Data.Set

Prelude Data.Set> set1 = insert 4 $ insert 2 $ insert 0 $ singleton 2

Prelude Data.Set> set1
fromList [0,2,4]

Prelude Data.Set> delete 2 set1
fromList [0,4]

Prelude Data.Set> member 4 set1
True

Prelude Data.Set> member (-6) set1
False

Prelude Data.Set> Data.Set.filter (>3) set1
fromList [4]

Prelude Data.Set> set2 = insert 5 (insert 3 (singleton 2))

Prelude Data.Set> intersection set1 set2
fromList [2]

Prelude Data.Set> union set1 set2
fromList [0,2,3,4,5]
```

There is also a specialized version of `Set` for integers: [`Data.IntSet`](https://hackage.haskell.org/package/containers/docs/Data-IntSet.html), which is more efficient but only works with `Int` values (faster than `Set Int`).

### Map (`Map`)

The type `Map k v` represents a finite mapping:

* from keys of type `k`
* to values of type `v`

This is also known as:

* dictionary,
* associative array,
* key-value store.

Maps are implemented also as balanced binary trees, so most operations require `Ord k` (the keys must be orderable).

There are two main variants:

* `Data.Map.Lazy` (values are lazy, default)
* `Data.Map.Strict` (values are evaluated eagerly)

```
ghci> import Data.Map

Prelude Data.Map> map1 = insert "suchama4" "Marek Suchanek" (singleton "perglr" "Robert Pergl")

Prelude Data.Map> map1 ! "suchama4"
"Marek Suchanek"

Prelude Data.Map> map1 !? "suchamar"
Nothing

Prelude Data.Map> map1 !? "suchama4"
Just "Marek Suchanek"

Prelude Data.Map> size map1
2

Prelude Data.Map> delete "suchama4" map1
fromList [("perglr","Robert Pergl")]
```

Two important lookup operators:

* `(!)` throws an exception if the key is missing (unsafe),
* `(!?)` returns `Maybe v` (safe) = same as `Map.lookup` function and should be preferred in most cases.

There is again a specialized version of `Map` for integer keys: [`Data.IntMap`](https://hackage.haskell.org/package/containers/docs/Data-IntMap.html), which is more efficient but only works with `Int` keys (faster than `Map Int v`).

### Tree and Graph

The containers package also provides generic data structures for:

* trees: [`Data.Tree`](https://hackage.haskell.org/package/containers/docs/Data-Tree.html)
* graphs: [`Data.Graph`](https://hackage.haskell.org/package/containers/docs/Data-Graph.html)

You typically use them when:

* you want to model hierarchical data (tree),
* you want to model dependencies or connectivity (graph),
* you want a standard, well-tested implementation.

They are especially useful because they integrate nicely with the rest of the ecosystem.

### Summary of `containers`

The `containers` package provides a rich set of data structures that are essential for functional programming in Haskell. They allow us to work with collections of data in a way that is efficient, expressive, and integrates well with the functional programming paradigm.

* Use **lists** `[a]` for simple, sequential data processing.
* Use **sequences** `Seq a` for efficient access and modification at both ends.
* Use **sets** `Set a` for collections of unique elements with efficient membership testing.
* Use **maps** `Map k v` for key-value associations with efficient lookup.
* Use **trees** and **graphs** for hierarchical and relational data structures.
* Use `IntSet` and `IntMap` for optimized performance when working with integer keys.


## Functions as first-class values

So far, we have been talking about data:

* simple types (`Int`, `Bool`, `()`)
* algebraic types (`Maybe`, `Either`)
* containers (`List`, `Seq`, `Set`, `Map`)

But in functional programming, the real power lies not only in *what* we store — but in *how we transform it*.

In imperative languages, functions are often secondary while data is central, and functions manipulate it. In Haskell (and FP), functions are *first-class values* that can be passed around, stored in data structures, and returned from other functions. This allows us to write highly abstract and reusable code.

That is a key idea in functional programming and consequence of the mathematical idea that:

> A function is just another value - an element of a function space.

We already know that functions written in mathematics like:

```math
f: A \to B
```

can be represented in Haskell as:

```haskell
f :: A -> B
```

But `->` is not just a notation for function types. It is also a type constructor that takes two types `A` and `B` and produces a new type `A -> B`, which is the type of functions that take an argument of type `A` and return a value of type `B`. Just like `(A, B)` constructs a product type (tuple) or `Either A B` constructs a sum type (either `A` or `B`),  `A -> B` constructs a function type (functions from `A` to `B`).

`(A -> B)` is a type, and values of this type are functions.

### Currying

Let's consider a function of two arguments. In mathematics there are two common and equivalent ways to write it:

First, as a function on pairs (tuples):

```math
f: A \times B \to C
```

This one takes a pair of values (a tuple) and returns a value of type `C`. In Haskell, we can write it as:

```haskell
f :: (a, b) -> c
```

Second, as a curried function:

```math
f: A \to (B \to C)
```

This one takes a value of type `A` and returns a function of type `B -> C`, which takes a value of type `B` and returns a value of type `C`. In Haskell, we can write it as:

```haskell
f :: a -> b -> c
```

In Haskell, all functions are curried by default, which means that they are of the second form. The first form can be achieved using tuples, but it is not the default and is not as common. If you would wonder why *curry*, it is named after the mathematician [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry), who developed the technique of currying in the context of combinatory logic.

If we now look at `->` as a type constructor, we can see that it is right-associative, which means that `a -> b -> c` is the same as `a -> (b -> c)`. That is why all functions in Haskell are curried by default.

```
ghci> let myFunc x y z = x * y + z

ghci> :type myFunc
myFunc :: Num a => a -> a -> a -> a

ghci> :type (myFunc 8)
(myFunc 8) :: Num a => a -> a -> a

ghci> :type (myFunc 8 7)
(myFunc 8 7) :: Num a => a -> a

ghci> :type (myFunc 8 7 1)
(myFunc 8 7 1) :: Num a => a
```

Each application consumes one argument and returns another function — until fully applied.

### Partial application

This behavior gives us something extremely powerful:

> We can supply only some arguments and obtain a new function.

```haskell
myFunc2 :: Num a => a -> a
myFunc2 = myFunc 8 7
```

In simple terms: we partially apply `myFunc` to the first two arguments (`8` and `7`) and get a new function `myFunc2` that takes one argument and adds it to the product of `8` and `7`.

Mathematically, you can think of it as:

```math
f: A \to B \to C
```

then partially applied:

```math
f(a): B \to C
```

and then:

```math
\left(f(a)\right)(b): C
```

When we partially apply a function, Haskell internally remembers the arguments we supplied. This is called (and may be known also from other programming languages) a **closure**. It is important to understand that a closure it not something magical or special. It is just a function that has some of its arguments fixed. The fact that it can remember those arguments is just a consequence of how functions work in Haskell and FP. Closures are one of the fundamental building blocks of functional architecture.

### `curry` and `uncurry`

Haskell provides two standard functions for converting between curried and uncurried forms:

```haskell
curry :: ((a, b) -> c) -> (a -> b -> c)
uncurry :: (a -> b -> c) -> ((a, b) -> c)
```

```
ghci> let addPair (x, y) = x + y

ghci> :type addPair
addPair :: Num a => (a, a) -> a

ghci> (curry addPair) 1 5
6

ghci> :type uncurry (+)
uncurry (+) :: Num a => (a, a) -> a
```

Mathematically, this corresponds to the equivalence:

```math
\text{curry} : (A \times B \to C) \to (A \to (B \to C))
```

```math
\text{uncurry} : (A \to (B \to C)) \to (A \times B \to C)
```
This equivalence is fundamental in lambda calculus. It shows that there is no loss of generality in using curried functions, and that we can always convert between the two forms as needed.

### Higher-order functions (HOFs)

A higher-order function (HOF) is a function that:

* takes a function as an argument, and/or
* returns a function as a result.

Example of a function that takes a function as an argument:

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

result = applyTwice (+1) 5
```

Example of a function that returns a function as a result:

```haskell
makeAdder :: Int -> (Int -> Int)  -- same as: Int -> Int -> Int
makeAdder x = \y -> x + y

add5 :: Int -> Int
add5 = makeAdder 5
```

In pure **lambda calculus**, all functions are higher-order. There are no primitive data types — only functions.

For example, a lambda abstraction

```haskell
\x -> x + 1
```

corresponds directly to:

```math
\lambda x. x + 1
```

In fact, currying, partial application, and composition all emerge naturally from lambda calculus. Naturally, Haskell’s function system is directly inspired by lambda calculus.


### Function composition

One of the central architectural ideas in functional programming is:

> Build programs by composing small functions.

This is not metaphorical. It is literally mathematical **function composition** which you may know very well from math. If we have two functions:

```math
f: A \to B
g: B \to C
```

Then we can compose them to get a new function:

```math
g \circ f: A \to C
```

where

```math
(g \circ f)(a) = g(f(a))
```

And in Haskell, we can do the same thing using the `(.)` operator:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

Notice that the order of arguments is reversed compared to the mathematical notation. In Haskell, `g . f` means "first apply `f`, then apply `g` to the result". This is a common convention in programming languages, where the composition operator is often defined in a way that allows for more natural reading from left to right. It is also right-associative, which means that `f . g . h` is the same as `f . (g . h)`. In this way, you can compose multiple functions together without needing parentheses (forming a **pipeline**).

Also, the type signature of `(.)` reflects the fact that it takes two functions and returns a new function that is their composition. The operator `(.)` is thus itself a **higher-order function**.

```
ghci> :type (5+)
(5+) :: Num a => a -> a

ghci> :type (5*)
(5*) :: Num a => a -> a

ghci> :type show
show :: Show a => a -> String

ghci> show ( (5+) ( (5*) 5 ) )
"30"

ghci> (show . (5+) . (5*)) 5
"30"
```

Notice that function composition works only with functions that have compatible types and a single argument. If you want to compose functions with multiple arguments, you can use `curry` and `uncurry` to convert them to the appropriate form.

### Function application

Function application in Haskell has the **highest precedence**.

That means:

```haskell
foo 5 + 3
```

is parsed as:

```haskell
(foo 5) + 3
```

There is a defined operator for function application: `($)`, which is right-associative and has the **lowest precedence**. It is defined as:

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

This becomes useful when you want to avoid parentheses:

```
ghci> (show . (5+) . (5*)) 5
"30"

ghci> show . (5+) . (5*) $ 5
"30"
```

### The "point-free" style

In functional programming, it is common to define functions *without explicitly naming their arguments*. This is known as **point-free style** (or tacit programming). It emphasizes the composition of functions rather than the manipulation of data.

Compare the two definitions of the same function:

```haskell
add1 :: Num a => a -> a
add1 x = x + 1

-- point-free style
add1' :: Num a => a -> a
add1' = (+1)
```

Both mean the same thing, but the second version is more declarative:

* it describes what the function is,
* not how it receives its input.

The name comes from mathematics: a point-free definition does not explicitly mention the “points” (values) of the domain. (It is not related to the "dot" operator for composition, but it is often used together with it.)

Point-free style is great when:

* the function is a clean composition,
* it improves readability,
* it avoids boilerplate.

But it can become unreadable if overused. A good rule of thumb: *Use point-free style when it makes the code clearer, not when it makes it shorter*.

### Operators, associativity and precedence

When reading Haskell code, it is important to understand **how expressions are parsed**. There are two main rules that determine evaluation structure:

1) Function application has the highest priority, always.
2) Infix operators have precedence and associativity (called *fixity*).

Every infix operator in Haskell has a **fixity declaration** containing associativity (no, left, right) and precedence information:

```haskell
infix  n operator -- non-associative
infixl n operator -- left-associative
infixr n operator -- right-associative
```

where `n` is an integer from 0 to 9 (inclusive) that determines precedence. Higher numbers bind more tightly. The default precedence is 9. Any infix operator without an explicit fixity declaration is left-associative with precedence 9 (`infixl 9`).

We can easily inspect the fixity of operators using `:info` in GHCi:

```
ghci> :info (+)
class Num a where
  ...
  (+) :: a -> a -> a
  infixl 6 +
```

For widely known operators, we can also take a look at the table:

| Precedence | Associativity | Example operators |
|------------|---------------|-------------------|
| 9          | right         | `.`               |
| 8          | right         | `^`, `^^`, `**`   |
| 7          | left          | `*`, `/`, `mod`, `div` |
| 6          | left          | `+`, `-`          |
| 5          | right         | `++`, `:`         |
| 4          | non-          | `==`, `/=`, `<`, `>`, `<=`, `>=` |
| 3          | right         | `&&`              |
| 2          | right         | `||`              |
| 1          | left          | `>>=`, `>>`       |
| 1          | right         | `=<<`             |
| 0          | right         | `$`               |

To see this in practice:

```
ghci> 1 + 2 * 3
7

ghci> (1 + 2) * 3
9

ghci> 1 + 2 * 3 ^ 4
163
```

With non-associative operators, you must use parentheses to disambiguate:

```
ghci> 1 == 2 == 3
<interactive>:1:1: error:
    • Non type-variable argument in a constraint ‘Num (a -> a)’
      (Use UndecidableInstances to permit this)
    • When checking the inferred type
        it is a function of type Num (a -> a) => a -> a
```

### Custom operators

Haskell allows you to define your own infix operators. You can choose any combination of symbols (except for reserved ones) and assign them a fixity. For example:

```haskell
(><) :: [a] -> [a] -> [a]
xs >< ys = reverse xs ++ reverse ys
```

```
ghci> "abc" >< "xyz" >< "klm"
"xyzabcmlk"

ghci> ("abc" >< "xyz") >< "klm"
"xyzabcmlk"

ghci> "abc" >< ("xyz" >< "klm")
"cbaklmxyz"

ghci> "abc" >< "xyz" ++ "klm"
"cbazyxklm"

ghci> "klm" ++ "abc" >< "xyz"
"klmcbazyx"
```

We can easily change fixity and if the new precedence is lower, then `(++)` will be done first. If the precedence is the same, then it is applied in "natural" order (thus, it must have the same associativity, otherwise you get an error).

```haskell
infixl 5 ><
(><) :: [a] -> [a] -> [a]
(><) xs ys = reverse xs ++ reverse ys

infixl 2 >-<
(>-<) :: [a] -> [a] -> [a]
xs >-< ys = reverse xs ++ reverse ys

foo :: Int -> Int -> Int
x `foo` y = 2*x + y
```

```
ghci> :info (><)
(><) :: [a] -> [a] -> [a]       -- Defined at test01.hs:3:1
infixl 5 ><

ghci> "abc" >< "xyz" ++ "klm"

<interactive>:6:1: error:
    Precedence parsing error
        cannot mix `><' [infixl 5] and `++' [infixr 5] in the same infix expression

ghci> ("abc" >< "xyz") ++ "klm"
"cbazyxklm"

ghci> :info (>-<)
(>-<) :: [a] -> [a] -> [a]      -- Defined at test01.hs:7:1
infixl 2 >-<

ghci> "abc" >-< "xyz" ++ "klm"
"cbamlkzyx"

ghci> "klm" ++ "abc" >-< "xyz"
"cbamlkzyx"
```

For operators you can use *symbol characters* as being any of `!#$%&*+./<=>?@\^|-~:` or "any *non-ascii* Unicode symbol or punctuation". But, an operator symbol starting with a colon `:` is a constructor. That means that it can only be used in data type definitions and pattern matching, but not in expressions. For example:

```haskell
data MyList a = Nil | Cons a (MyList a)

infixr 5 `Cons`

data MyTuple2 a b = :@ a b

infix 6 :@
```


## Folds

You may have heard of **Map/Reduce** — a powerful programming model for processing large data sets. It consists of two main steps. We already know `map`. The *reduce* part exists in Haskell too — but it is traditionally called a **fold**. A fold processes a container (usually a list) and accumulates a result.

Intuitively, it takes:

1) an initial value (the "zero" or "identity" for the operation),
2) a binary function that combines an element of the container with the accumulated result,
3) a container of elements (typically a list).

Then, a fold replaces the (list) structure with repeated application of the binary function, starting with the initial value.

### Right fold (`foldr`)

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

The `foldr` function takes a binary function, an initial value, and a list. It processes the list from the right, applying the function to each element and the accumulated result.

Think of it as:

```haskell
foldr f z [x1, x2, x3] = f x1 (f x2 (f x3 z)) = x1 `f` (x2 `f` (x3 `f` z))
```

### Left fold (`foldl`)

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
```

The `foldl` function is similar to `foldr`, but it processes the list from the left. It applies the function to the accumulated result and each element in order.

Think of it as:

```haskell
foldl f z [x1, x2, x3] = f (f (f z x1) x2) x3 = ((z `f` x1) `f` x2) `f` x3
```

### Own fold

We can easily implement our own fold function using recursion. For example, here is a simple implementation of `foldr`:

```haskell
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs
```

### Scans

In addition to folds, Haskell also provides **scans**, which are similar but return a list of all intermediate results instead of just the final accumulated value.

```haskell
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
```

```
ghci> scanl (-) 0 [1..5]
[0,-1,-3,-6,-10,-15]

ghci> scanr (-) 0 [1..5]
[-3,4,-2,3,-1,5,0]
```

### Strict folds

The standard `foldl` is not strict, which means that it can lead to building up large thunks (deferred computations) that can cause memory issues. To avoid this, Haskell provides a strict version of `foldl` called `foldl'` in the `Data.List` module:

```haskell
foldl' :: (b -> a -> b) -> b -> [a] -> b
```

## Task assignment

For the assignment, navigate to the `hw04` project and follow the instructions in the `README.md` file there. It will test your skills in working with containers and more complex functions.

## Further reading

* [Haskell containers](https://haskell-containers.readthedocs.io/en/latest/)
* [Haskell - Handling errors in Haskell](https://wiki.haskell.org/Handling_errors_in_Haskell)
* [Haskell - error](https://wiki.haskell.org/Error)
* [8 ways to report errors in Haskell](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)

[containers]: https://hackage.haskell.org/package/containers
[GHC]: https://www.haskell.org/ghc/
[Hackage]: https://hackage.haskell.org
[Hoogle]: https://www.haskell.org/hoogle/
