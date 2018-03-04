# Advanced functions and intro to typeclasses

## More about functions

Creating new own functions or using the predefined from libraries is quite simple. But there are more interesting things you can do with functions. This is functional programming and all is about functions!

### Currying

When we talk about "currying" in Haskell it has (almost) nothing to do with dishes or spices. Famous mathematician and logician [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry) (the language is named after him) developed with others technique called currying: translating the evaluation of a function that takes multiple arguments (or a tuple of arguments) into evaluating a sequence of functions, each with a single argument. To better understand this, let's focus how we parenthesise function types:

```haskell
myFunc1 :: a ->  b -> c
myFunc1 :: a -> (b -> c)
```

We see that `myFunc1` takes value of `a` and returns a function that takes value of `b` and result is a value of `c`. It is possible to apply value of `a` and "store" the function `b -> c` for later application or reuse:

```
Prelude> let myFunc x y z = x * y + z
Prelude> :type myFunc
myFunc :: Num a => a -> a -> a -> a
Prelude> :type (myFunc 8)
(myFunc 8) :: Num a => a -> a -> a
Prelude> :type (myFunc 8 7)
(myFunc 8 7) :: Num a => a -> a
Prelude> :type (myFunc 8 7 1)
(myFunc 8 7 1) :: Num a => a
Prelude> myFunc 8 7 1
57
Prelude> myFunc2 = myFunc 8 7
Prelude> myFunc2 1
57
Prelude> myFunc2 2
58
```

In Haskell there are two basic functions called `curry` and `uncurry` which work with functions:

```
Prelude> :type curry
curry :: ((a, b) -> c) -> a -> b -> c
Prelude> let myFunc (x, y) = x + y
Prelude> :type myFunc
myFunc :: Num a => (a, a) -> a
Prelude> myFunc (1, 5)
6
Prelude> (curry myFunc) 1 5
6
Prelude> :type (curry myFunc)
(curry myFunc) :: Num c => c -> c -> c
Prelude> :type uncurry
uncurry :: (a -> b -> c) -> (a, b) -> c
Prelude> let myFunc x y = x + y
Prelude> :type (uncurry myFunc)
(uncurry myFunc) :: Num c => (c, c) -> c
```

### Function composition

Just as in math, it is possible to compose functions in Haskell. With having two functions, one with type `a -> b` and second with type `b -> c` you can create composed one with type `a -> c`. All you need is the dot `(.)` operator. It can make your code more readable and easier to understand. It also provides a way how to build more complex functions from simpler parts (functions or partially applied functions).

```
Prelude> :type (5+)
(5+) :: Num a => a -> a
Prelude> :type (5*)
(5*) :: Num a => a -> a
Prelude> :type show
show :: Show a => a -> String
Prelude> show ( (5+) ( (5*) 5 ) )
"30"
Prelude> (show . (5+) . (5*)) 5
"30"
```

Other operator which you now might want to know is `($)` called "application". Which can help you with getting rid of parentheses for applying arguments to right function.

```
Prelude> show . (5+) . (5*) $ 5
"30"
```

### Pointfree style

It is very common in FP to write functions as a composition of other functions without mentioning the actual arguments they will be applied to. Consider following two examples and notice that although the result is the same, the first one is way cleaner and more readable.

```haskell
sumA = foldr (+) 0
sumB xs = foldr (+) 0 xs
```

Those are very simple examples but you can build more complex with function composition `(.)` and more partially applied or plain functions.

```haskell
myFunc :: Int -> String
myFunc = show . (5+) . (5*)
```

Now you might ask why we call this pointfree style when there are actually more points. A 'points-free' definition originates in math (not a suprise!) of a function is one which does not explicitly mention the points (values) of the space on which the function acts.

### Infix and prefix

### Own operators

### Anonymous functions

An anonymous function is a function without a name. It is a Lambda abstraction and might look like this: `\x -> x + 1`. Sometimes it is more convenient to use a lambda expression rather than giving a function a name. You should use anonymous functions only for very simple functions because it decreases readability of the code.

```haskell
myFunc1 = (\x y z -> x * y + z)
myFunc2 x y z = x * y + z
mapFunc1 = map myFunc1
mapAFunc1 = map (\x y z -> x * y + z)
```

## Higher order functions

As is visible from previous example, anonymous functions are sometimes good to use with combination of higher order functions. Higher order function is a function that takes function as argument and/or returns function as result. In this course there were already many of them: `(.)`, `curry`, `uncurry`, `map`, etc. We will now looks at those more used for structures...

### Map, filter

```
Prelude> :type map
map :: (a -> b) -> [a] -> [b]
Prelude> map show [1..5]
["1","2","3","4","5"]
Prelude> map (5*) [1..5]
[5,10,15,20,25]
Prelude> :type filter
filter :: (a -> Bool) -> [a] -> [a]
Prelude> filter (\x -> x `mod` 7 == 0) [1..50]
[7,14,21,28,35,42,49]
```

### Folds and scans

"reduce"

```
Prelude> :type foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
Prelude> :type foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
Prelude> foldl (-) 0 [1..10]
-55
Prelude> ((((((((((0-1)-2)-3)-4)-5)-6)-7)-8)-9)-10)
-55
Prelude> scanl (-) 0 [1..10]
[0,-1,-3,-6,-10,-15,-21,-28,-36,-45,-55]
Prelude> foldr (-) 0 [1..10]
-5
Prelude> (1-(2-(3-(4-(5-(6-(7-(8-(9-(10-0))))))))))
-5
Prelude> scanr (-) 0 [1..10]
[-5,6,-4,7,-3,8,-2,9,-1,10,0]
```

### Catamorphisms theory

## Typeclasses

Typeclass is class of types with definition of common functions for all instances of that class. It is allowing polymorphism and you can imagine it as interfaces in other languages.

### Kinds

In type theory, a kind is the type of a type constructor or, less commonly, the type of a higher-order type operator. A kind system is essentially a simply typed lambda calculus 'one level up,' endowed with a primitive type, denoted * and called 'type', which is the kind of any (monomorphic) data type. Simply you can observe this with GHCi and `:kind` command on various types. For example kind `* -> *` tells that you need to specify one type argument to create type with kind `*` so you can have values of it.

```
Prelude> :kind Integer
Integer :: *
Prelude> :kind Maybe
Maybe :: * -> *
Prelude> :kind Either
Either :: * -> * -> *
Prelude> :kind (Either Integer)
(Either Integer) :: * -> *
Prelude> :kind (Either Integer String)
(Either Integer String) :: *

```

### Polymorfism

https://en.wikibooks.org/wiki/Haskell/Polymorphism

### Own typeclass and instance

There are many defined typeclasses and basic types which are instances of those classes. You can create your own on top of it if you need more.

```haskell
class CSVExportable a where
    headings :: a -> [String]
    toList :: a -> String
    toCSV :: a -> String
    headingsCSV :: a -> String
```

*TODO example*

## Basic typeclasses in detail

### Deriving

We have already used the keyword `deriving` many times. It is kind of magic which will automatically derive instance of desired typeclass(es) so you don't have to write functions on your own.

You can derive only built-in typeclasses:

* `Eq` = (in)equality based on arguments
* `Ord` = `compare`, `min`, `max` and comparison operators
* `Show` = to `String` conversion
* `Read` = from `String` conversion
* `Enum` = enumerations only (no arguments), list `..` syntax
* `Bounded` = only for enumerations or just one arguments, `minBound` and `maxBound`

### Read and Show

If you derive default implementation of instance for `Show` and `Read` the string representing the data is actually the same as you would write it in Haskell code:

```
Prelude> data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Read)
Prelude>
Prelude> myTree = Node (Leaf 8) (Node (Leaf 70) (Leaf 15))
Prelude> show myTree
"Node (Leaf 8) (Node (Leaf 70) (Leaf 15))"
Prelude> read "Node (Leaf 5) (Leaf 100)"
*** Exception: Prelude.read: no parse
Prelude> (read "Node (Leaf 5) (Leaf 100)") :: Tree Int
Node (Leaf 5) (Leaf 100)

Prelude> :info Show
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
  	-- Defined in ‘GHC.Show’

...
```

You can of course make your own instance of those classes where would be representation different but why would you do that - you already like Haskell! When you make own `read` and `show`, you should ensure that after using `read` on string produced by `show` you will get the same data.

```haskell
-- TODO: Read, Show custom instance
```

### Numerics

For numbers there are several builtin typeclasses making the computation more flexible:

* `Num`
  * `Real`
    * `Integral`
    * `RealFrac`
  * `Fractional`
    * `Floating`
    * `RealFloat` (subclass of `Floating` and `RealFrac`)

### Comparison

For comparison there are two basic typeclasses - `Eq` and its subclass `Ord`:

```
-- ord, eq in GHCi
```

You can again implement your own instances of those classes:

```haskell
Prelude> :info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  	-- Defined in ‘GHC.Classes’
...

Prelude> :info Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
  	-- Defined in ‘GHC.Classes’
...
```

## FP in other languages

Functional programming concepts that you learn in pure functional language are applicable in other non-functional languages as well. Simply because those languages adopt some principles which make them more useful (or not so useless).

### C/C++

C++ is general purpose object oriented programming language based on C which is imperative procedural language. In both is possible to create functions (and procedures). There is no control if function is pure or not (if it is makind side effects). And in C/C++ you need to deal with mutability, pointers and working with memory on low level (de/allocation). But typing is strict and you can make higher order functions with "function pointer" types.

```cpp
int calculate(int(*binOperation)(const int, const int), const int operandA, const int operandB){
    return binOperation(operandA, operandB);
}
```

If you are normal person and not bighead you will most probably use `typedef` to name the type of such functions so the code is more readable and understandable. In newer versions of C++ there are also anonymous functions, combinators (`for_each`, `transform`, `filter`, ...), functors. Then you can of course use simpler functional concepts such as closures or recursion.

### Java

In Java 8 were added many functional programming concepts *TODO*

### Python

http://www.oreilly.com/programming/free/functional-programming-python.csp
https://docs.python.org/3/howto/functional.html
https://docs.python.org/3/library/typing.html

### Smalltalk and Ruby

## Task assignment

## Further reading

* [Haskell - Currying](https://wiki.haskell.org/Currying)
* [Haskell - Higher order function](https://wiki.haskell.org/Higher_order_function)
* [Haskell - Fold](https://wiki.haskell.org/Fold)
* [Learn You a Haskell for Great Good](http://learnyouahaskell.com) (chapters 3, 6, 8)
* [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
* [Haskell - OOP vs type classes](https://wiki.haskell.org/OOP_vs_type_classes)
* [WikiBooks - Haskell: Classes and types](https://en.wikibooks.org/wiki/Haskell/Classes_and_types)
