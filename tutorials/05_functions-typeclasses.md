# Advanced functions and intro to typeclasses

## More about functions

Creating new own functions or using the predefined from libraries is quite simple. But there are more interesting things you can do with functions. This is functional programming and all is about functions!

### Currying

When we talk about "currying" in Haskell it has (almost) nothing to do with dishes or spices. Famous mathematician and logician [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry) (the language is named after him) developed with others technique called currying: translating the evaluation of a function that takes multiple arguments (or a tuple of arguments) into evaluating a sequence of functions, each with a single argument. To better understand this, let's focus how we parenthesize function types:

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

If you like math, then it is the same difference as between *f*: &#8477; &rarr; &#8477; &rarr; &#8477; and *g*: &#8477; × &#8477; &rarr; &#8477;.

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

Another operator which you now might want to know is `($)` called "application". Which can help you with getting rid of parentheses for applying arguments to right function.

```
Prelude> show . (5+) . (5*) $ 5
"30"
```

Again like in math, *f*: A &rarr; B and *g*: B &rarr; C, then (*f* &#8728; *g*): A &rarr; C.

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

Now you might ask why we call this pointfree style when there are actually more points. A 'points-free' definition originates in math (not a surprise!) of a function is one which does not explicitly mention the points (values) of the space on which the function acts.

### Fixity and precedence

You might wonder how it works in Haskell that following expression is evaluated in correct order as you would expect without using brackets:

```
Prelude> 5 + 2^3 - 4 + 2 * 2
13
Prelude> 5 * sin pi - 3 * cos pi + 2
5.0
```

The first basic rule is that function application binds the most. For example, in `foo 5 + 4` it will first evaluate `foo 5` and then add `4` (`foo 5 + 4` is the same as `(+) (foo 5) 4`. If you want to avoid that, you need to use brackets `foo (5 + 4)` or function application operator `foo $ 5 + 4` (or strict `$!`). 

For infix operators (`+`, `**`, `/`, `==`, etc.) and functions (with backticks: `div`, `rem`, `quot`, `mod`, etc.), there is special infix specification with one of three keywords:

- `infix` = Non-associative operator (for example, comparison operators)
- `infixl` = Left associative operator (for example, `+`, `-`, or `!!`) 
- `infixr` = Right associative operator (for example, `^`, `**`, `.`, or `&&`)

Each of them should be followed by precedence (0 binds least tightly, and level 9 binds most tightly, default is 9) followed by the function/operator name. To see it in action, you can use `:info` to discover this specification for existing well-known operators and infix functions:

```
Prelude> :info (+)
...
infixl 6 +
Prelude> :info (&&)
...
infixr 3 &&
Prelude> :info div
...
infixl 7 `div`
```

You can also check *Table 4.1* of [The Haskell 2010 Language: Chapter 4 - Declarations and Bindings](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820061).

### Own operators

You know that operators are just functions and that you can switch always between prefix and infix:

```
Prelude> (+) 5 7
12
Prelude> 7 `div` 2
3
Prelude> foo x y z = x * (y + z)
Prelude> (5 `foo` 3) 12
65
```

You can define own operator as you would do it with function:

```
Prelude> (><) xs ys = reverse xs ++ reverse ys
Prelude> (><) "abc" "xyz"
"zyxcba"
Prelude> "abc" >< "xyz"
"zyxcba"
Prelude> :info (><)
(><) :: [a] -> [a] -> [a]
```

By default, its asociativity is left - as you can easily observe:

```
Prelude> "abc" >< "xyz" >< "klm"
"xyzabcmlk"
Prelude> ("abc" >< "xyz") >< "klm"
"xyzabcmlk"
Prelude> "abc" >< ("xyz" >< "klm")
"cbaklmxyz"
```

By default, its precedence level is 9. We can observe that by constructing expression with `(++)` which has level 5 and right associativity.

```
Prelude> "abc" >< "xyz" ++ "klm"
"cbazyxklm"
Prelude> "klm" ++ "abc" >< "xyz"
"klmcbazyx"
```

You can easily change that and if new precedence is lower, than `(++)` will be done first. If the precedence is the same, then it is applied in "natural" order (thus, it must have the same associativity, otherwise you get an error).

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
*Main> :info (><)
(><) :: [a] -> [a] -> [a]       -- Defined at test01.hs:3:1
infixl 5 ><
*Main> "abc" >< "xyz" ++ "klm"

<interactive>:6:1: error:
    Precedence parsing error
        cannot mix `><' [infixl 5] and `++' [infixr 5] in the same infix expression
*Main> ("abc" >< "xyz") ++ "klm"
"cbazyxklm"
*Main> :info (>-<)
(>-<) :: [a] -> [a] -> [a]      -- Defined at test01.hs:7:1
infixl 2 >-<
*Main> "abc" >-< "xyz" ++ "klm"
"cbamlkzyx"
*Main>

<interactive>:12:1: error: lexical error at character '\ESC'
*Main> "klm" ++ "abc" >-< "xyz"
"cbamlkzyx"
```

For operators you can use *symbol characters* as being any of `!#$%&*+./<=>?@\^|-~:` or "any *non-ascii* Unicode symbol or punctuation". But, an operator symbol starting with a colon `:` is a constructor.

### Infix and operator-like data constructors

Data constructors can be treated just as functions. You can pass them to a function as a parameter, return them from a function as a result and also use them in infix:

```
Prelude> data MyTuple a b = MyTupleConstr a b deriving Show
Prelude> :type MyTupleConstr
MyTupleConstr :: a -> b -> MyTuple a b
Prelude> MyTupleConstr 5 "Hi"
MyTupleConstr 5 "Hi"
Prelude> 5 `MyTupleConstr` "Hi"
MyTupleConstr 5 "Hi"
```

As we said, for constructors you may create operator starting with a colon (and optionally specify also `infix`, `infixl`, or `infixr`).

```
Prelude> data MyTuple2 a b = a :@ b deriving Show
Prelude> :type (:@)
(:@) :: a -> b -> MyTuple2 a b
Prelude> 5 :@ "Hi"
5 :@ "Hi"
Prelude> (:@) 5 "Hi"
5 :@ "Hi"
```

You can try that using operator which doesn't start with a colon is not possible. But you can always make a synonym and then your code more readable:

```
Prelude> data MyTuple3 a b = a @@ b deriving Show

<interactive>:15:17: error: Not a data constructor `a'
Prelude> (@@) = (:@)
Prelude> :type (@@)
(@@) :: a -> b -> MyTuple2 a b
Prelude> 5 @@ "Hi"
5 :@ "Hi"
```

Another fine feature is, that operators `:@` and `@@` can be specified with different associativity and precedence!

### Anonymous functions

An anonymous function is a function without a name. It is a Lambda abstraction and might look like this: `\x -> x + 1`. Sometimes it is more convenient to use a lambda expression rather than giving a function a name. You should use anonymous functions only for very simple functions because it decreases readability of the code.

```haskell
myFunc1 = (\x y z -> x * y + z)
myFunc2 x y z = x * y + z
mapFunc1 = map myFunc1
mapAFunc1 = map (\x y z -> x * y + z)
```

## Higher order functions

As is visible from the previous example, anonymous functions are sometimes good to use with a combination of higher order functions. Higher order function is a function that takes a function as an argument and/or returns a function as a result. In this course, there were already many of them: `(.)`, `curry`, `uncurry`, `map`, etc. We will now look at those more used for structures...

### Map and filter

Two widely used functions well-known in the most of functional (but others as well) programming languages are `map` and `filter`. In the `Prelude` module, they are defined for lists, but they work in the same way for other data structures (`Data.Sequence`, `Data.Set`, etc.). When you need to *transform* a list by applying a function to its every element, then you can use `map`. If you have a list and need to make a sublist based on some property of its elements, use `filter`. The best for understanding is to look at its possible implementation.

```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap xs

myFilter :: (a -> Bool) -> [a] -> [a]  
myFilter _ []     = []  
myFilter p (x:xs)   
      | p x       = x : filter p xs  
      | otherwise = filter p xs  
```

It is really nothing complicated, actually, it is very simple, straightforward, and powerful.

```
Prelude> :type map
map :: (a -> b) -> [a] -> [b]
Prelude> map show [1..5]
["1","2","3","4","5"]
Prelude> map (5*) [1..5]
[5,10,15,20,25]
Prelude> map (length . show . abs) [135, (-15), 0, 153151]
[3,2,1,6]
Prelude> :type filter
filter :: (a -> Bool) -> [a] -> [a]
Prelude> filter (\x -> x `mod` 7 == 0) [1..50]
[7,14,21,28,35,42,49]
```

Soon we will get into a generalized function called `fmap` while discussing the term *functor*.

### Folds and scans

Maybe you've heard about *Map/Reduce*... We know `map`, but there is no `reduce`! Actually, there is, but it is called [fold](https://wiki.haskell.org/Fold) (it is practically a synonym in functional programming). Folds are higher order functions that process a data structure in some order and build a return value. It (as everything in Haskell) has foundations in math - concretely in [Catamorphism](https://wiki.haskell.org/Catamorphisms) (Category Theory). 

To get into folds in practice, let's try to implement `sum` and `product` functions (if you want to practice on your own, try it with `and` and `or`).

```haskell
mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs
```

Obviously, there are some similarities:

1. initial value for an empty list (`0` for `sum` and `1` in the case of `product`),
2. use a function and apply it to an element and recursive call to the rest of the list.

Let's do the generalized higher order function that also takes an initial value and a function for processing.

```haskell
process :: (a -> a -> a) -> a -> [a] -> a
process _ initValue [] = initValue
process f _ (x:xs)     = f x (process xs)

mySum = process (+) 0
myProduct = process (*) 1
```

But here we are getting into a problem. Both `(+)` and `(*)` use operands and result of the same type - if we want to convert a number to string and join it in one go with `process`, it is not possible!

```
Prelude> process (\x str -> show x ++ str) "" [1,2,3,4]
```

The type of inital value must be the same as the type which is returned by given function. Now we get this:


```haskell
process :: (a -> b -> b) -> b -> [a] -> b
process _ initValue [] = initValue
process f _ (x:xs)     = f x (process xs)

mySum = process (+) 0
myProduct = process (*) 1
myToStrJoin = process (\x str -> show x ++ str) ""
```

Now problem is that both `(+)` and `(*)` are commutative, but `(\x str -> show x ++ str)` is not, even type of `x` and `str` can be different. What if we need to apply the function in a different order? Now we have to create two variants.


```haskell
processr :: (a -> b -> b) -> b -> [a] -> b   -- "accumulates" in the RIGHT operand
processr _ initValue [] = initValue
processr f _ (x:xs)     = f x (processl xs)

processl :: (b -> a -> b) -> b -> [a] -> b   -- "accumulates" in the LEFT operand
processl _ initValue [] = initValue
processl f _ (x:xs)     = f (processr xs) x

mySum = processl (+) 0
myProduct = processl (*) 1
myToStrJoinR = processr (\x str -> show x ++ str) ""
myToStrJoinL = processl (\x str -> show x ++ str) ""
```

It is something so general, that it is prepared for you and not just for lists but for every instance of typeclass `Foldable` - two basic folds `foldl`/`foldr` and related `scanl`/`scanr`, which capture intermediate values in a list:

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

There are some special folds and specific folds:

```
Prelude> foldr1 (+) [1..10]
Prelude> foldl1 (*) [1..10]
Prelude> foldr1 (+) []
Prelude> foldl1 (*) []

Prelude> foldl' (*) 0 [1..10]  -- strict evaluation before reduction
Prelude> foldl'1 (*) [1..10]

Prelude> minimum [1,2,63,12,5,201,2]
Prelude> maximum [1,2,63,12,5,201,2]
```

If you like folds, try to implement `foldl` by using `foldr` (spoiler: [solution](https://wiki.haskell.org/Foldl_as_foldr)).

## Typeclasses

Typeclass is class of types with a definition of common functions for all instances of that class. It is allowing polymorphism and you can imagine it as interfaces in other languages.

### Kinds

In the type theory, a kind is the type of a type constructor or, less commonly, the type of a higher order type operator. A kind system is essentially a simply typed lambda calculus 'one level up,' endowed with a primitive type, denoted * and called 'type', which is the kind of any (monomorphic) data type. Simply you can observe this with GHCi and `:kind` command on various types. For example kind `* -> *` tells that you need to specify one type argument to create a type with kind `*` so you can have values of it.

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

In Haskell, there are more ways of polymorphism. It might be very surprising, especially for people who believe that polymorphism is something related only to object oriented programming. We already used type classes and type variables - the basic enablers of polymorphism in Haskell.

**Parametric polymorphism** refers to when the type of a value contains one or more (unconstrained) type variables, so that the value may adopt any type that results from substituting those variables with concrete types. It is when the type variable *is not constrained* by some type class. For example, length of a list `[a]` works for any type `a`. In this tutorial, there was the `map` function with type `(a -> b) -> [a] -> [b]` - also parametric polymorphism. This type of polymorphism doesn't know anything about the type which it will be used with. It must behave the same regardless of its type. This is a somewhat limiting but extremely useful property known as versatility.

**Ad-hoc polymorphism** refers to when a value is able to adopt any one of several types because it, or a value it uses, has been given a separate definition for each of those types. It is when the type variable *is constrained* by some type class. Thanks to that extra information about the given *still-generic* type, it is possible to used some behavior defined during class instantiation. Haskell even allows class instances to be defined for types which are themselves polymorphic - you can make your implementation of the arbitrary list an instance of some class. This polymorphism is not only about functions, but also values - recall `Bounded` class with `minBound` and `maxBound` values which are different for each instance.

There are some other types of polymorphism that are implemented in some extensions to Haskell, e.g. [rank-N types](https://wiki.haskell.org/Rank-N_types) and [impredicative types](https://wiki.haskell.org/Impredicative_types). But there are also types of polymorphism that are not supported in Haskell, for example, subtyping and inclusion polymorphism.

### Own typeclass and instance

There are many defined typeclasses and basic types which are instances of those classes. You can create your own on top of it if you need more. You need to use keyword `class` to defined functions for that typeclass and also optionally class inheritance. It is possible to just declare the type of function but also to define it completely (also using other declared functions). Then by using the keyword `instance` you can define how some type is an instance of a typeclass (define or re-define functions for that class).

```haskell
-- from https://en.wikibooks.org/wiki/Haskell/Classes_and_types#A_concerted_example
-- Location, in two dimensions.
class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

-- An example type, with accompanying instances.
data NamedPoint = NamedPoint
    { pointName :: String
    , pointX    :: Int
    , pointY    :: Int
    } deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p { pointX = x, pointY = y }

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
    where
    (x, y) = getLocation p
```

## Basic typeclasses in detail

### Deriving

We have already used the keyword `deriving` many times. It is kind of magic which will automatically derive an instance of desired typeclass(es) so you don't have to write functions on your own.

You can derive only built-in typeclasses:

* `Eq` = (in)equality based on arguments
* `Ord` = `compare`, `min`, `max` and comparison operators
* `Show` = to `String` conversion
* `Read` = from `String` conversion
* `Enum` = enumerations only (no arguments), list `..` syntax
* `Bounded` = only for enumerations or just one arguments, `minBound` and `maxBound`

You cannot use `deriving` for your own classes without putting some more effort in it by using [Data.Derive](https://hackage.haskell.org/package/derive) and [Template Haskell](https://wiki.haskell.org/Template_Haskell). 

### Read and Show

If you derive default implementation of instances for `Show` and `Read` the string representing the data is actually the same as you would write it in Haskell code:

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
```

```haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
  	-- Defined in ‘GHC.Show’
```

You can of course make your own instance of those classes where would be representation different but why would you do that - you already like Haskell! When you make own `read` and `show`, you should ensure that after using `read` on string produced by `show` you will get the same data.

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

size :: Num a => Tree b -> a
size (Leaf _)   = 1
size (Node l r) = size l + size r

instance (Show a) => Show (Tree a) where
  show (Leaf x)   = show x
  show (Node l r) = show l ++ " " ++ show r     -- would be possible to write read for this?
```

Typeclass `Read` is a bit more complicated. If you want to make own implementation, you need to write the parser. Parsing will be covered later on during the course. Basically, it tries to convert `String` to `a` and return it together with remaining `String`.

```haskell
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
  GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec [a]
  {-# MINIMAL readsPrec | readPrec #-}
        -- Defined in `GHC.Read'
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
    
If you really don't need to explicitly specify the type of number, use typeclass constraint in the declaration of function type.

### Comparison

For comparison there are two basic typeclasses - `Eq` and its subclass `Ord`:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  	-- Defined in ‘GHC.Classes’

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
```

You can again implement your own instances of those classes:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

size :: Num a => Tree b -> a
size (Leaf _)   = 1
size (Node l r) = size l + size r

instance (Show a) => Show (Tree a) where
  show (Leaf x)   = show x
  show (Node l r) = show l ++ " " ++ show r     -- would be possible to write read for this?

instance Eq (Tree a) where
    (==) t1 t2 = (size t1) == (size t2)

instance Ord (Tree a) where
    compare t1 t2 = compare (size t1) (size t2)
```

### Enum and Bounded

Class `Enum` defines operations on sequentially ordered types and it is a subclass of `Bounded` which defines just `minBound` and `maxBound` values. As you can see below, `Enum` describes 8 functions but only 2 are required (other will be derived based on that). Functions `toEnum` and `fromEnum` serve to specifying the order by numbering with `Int`s.

```haskell
class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int
    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]
    {-# MINIMAL toEnum, fromEnum #-}
```

When you derive `Enum` the order will be generated as left-to-right order of data constructors (without parameters, an enumeration consists of one or more nullary). Similarly, deriving `Bounded` will use first and last data constructor.

Enumerations have also the `..` syntactic sugar. For example, `[1..10]` is translated to `enumFromThen 1 10` and `[1,5..100]` is translated to `enumFromThenTo`

## FP in other languages

Functional programming concepts that you learn in pure functional language are applicable in other non-functional languages as well. Simply because those languages adopt some principles which make them more useful (or not so useless).

### C/C++

C++ is general purpose object oriented programming language based on C which is imperative procedural language. In both is possible to create functions (and procedures). There is no control if a function is pure or not (if it is making side effects). And in C/C++ you need to deal with mutability, pointers and working with memory on low level (de/allocation). But typing is strict and you can make higher order functions with "function pointer" types.

```cpp
int calculate(int(*binOperation)(const int, const int), const int operandA, const int operandB){
    return binOperation(operandA, operandB);
}
```

If you are a normal person and not bighead you will most probably use `typedef` to name the type of such functions so the code is more readable and understandable. In newer versions of C++ there are also anonymous functions, combinators (`for_each`, `transform`, `filter`, ...), functors. Then you can of course use simpler functional concepts such as closures or recursion.


```cpp
typedef int(*binOperation)(const int, const int);  /* I am not a bighead */

int calculate(binOperation bo, const int operandA, const int operandB){
    return bo(operandA, operandB);
}
```

* [libf - C++ as a Pure Functional Programming Language](https://github.com/GJDuck/libf)
* [Functional in C++17 and C++20](http://www.modernescpp.com/index.php/functional-in-c-17-and-c-20)

### Java

* [Flying Bytes - An Introduction to Functional Programming in Java 8](https://flyingbytes.github.io/programming/java8/functional/part0/2017/01/16/Java8-Part0.html)
* [Hackernoon - Finally Functional Programming in Java](https://hackernoon.com/finally-functional-programming-in-java-ad4d388fb92e)
* [JavaCodeGeeks - Java 9 Functional Programming Tutorial](https://examples.javacodegeeks.com/core-java/java-9-functional-programming-tutorial/)

### Python

* [Functional Programming in Python (free O'Reilly)](http://www.oreilly.com/programming/free/functional-programming-python.csp)
* [Python 3 - Functional Programming HOWTO](https://docs.python.org/3/howto/functional.html)
* [Python 3 - Typing](https://docs.python.org/3/library/typing.html)

### Smalltalk and Ruby

* [Smalltalk, A Functional Programming Language](http://monospacedmonologues.com/post/138978728947/smalltalk-a-functional-programming-language)
* [Functional Programming in Ruby for people who don’t know what functional programming is](https://medium.com/craft-academy/functional-programming-in-ruby-for-people-who-dont-know-what-functional-programming-is-f0c4ab7dc68c)
* [Functional Programming in Ruby](http://joelmccracken.github.io/functional-programming-in-ruby/#/)
* [Embracing Functional Programming in Ruby](https://kellysutton.com/2017/09/13/embracing-functional-programming-in-ruby.html)

## Task assignment

The homework to practice working with advanced functional patterns, operators, and typeclasses is in repository [MI-AFP/hw05](https://github.com/MI-AFP/hw05).

## Further reading

* [Haskell - Currying](https://wiki.haskell.org/Currying)
* [Haskell - Higher order function](https://wiki.haskell.org/Higher_order_function)
* [Haskell - Fold](https://wiki.haskell.org/Fold)
* [Learn You a Haskell for Great Good](http://learnyouahaskell.com) (chapters 3, 6, 8)
* [Haskell - Polymorphism](https://wiki.haskell.org/Polymorphism)
* [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
* [Haskell - OOP vs type classes](https://wiki.haskell.org/OOP_vs_type_classes)
* [WikiBooks - Haskell: Classes and types](https://en.wikibooks.org/wiki/Haskell/Classes_and_types)
