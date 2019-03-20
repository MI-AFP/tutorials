# Typeclasses - custom and predefined

## Typeclasses

Typeclass is a class of types with a definition of common functions for all instances of that class. Please note that the term "class" here means a [mathematical class in the set theory](https://en.wikipedia.org/wiki/Class_(set_theory)), not an [object-oriented class](https://en.wikipedia.org/wiki/Class_(computer_programming))!.

Typeclasses represent a powerful means for polymorphism in a strong static type system. They can be related to interfaces in Java or protocols in Clojure, however, they are more powerful.

### Kinds

In the type theory, a kind is the type of a type constructor or, less commonly, the type of a higher order type operator. A kind system is essentially a simply-typed lambda calculus 'one level up' from functions to their types, endowed with a primitive type, denoted `*` and called 'type', which is the kind of any (monomorphic) data type. Simply you can observe this with GHCi and `:kind` command on various types. For example kind `* -> *` tells that you need to specify one type argument to create a type with kind `*` so you can have values of it.

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

### Polymorphism

[Polymorphism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) (from Greek πολύς, polys, "many, much" and μορφή, morphē, "form, shape") is the provision of a single interface to entities of different types. A polymorphic type is one whose operations can also be applied to values of some other type, or types. Polymorphism is usually connected with object-oriented programming today, however, there are even more powerful types of polymorphism in functional programming languages, such as Haskell or Clojure.

We already used type classes and type variables - the basic enablers of polymorphism in Haskell. **Parametric polymorphism** refers to the situation when the type of a value contains one or more (unconstrained) type variables, so that the value may adopt any type that results from substituting those variables with concrete types. It is when the type variable *is not constrained* by some type class. For example, length of a list `[a]` works for any type `a`. In this tutorial, there was the `map` function with type `(a -> b) -> [a] -> [b]` - also a parametric polymorphism. This type of polymorphism doesn't know anything about the type which it will be used with. The behaviour is abstracted regardless of a concrete type. This is a somewhat limiting but extremely useful property known as versatility.

**Ad-hoc polymorphism** refers to the situation when a value is able to adopt any one of a defined set of types because it, or a value it uses, has been given a separate definition for each of those types. This is a situation when the type variable *is constrained* by some type class. Thanks to that extra information about the given *still-generic* type, it is possible to use behaviour defined during class instantiation. Haskell even allows class instances to be defined for types which are themselves polymorphic - you can make your implementation of an arbitrary list an instance of some class. This polymorphism is not only about functions, but also about values - recall `Bounded` class with `minBound` and `maxBound` values which are different for each instance.

There are some other types of polymorphism that are implemented in some extensions to Haskell, e.g. [rank-N types](https://wiki.haskell.org/Rank-N_types) and [impredicative types](https://wiki.haskell.org/Impredicative_types). But there are also types of polymorphism that are not supported in Haskell, for example, subtyping and inclusion polymorphism.

### Own typeclass and instance

There are some commonly used typeclasses and their instances available and you can create your own, as well. You need to use keyword `class` to defined functions for that typeclass and also optionally a class inheritance, i.e. a base class, which is extended. There can be even more, so the inheritance forms [lattices](https://commons.wikimedia.org/wiki/File:Base-classes.svg).

Contrary to Java interfaces, apart from function declarations, Haskell typeclasses can also contain function implementations.

Next, you create typeclass instances using the keyword `instance`, which means you define or (re-define) functions for that class.

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

You can use `deriving` for your own classes, but you need to put some (advanced) effort in it by using [Data.Derive](https://hackage.haskell.org/package/derive) and [Template Haskell](https://wiki.haskell.org/Template_Haskell).

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

When you make own `read` and `show`, you should ensure that after using `read` on a string produced by `show`, you will get the same data:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

size :: Num a => Tree b -> a
size (Leaf _)   = 1
size (Node l r) = size l + size r

instance (Show a) => Show (Tree a) where
  show (Leaf x)   = show x
  show (Node l r) = show l ++ " " ++ show r     -- would be possible to write read for this?
```

 `show` is (quite often) abused to represent fancy string representations, but it is adviced to name such functions differently (such as `myShow`), or to use a pretty-printing library such as [Text.PrettyPrint](https://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint.html), [pretty](https://hackage.haskell.org/package/pretty), [pretty-simple](https://hackage.haskell.org/package/pretty-simple) or [prettyprinter](https://hackage.haskell.org/package/prettyprinter).

Typeclass `Read` is a bit more complex. If you want to make your own implementation, you need to write a parser. Parsing will be covered later on during the course. Basically, it tries to convert `String` to `a` and return it together with the remaining `String`.

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

For numbers, there are several built-in typeclasses making numeric computing more flexible:

* `Num`
  * `Real`
    * `Integral`
    * `RealFrac`
  * `Fractional`
    * `Floating`
    * `RealFloat` (subclass of `Floating` and `RealFrac`)

If you don't need to explicitly specify the type of number, use the typeclass constraint in the declaration of function type. This is a general rule of thumb: be as much generic, as possible.

### Comparison

There are two basic typeclasses - `Eq` and its subclass `Ord` that specify comparisons.

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

Class `Enum` defines operations on sequentially ordered types and it is a subclass of `Bounded` which defines just `minBound` and `maxBound` values. As you can see below, `Enum` describes 8 functions but only 2 are required (other will be derived based on that). Functions `toEnum` and `fromEnum` serve for specifying the order by numbering with `Int`s.

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

When you derive `Enum`, the order will be generated as left-to-right order of data constructors (without parameters, an enumeration consists of one or more nullary ones). Similarly, deriving `Bounded` will use the first and the last data constructor.

Enumerations have also the `..` syntactic sugar. For example, `[1..10]` is translated to `enumFromThen 1 10` and `[1,5..100]` is translated to `enumFromThenTo`

## Mathematical Typeclasses

Now we are going to spend some time with predefined and important typeclasses that capture important concepts in Haskell that are widely used in many projects. Typeclass always says something about the structure of the type and what you can do with that. Also, there are some laws and it is very tightly related to math -- specifically the algebra and the category theory.

After learning common typeclasses, it is not just easier to use them and understand a code written by other developers, but it also helps with designing own custom typeclasses. You can always find out more about a typeclass and instances with GHCi `:info` command.

### Intro: Mathematical foundations

The relation between math and Haskell is very strong. You can observe it everywhere. Haskell functions are very similar to mathematical functions when you talk about their type, definitions with `let` and `where` keywords, guards with `otherwise`, function compositions, and so on. In this tutorial, we are going to see this relation even more -- with typeclasses that come from the mathematical world. You should be already familiar with [basic abstract algebra](https://en.wikipedia.org/wiki/Algebra#Abstract_algebra) (esp. algebraic structures with a single binary operation).

When getting into the math, you should know that Haskell is based not just on the basic algebra, set theory, and logic, but also on the category theory. In order to sometimes mention the relation, we will briefly explain what it is about. If you want to know more, please refer to some mathematical tutorials on your own.

#### Category theory

Category theory is a higher abstraction level over *Monoid*, *Semigroup*, and similar algebraic structures. Actually, it is so abstract that it can describe so many things ... that it is very hard to comprehend. The "best practice" among programmers is to learn it bit by bit and return to it from time to time until things "click" together. This "click" means putting together the theory and its practical applications.

A **category** is a structure or collection *C* with three components:

* a collection of **objects**, *ob(C)*,
* a collection of **morphisms**, *hom(C)*, that ties two objects together; sometimes they are called **arrows**,
* a **composition** of morphisms (similar to function composition).

There are many categories, for example, **Set** category has all possible sets as objects, standard functions as morphisms, and classical function composition. There are also three laws:

1. the composition of category must be associative (i.e., *f ∘ (g ∘ h) = (f ∘ g) ∘ h*),
2. the category needs to be closed under the composition operation (i.e., for all applies *h = f ∘ g ⇒ h ∈ C*),
3. for every object *A ∈ ob(C)* there is an identity function *idA: A → A*, *idA ∈ hom(C)*.

#### The Hask category

In Haskell, we have the **Hask** category where:

* *ob(Hask)* are **types** (`Char`, `Int`, `Double`, `[Integer]`, `Person`, `Int -> String`, etc.),
* *hom(C)* are **functions** (`show`, `id`, `length`, `words`, `flip`, `reverse`, etc.),
* composition is **function composition** `(.)`.

The identity function is for every *o ∈ ob(Hask)* the polymorphic `id` function. The associativity of the composition is assured and in *hom(C)* there are all the functions, even those created by composition. That's it for now -- now we will show some typeclasses, their laws and come back to **Hask** when necessary...

### Semigroup, Monoid, ...

`Monoid` is the most simple typeclass we will learn. You can recall the [monoid](https://en.wikipedia.org/wiki/Monoid) from the algebra -- it is an algebraic structure with one binary operation that is associative and there is also one identity element. The same goes for Haskell -- the operation is called `mappend` and the identity is `mempty` (the first letter `m` if for **m**onoid).

Since `base-4.11.0.0`, the `Monoid` is a subclass of `Semigroup` (just like in algebra). `Semigroup` defines just binary operation which is associative. In Haskell, the binary operation is `(<>)` (infixr 6)

```haskell
class Semigroup s where
  (<>) :: s -> s -> s               -- binary associative operation

class Semigroup m => Monoid m where
  mempty  :: m                      -- identity of mappend
  mappend :: m -> m -> m
  mappend = (<>)                    -- binary operation from Semigroup
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty    -- fold with mappend (catamorphism)
```

The law of monoid says that `mappend` must be associative and `mempty` is a real identity when working with `mappend`:

```haskell
mappend x (mappend y z) == mappend (mappend x y) z
-- the same in infix:
x `mappend` (y `mappend` z) == (x `mappend` y) `mappend` z

mappend x mempty == x
mappend mempty x == x
```

If you take a look at the documentation of [Data.Monoid](https://hackage.haskell.org/package/base/docs/Data-Monoid.html), you might notice few more things:

* by default, a synonym for `mappend` is `(<>)` so you can simply use it as operator `x <> b` (notice that it is not the same as not-equals in other languages),
* multiple newtypes for specifying monoid for basic types, like `Sum` and `Product` for numeric types, `All` and `Any` for booleans, `First` and `Last` for maybes and few more.


```
Prelude> import Data.Monoid
Prelude Data.Monoid> :info Sum
newtype Sum a = Sum {getSum :: a}       -- Defined in ‘Data.Monoid’
...
Prelude Data.Monoid> :info Product
newtype Product a = Product {getProduct :: a}
...
Prelude Data.Monoid> (Product 5) <> (Product 2)
Product {getProduct = 10}
Prelude Data.Monoid> mempty :: Product  Int
Product {getProduct = 1}
Prelude Data.Monoid> (Sum 5) <> (Sum 2)
Sum {getSum = 7}
Prelude Data.Monoid> mempty :: Sum Int
Sum {getSum = 0}
Prelude Data.Monoid> mconcat (map Sum [1..5])
Sum {getSum = 15}
Prelude Data.Monoid> mconcat (map Product [1..5])
Product {getProduct = 120}
```

#### Example: textual types

One of the very practical usages of `mappend` is string concatenation, which is independent of its concrete implementation:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)

s1 :: String
s1 = "hello"
s2 :: String
s2 = "monoid"

t1 :: Text
t1 = "hello"
t2 :: Text
t2 = "monoid"

s1 <> ", " <> s2 -- instead of s1 ++ ", " ++ s2
t1 <> ", " <> t2 -- works the same for text!
```

Here, obviously `mappend` is string concatenation and `mempty = ""`.

#### Example: Maybe

From `:info Monoid`, we can see that `Maybe a` is an instance of `Monoid` iff (=if and only if) `a` is an instance of `Monoid`. Then, the `(<>)` is "propagated" inside and obviously the identity is `Nothing`.

```
Prelude Data.Maybe Data.Semigroup> (Just "a") <> (Just "b")
Just "ab"
Prelude Data.Maybe Data.Semigroup> (Just "a") <> Nothing
Just "a"
Prelude Data.Maybe Data.Semigroup> (Just "a") <> Nothing <> (Just "b")
Just "ab"
Prelude Data.Maybe Data.Semigroup> Nothing <> Nothing
Nothing
Prelude Data.Maybe Data.Semigroup> mconcat [Just "a", Nothing, Just "b"]
Just "ab"
Prelude Data.Maybe Data.Semigroup> mempty :: Maybe String
Nothing
```

#### Verify the laws

As said, there are some laws (identity and associativity in this case) that should be valid, but the compiler cannot enforce it. Whenever you introduce your own instance of `Monoid` or other structure with some laws that are expected to be valid, use tests to prove it. One way is to write the properties on your own. The second and better one is to use [checkers](https://hackage.haskell.org/package/checkers), where many standard properties are prepared for you...

#### Others from basic algebra

Apart from basic `Monoid` from algebra, there are also other variants. You might find interesting to learn more about:

* [semigroupoids](https://hackage.haskell.org/package/semigroupoids/docs/Data-Groupoid.html) (Semigroupoid, Grupoid),
* [groups](https://hackage.haskell.org/package/groups/docs/Data-Group.html) (Group, Abelian),
* etc.

It is possible to write own instances of `Monoid` or other typeclasses. However, mind that compiler *won't* check if laws are valid in your instance. For such checks, you can use testing frameworks (esp. property testing), which will be covered later on.

### Functor

A functor is a way to apply a function to values inside some structure, while the structure remains intact. For example, if you want to change values in a list, tree or in Either without dealing with complexity and internal structure.

```haskell
class Functor f where                -- f is type constructor of kind * -> *
  fmap :: (a -> b) -> f a -> f b
```

The definition says that there is a function `fmap` which applies a function of type `a -> b` on elements in functor `f` with inner type `a` and the result will be functor `f` with inner type `b`. Moreover, there are two laws:

```haskell
-- identity (fmap doesn't do anything more than applying given function)
fmap id == id
-- composition
fmap (f . g) == fmap f . fmap g
```

Let's try it with basic containers like list, `Maybe`, and `Either`:

```
Prelude> fmap (*2) [1..5]   -- just like map!
[2,4,6,8,10]
Prelude> fmap (show . (*2)) [1..5]   -- just like map!
["2","4","6","8","10"]
Prelude> fmap (*2) (Just 7)
Just 14
Prelude> fmap (*2) Nothing
Nothing
Prelude> fmap (+10) (Left 5)
Left 5     -- no change!
Prelude> fmap (+10) (Right 5)
Right 10   -- changed, because "Either c" is functor for whatever "c" - it doesn't care
```

Just as with Monoid, you can take a look at the documentation of [Data.Functor](https://hackage.haskell.org/package/base/docs/Data-Functor.html). Again, there is an operator alias, in this case `(<$>)` for `fmap` (denoting a sort of "wrapped" or "inside" apply). There are two more -- `<$` and `$>` (just flipped `<$`). Flipped version of `(<$>)` is `(<&>)`.

```
Prelude Data.Functor> (*2) <$> [1..5]
[2,4,6,8,10]
Prelude Data.Functor>  [1..5] <&> (*2)
[2,4,6,8,10]
Prelude Data.Functor> 2 <$ [1..5]
[2,2,2,2,2]
Prelude Data.Functor> [1..5] $> 2
[2,2,2,2,2]
Prelude> (*2) <$> (Just 7)
Just 14
Prelude> 2 <$ (Just 7)
Just 2
Prelude> (Just 7) $> 2
Just 2
```

These examples might seem a bit too simple, but you can have any instance of `Functor` without knowing the structure and implementation of it and affect what is inside by these two (four if counting also flipped) simple operators.

#### Lifting

[Lifting](https://wiki.haskell.org/Lifting) is a concept that allows you to transform a function into a corresponding function within another (usually a more general) setting. Lifting is again a concept taken from mathematics and category theory (see [wikipedia](https://en.wikipedia.org/wiki/Lift_(mathematics)).

```haskell

data Point2D a = Point2D a a
               deriving Show

instance Functor Point2D where
    fmap f (Point2D x y) = Point2D (f x) (f y)

liftF0 :: a -> Point2D a
liftF0 x = Point2D x x

liftF1 :: (a -> b) -> Point2D a -> Point2D b
liftF1 = fmap

liftF2 :: (a -> b -> c) -> Point2D a -> Point2D b -> Point2D c
liftF2 f (Point2D x1 x2) (Point2D y1 y2) = Point2D (f x1 y1) (f x2 y2)

origin :: Point2D Int
origin = liftF0 0

doublePoint :: Point2D Int -> Point2D Int
doublePoint = liftF1 (*2)

plusPoints :: Point2D Int -> Point2D Int -> Point2D Int
plusPoints = liftF2 (+)
```

#### Functors on Hask category

In mathematics, a functor is a type of mapping between categories arising in category theory. Functors can be thought of as homomorphisms between categories. In the category of small categories, functors can be thought of more generally as morphisms. ([wikipedia](https://en.wikipedia.org/wiki/Functor))

We have some categories which have objects and morphisms that relate our objects together. Functor *F: C → D* relates categories *C* and *D* together - it is a transformation between categories:

- maps every object *A* from category *C* to object *F(A)* in category *D*
- maps every morphism *f: A → B* from category *C* to morphism *F(f): F(A) → F(B)* in category *D*

```haskell
class Functor (f :: * -> *) where
   fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> (f a -> f b)
```

There is also the identity law for functions and they must be homomorphic that, of course, apply for functors in Haskell:

```haskell
fmap id == id

fmap (f . g) = fmap f . fmap g
```

### Applicative

Another important typeclass is [Control.Applicate](https://hackage.haskell.org/package/base/docs/Control-Applicative.html). Notice that it is not "Data" anymore, but "Control" instead! It is an intermediate structure between a `Functor` and a `Monad`. It is simpler than `Monad`, not so powerful, but sufficient in many use cases, and also easier to understand - it is **Applicative functor**.

In functor, we applied a function over a "wrapped" value to get a resulting "wrapped" value. In applicative, we have the function wrapped, as well:

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a                   -- lift a
  (<*>) :: f (a -> b) -> f a -> f b   -- sequential application

--(<$>) ::   (a -> b) -> f a -> f b   -- this is from functor
```

Function `pure` only lifts something into applicative structure `f`. The more interesting part is the ["tie-fighter"](http://starwars.wikia.com/wiki/TIE/LN_starfighter) operator `<*>` that applies a lifted function over an applicative. You can find out in the documentation following similar functions and partial functions as in [Data.Functor](https://hackage.haskell.org/package/base/docs/Data-Functor.html):

```haskell
(<*) :: f a -> f b -> f a
(*>) :: f a -> f b -> f b

liftA  :: (a -> b) -> f a -> f b
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

There are again some laws:

```haskell
-- identity
pure id <*> v == v
-- composition
pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
-- homomorphism
pure f <*> pure x = pure (f x)
-- interchange
u <*> pure y = pure ($ y) <*> u
```

```
Prelude> linfunc x = 2 * x + 10
Prelude> (Just lin
lines    linfunc
Prelude> (Just linfunc) <*> (Just 5)
Just 20
Prelude> pure linfunc <*> (Just 5)
Just 20
Prelude> pure linfunc <*> Nothing
Nothing
Prelude> Nothing <*> (Just 5)
Nothing
Prelude> (Just 5) <* (Just 10)
Just 5
Prelude> (Just 5) *> (Just 10)
Just 10

Prelude> pure linfunc <*> (Left 7)
Left 7
Prelude> pure linfunc <*> (Right 15)
Right 40
Prelude> (Right 5) *> (Left 10)
Left 10
Prelude> (Right 5) <* (Left 10)
Left 10
Prelude Control.Applicative> (Right 15) <**> pure linfunc
Right 40

-- Lifts are already prepared in Control.Applicative
Prelude Control.Applicative> liftA2 (+) (Just 5) (Just 10)
Just 15
Prelude Control.Applicative> liftA2 (+) (Just 5) Nothing
Nothing
Prelude Control.Applicative> liftA2 (+) (Left "error") (Right 7)
Left "error"
Prelude Control.Applicative> liftA2 (+) (Right 15) (Right 7)
Right 22
```

#### Actions vs. functions

In Haskell terminology, we call `Functor f => f a` an **action**. Actions have the power to do some side effect but not necessarily (e.g., `Just "no effect"`). For example, `liftA2` is described as a function that lifts a binary function to actions. Special sort of actions are I/O actions that do something with input and output, but there can be also other actions making side effects.

### Monad

The most famous (and [scary](https://camo.githubusercontent.com/f2c3667a2cdf19c0cf203fad44c81d197c4cd740/68747470733a2f2f692e696d67666c69702e636f6d2f317a6e707a622e6a7067) :-)) typeclass for Haskell students is [Control.Monad](https://hackage.haskell.org/package/base/docs/Control-Monad.html). It defines basic operations over a monad, a term from category theory. From the perspective of a Haskell programmer, however, it is best to think of a monad as an "abstract datatype of actions". Haskell's `do` expressions provide a convenient syntax for writing monadic expressions. This time we will start Monads (operations, laws, basic behavior, etc.) and next time we will get deeper with some more practical use-cases.

```haskell
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b  -- compose two actions, passing the result
  (>>)   :: m a -> m b -> m b         -- compose two actions, discarding the result
  return :: a -> m a                  -- inject a value into the monadic type.

-- (<**>):: f a -> f (a -> b) -> f b  -- from Controll.Applicative
-- (*>)  :: f a -> f b -> f b         -- from Controll.Applicative
-- pure  :: a -> f a                  -- from Controll.Applicative
```

Function `return` works just as `pure` in `Applicative`. Why having two same functions? Historically; PureScript, for instance, has just `pure` both for the Applicative and Monad.

`>>=` is bind, which takes a monadic value (again, this is some "wrapped value") and a function, which takes an unwrapped value and transforms it into a monadic value.

`>>` is a sequencing operator, which "passes" computation from monad to another.

Again, there are functions `liftM` and laws:

```haskell
-- identity
return a >>= k == k a
m >>= return   ==  m
-- associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

```
Prelude Control.Monad> (Just 5) >> (Just 10)
Just 10
Prelude Control.Monad> (Just 5) >>= \x -> (Just (x+10))
Just 15
Prelude Control.Monad> return 5 >>= \x -> (Just (x+10))
Just 15

Prelude Control.Monad> (Left "err") >>= (\x -> return (x+10))
Left "err"
```

#### Do syntax

Monads in Haskell are so useful that they got their own special syntax called `do` notation. We first introduced it way back in the "Simple input and output" chapter. There, we used it to sequence input/output operations, but we hadn't introduced monads yet. Now, we can see that IO is yet another monad.

Imagine we have a sequence operation like this:

```haskell
Prelude> Just 7 >>= (\x -> Just (show x ++ "!"))
Just "7!"
Prelude> Just 7 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
Just "7!"
Prelude> print 3 >> print 5 >> print 7
3
5
7
```

Haskell provides the `do` notation so we can avoid writing all the lambdas:

```haskell
foo :: Maybe String
foo = do
    x <- Just 7
    y <- Just "!"
    Just (show x ++ y)
```

or

```haskell
main = do
    print 3
    print 5
    print 7
```

In `do`, you can use basic sequencing `>>`, `>>=` operator by binding `<-` and `let` instead of `let-in`:

```haskell
main = do
    putStrLn "Enter name:"
    name <- getLine                       -- getLine >>= (\name -> ...)
    putStrLn ("Hello, " ++ name ++ "!")
    let answer = 42                       -- let answer = 42 in (...)
    putStrLn "The answer to life, the universe and everything is..."
    print answer                          -- let and binding cannot be the last in do!
```

#### Loops

You might hear that "do" provides an imperative-like way of programming... That's true but it is really just *imperative-like* from a visual point of view, it is still purely functional! But even in math and functions, you can introduce something like `for` or `while` loops. When you want to compute some result like factorial, sum, length of a list it is natural to use recursion. With actions, it might give more sense to use loops (even when they are actually done by recursion):

```haskell
import System.Random
import Control.Monad.Loops

promptAnswer :: IO Int
promptAnswer = do
            putStrLn "Guess the answer: "
            x <- getLine
            return (read x)

guessAnswer :: Int -> IO Bool
guessAnswer x = do
             guess <- promptAnswer
             return (guess /= x)

main = do
    putStrLn "Welcome to GUESS 1 to 10 game"
    answer <- randomRIO (1, 10)
    whileM_ (guessAnswer answer) $ do         -- whileM_ :: Monad m => m Bool -> m a -> m ()
        putStrLn "Incorrect!"
    putStrLn "Good job!"
```

For other loops, visit [Control.Monad.Loops](https://hackage.haskell.org/package/monad-loops/docs/Control-Monad-Loops.html) and this [article](https://conscientiousprogrammer.com/blog/2015/12/11/24-days-of-hackage-2015-day-11-monad-loops-avoiding-writing-recursive-functions-by-refactoring/).

#### Monads in category theory

Again, monad comes from math and more specifically from category theory. A monad is a special type of functor, from a category to that same category (i.e., it is *endofunctor*), that supports some additional structure. Monad is a functor *M: C → C* with two morphisms for every object *X* from *C*:

1. *unit: X → M(X)* ~ `return :: Monad m => a -> m a`
2. *join: M(M(X)) →  M(X)* ~ `(>>=) :: Monad m => m a -> (a -> m b) -> m b`


## Task assignment

The homework to practice working with typeclasses is in repository [MI-AFP/hw05](https://github.com/MI-AFP/hw05).

## Further reading

* [Haskell: Polymorphism](https://wiki.haskell.org/Polymorphism)
* [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
* [Haskell: OOP vs type classes](https://wiki.haskell.org/OOP_vs_type_classes)
* [WikiBooks Haskell: Classes and types](https://en.wikibooks.org/wiki/Haskell/Classes_and_types)
* [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
* [Haskell and Category Theory](https://en.wikibooks.org/wiki/Haskell/Category_theory)
* [Category Theory for Programmers by Bartosz Milewski](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface)
* [LYAH: Functors, Applicative Functors and Monoids](http://learnyouahaskell.com/functors-applicative-functors-and-monoids)
* [LYAH: A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads)
* [Haskell: Monad](https://wiki.haskell.org/Monad)
