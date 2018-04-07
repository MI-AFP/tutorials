# Common typeclasses I

Now we are going to spend some time with predefined and important typeclasses that capture important concepts in Haskell that are widely used in many projects. Typeclass always says something about the structure of type and what you can do with that. Also, there are some laws and it is very tightly related to math -- specifically the algebra and the category theory.

After learning common typeclasses, it is not just easier to use them and understand a code written by other developer, but it also helps with designing own custom typeclasses. You can always find out more about typeclass and instances with GHCi `:info` command.

## Intro: Mathematical foundations

The relation between math and Haskell is very strong. You can observe it everywhere. Haskell functions are very similar to mathematical functions when you talk about their type, definitions with `let` and `where` keywords, guards with `otherwise`, function compositions, and so on. In this tutorial we are going to see this relation even more -- with typeclasses that come from mathematical world. You should be already familiar with [basic abstract algebra](https://en.wikipedia.org/wiki/Algebra#Abstract_algebra) (esp. algebraic structures with a single binary operation).

When getting into math, you should know that Haskell is based not just on the basic algebra, set theory, and logic, but also on the category theory. In order to sometimes mention the relation, we will briefly explain what it is about. If you want to know more, please refer to some mathematical tutorials on your own.

### Category theory

Category theory is a higher abstraction level over *Monoid*, *Semigroup*, and similar algebraic structures. Actually, it is so abstract that it can describe so many things ... that it is very hard to comprehend. The "best practice" among programmers is to learn it bit by bit and return to it from time to time until things "click" together. This "click" means putting together the theory and its practical applications.

A **category** is a structure or collection *C* with three components:

* a collection of **objects**, *ob(C)*,
* a collection of **morphisms**, *hom(C)*, that ties two objects together; sometimes they are called **arrows**,
* a **composition** of morphisms (similar to function composition).

There are many categories, for example, **Set** category has all possible sets as objects, standard functions as morphisms, and classical function composition. There are also three laws:

1. the composition of category must be associative (i.e., *f ∘ (g ∘ h) = (f ∘ g) ∘ h*),
2. the category needs to be closed under the composition operation (i.e., for all applies *h = f ∘ g ⇒ h ∈ C*),
3. for every object *A ∈ ob(C)* there is an identity function *idA: A → A*, *idA ∈ hom(C)*.

### The Hask category

In Haskell, we have the **Hask** category where:

* *ob(Hask)* are **types** (`Char`, `Int`, `Double`, `[Integer]`, `Person`, `Int -> String`, etc.),
* *hom(C)* are **functions** (`show`, `id`, `length`, `words`, `flip`, `reverse`, etc.),
* composition is **function composition** `(.)`.

The identity function is for every *o ∈ ob(Hask)* the polymorphic `id` function. The associativity of composition is assured and in *hom(C)* there are all the functions, even those created by composition. That's it for now -- now we will show some typeclasses, their laws and come back to **Hask** when necessary...

## Semigroup, Monoid, ...

Monoid is the most simple typeclass we will learn. You can recall the [monoid](https://en.wikipedia.org/wiki/Monoid) from the algebra -- it is an algebraic structure with one binary operation that is associate and there is also one identity element. The same goes for Haskell -- the operation is called `mappend` and the identity is `mempty` (first letter `m` if for **m**onoid).

Since `base-4.11.0.0`, the `Monoid` is subclass of `Semigroup` (just like in algebra). `Semigroup` defines just binary operation which is associative. In Haskell, the binary operation is `(<>)` (infixr 6)

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

* synonym for `mappend` is `(<>)` so you can simply use it as operator `x <> b` (notice that it is not the same as not-equals in other languages),
* multiple newtypes for specifying monoid for basic type, like `Sum` and `Product` for numeric types, `All` and `Any` for booleans, `First` and `Last` for maybes and few more.


```
Prelude> import Data.Monoid
Prelude Data.Monoid> :info Sum
newtype Sum a = Sum {getSum :: a} 	-- Defined in ‘Data.Monoid’
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

### Example: textual types

One of very practical usages of `mappend` is string concatenation, which is independent on its concrete implementation:

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

### Example: Maybe

From `:info Monoid`, we can see that `Maybe a` is instance of `Monoid` iff (=if and only if) `a` is instance of `Monoid`. Then, the `(<>)` is "propagated" inside and obviously the identity is `Nothing`.

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

### Verify the laws

As said, there are some laws (identity and associativity in this case) that should be valid, but the compiler can not enforce it. Whenever you are introducing your own instance of `Monoid` or other structure with some laws that are expected to be valid, use tests to prove it. One way is to write the properties on your own. The second and better one is to use [checkers](https://hackage.haskell.org/package/checkers), where many standard properties are prepared for you...

### Others from basic algebra

Apart from basic `Monoid` from algebra, there are also other variants. You might find interesting to learn more about:

* [semigrupoids](https://hackage.haskell.org/package/semigroupoids/docs/Data-Groupoid.html) (Semigrupoid, Grupoid),
* [groups](https://hackage.haskell.org/package/groups/docs/Data-Group.html) (Group, Abelian),
* etc.

It is possible to write own instances of `Monoid` or other typeclasses. However, mind that compiler *won't* check if laws are valid in your instance. For such checks you can use testing frameworks (esp. property testing), which will be covered later on.

## Functor

A functor is a way to apply a function on values inside some structure, while the structure remains intact. For example, if you want to change values in a list, tree or in Either without dealing with complexity and internal structure.

```haskell
class Functor f where                -- f is type constructor of kind * -> *
  fmap :: (a -> b) -> f a -> f b
```

The definition says that there is a function `fmap` which applies a function of type `a -> b` on elements in functor `f` with inner type `a` and the result will be functor `f` with inner type `b`. Moreover there are two laws:

```haskell
-- identity (fmap doesn't do nothing more than applying given function)
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
Right 10   -- changed, because "Either c" is functor for whatever "c" - it doesnt care
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

These examples might seem bit too simple, but you can have any instance of `Functor` without knowing the structure and implementation of it and affect what is inside by these two (four if counting also flipped) simple operators.

### Lifting

[Lifting](https://wiki.haskell.org/Lifting) is a concept which allows you to transform a function into a corresponding function within another (usually more general) setting. Lifting is again concept taken from mathematics and category theory (see [wikipedia](https://en.wikipedia.org/wiki/Lift_(mathematics)).

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

### Functors on Hask category

In mathematics, a functor is a type of mapping between categories arising in category theory. Functors can be thought of as homomorphisms between categories. In the category of small categories, functors can be thought of more generally as morphisms. ([wikipedia](https://en.wikipedia.org/wiki/Functor))

We have some categories which have objects and morphisms that relate our objects together. Functor *F: C → D* relates categories *C* and *D* together - it is a transformation between categories:

- maps every object *A* from category *C* to object *F(A)* in category *D*
- maps every morphism *f: A → B* from category *C* to morphism *F(f): F(A) → F(B)* in category *D*

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b    -- maps A->B to F(A) -> F(B) as well as A to F(A)
```

There is also identity for functions and they must be homomorphic that, of course, apply for functors in Haskell:

```haskell
fmap id == id

fmap (f . g) = fmap f . fmap g
```

## Applicative

Another important typeclass is [Control.Applicate](https://hackage.haskell.org/package/base/docs/Control-Applicative.html). Notice that it is not "Data" anymore, but "Control" instead! It is an intermediate structure between a `Functor` and a `Monad`. It is simpler than `Monad`, not so powerful, but sufficient in many use cases, and also easier to understand - it is **Applicative functor**.

In fucntor, we applied a function over a "wrapped" value to get a resulting "wrapped" value. In applicative, we have the function wrapped, as well:

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

### Actions vs. functions

In Haskell terminology, we call `Functor f => f a` an **action**. Actions have the power to do some side effect but not necessarily (e.g., `Just "no effect"`). For example, `liftA2` is described as function that lifts a binary function to actions. Special sort of actions are I/O actions with do something with input and output, but there can be also other actions making side effects.

## Monad

The most famous (and scary :-) typeclass for Haskell students is [Control.Monad](https://hackage.haskell.org/package/base/docs/Control-Monad.html). It defines basic operations over a monad, a term from category theory. From the perspective of a Haskell programmer, however, it is best to think of a monad as an "abstract datatype of actions". Haskell's `do` expressions provide a convenient syntax for writing monadic expressions.

```haskell
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b  -- compose two actions, passing the result
  (>>)   :: m a -> m b -> m b         -- compose two actions, discarding the result
  return :: a -> m a                  -- inject a value into the monadic type.

-- (<**>):: f a -> f (a -> b) -> f b  -- from Controll.Applicative
-- (*>)  :: f a -> f b -> f b         -- from Controll.Applicative
-- pure  :: a -> f a                  -- from Controll.Applicative
```

Function `return` works just as `pure` in `Applicative`. Why having two same functions? Historically; PureScript for instance has just `pure` both for the Applicative and Monad.

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

### Do syntax

Using `do` blocks as an alternative monad syntax was first introduced way back in the :"Simple input and output" chapter. There, we used do to sequence input/output operations, but we hadn't introduced monads yet. Now, we can see that IO is yet another monad.

Imagine we have a sequence operation like this:

```haskell
    putStr "Hello" >>
    putStr " " >>
    putStr "world!" >>
    putStr "\n"
```

Now we can chain it using do:

```haskell
main = do
   { putStr "Hello"
   ; putStr " "
   ; putStr "world!"
   ; putStr "\n" }
```

or

```haskell
main = do
    putStr "Hello"
    putStr " "
    putStr "world!"
    putStr "\n"
```

This becomes translated to:

```haskell
action1 >>
do { action2
   ; action3 }
```

### Monads in category theory

Again, monad comes from math and more specifically from category theory. A monad is a special type of functor, from a category to that same category (i.e., it is *endofunctor*), that supports some additional structure. Monad is a functor *M: C → C* with two morphisms for every object *X* from *C*: 

1. *unit: X → M(X)* ~ `return :: Monad m => a -> m a`
2. *join: M(M(X)) →  M(X)* ~ `(>>=) :: Monad m => m a -> (a -> m b) -> m b`

## IO - What is it?

Haskell separates pure functions from computations where side effects must be considered by encoding those side effects as values of a particular type. Specifically, a value of type (IO a) is an action, which executed produces a value of type a.

Some examples:

```haskell
getLine :: IO String
putStrLn :: String -> IO () -- note that the result value is an empty tuple.
randomRIO :: (Random a) => (a,a) -> IO a
```

## Task assignment

The homework to practice typeclasses from this tutorial is in repository [MI-AFP/hw07](https://github.com/MI-AFP/hw07).

## Further reading

* [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
* [Haskell and Category Theory](https://en.wikibooks.org/wiki/Haskell/Category_theory)
* [Category Theory for Programmers by Bartosz Milewski](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface)
* [LYAH - Functors, Applicative Functors and Monoids](http://learnyouahaskell.com/functors-applicative-functors-and-monoids)
* [Haskell - Typoclassopedia](https://wiki.haskell.org/Typeclassopedia)
* [Haskell - Monad](https://wiki.haskell.org/Monad)
* [Haskell - IO Monad](https://wiki.haskell.org/Introduction_to_IO)
