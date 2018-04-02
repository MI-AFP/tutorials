# Common typeclasses I

Now we are going to spend some with predefined and important typeclasses which capture some important concepts in Haskell that are widely used in any types of projects. Typeclass always say something about the structure of type and what can you do with that. Also there are some laws and it is very tightly related to math and specifically with algebra and theory of categories.

After learning common typeclasses it is not just easier to use them and understand code written by other developer but it also helps with designing own custom typeclasses. You can always find more about typeclass and instances with GHCi `:info` command.

## Intro: Mathematical foundations

Relation between math and Haskell is very strong. You can observe it everywhere. Functions are very similar to mathematical functions when you talk about their type, definitions with `let` and `where` keywords, guards with `otherwise`, function compositions, and so on. In this tutorial we are going to see this relation even more - with typeclasses that come from mathematical world. You should be already familiar with basic abstract algebra (esp. algebraic structures with a single binary operation). 

When getting into math, you should know that Haskell is based not just on the basic algebra, set theory, and logics, but also on the category theory. In order to sometimes mention the relation we will briefly explain what is it about. If you want to know more, please lookup some mathematical tutorials on your own.

### Category theory

Category theory is on higher abstract level than *Monoid*, *Semigroup*, and similar algebraic structures. A **category** is also a structure or collection *C* with three components:

* a collection of **objects**, *ob(C)*,
* a collection of **morphisms**, *hom(C)*, that ties two objects together and sometimes they are called **arrows**,
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

The identity function is for every *o ∈ ob(Hask)* the polymorphic `id` function. The associativity of composition is assured and in *hom(C)* are all the functions, even those created by composition. That's it - now we will show some typeclasses, their laws and come back to **Hask** when necessary...

## Monoid (and others from basic algebra)

Monoid is the most simple typeclass we will learn. You can recall the [monoid](https://en.wikipedia.org/wiki/Monoid) them from algebra - it is algebraic structure with one binary operation which is associate and there is also one identity element. Same goes for Haskell - the operation is called `mappend` and identity is `mempty` (first letter `m` if for **m**onoid).

```haskell
class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

The law of monoid says that `mappend` must be associative and `mempty` is real identity when working with `mappend`:

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

```haskell
-- TODO: Sum & Product
-- TODO: First & Last
```

Of course there are not just `Monoid` from basic algebra. You might find interesting to learn more about:

* [Data.Semigroup](https://hackage.haskell.org/package/base/docs/Data-Semigroup.html) (no identity as is in Monoid),
* [Data.Group, Data.Abelian](https://hackage.haskell.org/package/groups-0.4.0.0/docs/Data-Group.html) (inversions and commutative operation).

It is possible to write own instances of `Monoid` or other typeclasses. Problem is that compiler won't check if laws are valid in your instance. For such checks you can use testing frameworks (esp. property testing) which will be covered later on.

## Functor

A functor is a way to apply a function on values inside some structure that we don’t want to change. For example if you want to change the values in the list, tree or in either without dealing with complexity and internal structure.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

The definition says that there is a function `fmap` which applies a function of type `a -> b` on elements in functor `f` with inner type `a` and result will be functor `f` with inner type `b`. Moreover there are two laws:

```haskell
-- identity (fmap doesn't do nothing more than applying given function)
fmap id == id
-- composition
fmap (f . g) == fmap f . fmap g
```

Let's try it:

```
-- TODO play with functors
```

Just as with Monoid, you can take a look at the documentation of [Data.Functor](https://hackage.haskell.org/package/base/docs/Data-Functor.html). Again, there is operator alias, in this case `(<$>)` for `fmap`. There are two more similar - `<$` and `$>` (just flipped `<$`).

```
-- TODO play with functors and operators
```

### Lifting

### Functors on Hask category

### forall quantification

## Applicative

Next important typeclass is [Control.Applicate](https://hackage.haskell.org/package/base/docs/Control-Applicative.html). Notice that it is not "Data" anymore, but "Control" instead! It is intermediate structure between a `Functor` and a `Monad`. It is simpler than `Monad`, not so powerful, but sufficient in many use cases, and also easier to understand.

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Function `pure` only lifts something into applicative structure `f`. The more interesting part is the "tie-fighter" operator `<*>` which applies lifted function over applicative. You can find out in the documentation following similar functions and partial functions as in [Data.Functor](https://hackage.haskell.org/package/base/docs/Data-Functor.html):

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
-- TODO play with applicative and operators
```

### Lifting

## Monad

The most famous and scary typeclass for Haskell students is [Control.Monad](https://hackage.haskell.org/package/base/docs/Control-Monad.html). It defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. From the perspective of a Haskell programmer, however, it is best to think of a monad as an abstract datatype of actions. Haskell's do expressions provide a convenient syntax for writing monadic expressions.

```haskell
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a
```

Function `return` work just as `pure` in `Functor`. The `>>` is sequencing operator and `>>=` is bind. Also there are functions `liftM` and laws:

```haskell
-- identity
return a >>= k == k a
m >>= return   ==  m
-- associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

```
-- TODO play with applicative and operators
```

### Do syntax

Using `do` blocks as an alternative monad syntax was first introduced way back in the Simple input and output chapter. There, we used do to sequence input/output operations, but we hadn't introduced monads yet. Now, we can see that IO is yet another monad.

Following are equivalent:

```haskell
main =
    putStr "Hello" >>
    putStr " " >>
    putStr "world!" >>
    putStr "\n"
```

```haskell
main = do
   { putStr "Hello"
   ; putStr " "
   ; putStr "world!"
   ; putStr "\n" }
```

```haskell
main = do
    putStr "Hello"
    putStr " "
    putStr "world!"
    putStr "\n"
```

### Monads in category theory

### IO Monad

Haskell separates pure functions from computations where side effects must be considered by encoding those side effects as values of a particular type. Specifically, a value of type (IO a) is an action, which if executed would produce a value of type a.

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
* [Haskell - Typoclassopedia](https://wiki.haskell.org/Typeclassopedia)
* [Haskell - Monad](https://wiki.haskell.org/Monad)
* [Haskell - IO Monad](https://wiki.haskell.org/Introduction_to_IO)
