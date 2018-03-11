# Common typeclasses I

Now we are going to spend some with predefined and important typeclasses which capture some important concepts in Haskell that are widely used in any types of projects. Typeclass always say something about the structure of type and what can you do with that. Also there are some laws and it is very tightly related to math and specifically with algebra and theory of categories.

After learning common typeclasses it is not just easier to use them and understand code written by other developer but it also helps with designing own custom typeclasses. You can always find more about typeclass and instances with GHCi `:info` command.

## Intro: Mathematical foundations

// math, category theory

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

*TODO*: IO Functor, lifting, forall

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

### IO Monad

Haskell separates pure functions from computations where side effects must be considered by encoding those side effects as values of a particular type. Specifically, a value of type (IO a) is an action, which if executed would produce a value of type a.

Some examples:

```haskell
getLine :: IO String
putStrLn :: String -> IO () -- note that the result value is an empty tuple.
randomRIO :: (Random a) => (a,a) -> IO a
```

## Task assignment

## Further reading

* [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
* [Monad](https://wiki.haskell.org/Monad)
* [IO Monad](https://wiki.haskell.org/Introduction_to_IO)
