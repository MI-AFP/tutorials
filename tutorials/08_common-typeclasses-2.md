# Common typeclasses 2

## Foldable

Recall the time when we were talking about folds... The `Foldable` type class provides a generalisation of list folding (`foldr` and friends) and operations derived from it to arbitrary data structures. The class does not require Functor superclass in order to allow containers like Set or StorableVector that have additional constraints on the element type. But many interesting Foldables are also Functors. A foldable container is a container with the added property that its items can be 'folded' to a summary value. Recall what `foldr` and `foldl` do...

In other words, it is a type which supports "foldr". Once you support foldr, of course, it can be turned into a list, by using `toList = foldr (:) []`. This means that all foldables have a representation as a list, but the order of the items may or may not have any particular significance. However, if a Foldable is also a Functor, parametricity and the Functor law guarantee that `toList` and `fmap` commute. Further, in the case of [Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html), there is a well defined order and it is exposed as expected by `toList`. A particular kind of fold well-used by Haskell programmers is `mapM_`, which is a kind of fold over `(>>)`, and Foldable provides this along with the related `sequence_`.

```haskell
import Data.Foldable

class Foldable (t :: * -> *) where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
```

This class is very straight-forward, has no specific laws, but is very powerful as we've already know... It allows you to create new or use various containers with same generic functions like `null`, `length`, `elem`, `minimum`, `maximum`, and others seamlessly and without any problems. For more, see [Data.Foldable](https://hackage.haskell.org/package/base/docs/Data-Foldable.html).

### Specialized folds

Aside functions defined in `Foldable` typeclass, there are some additional specialized folds that can be very useful and avoid reinventing the wheel in your code:

```haskell
concat :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

and :: Foldable t => t Bool -> Bool
or :: Foldable t => t Bool -> Bool

any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
```

### Foldable and Applicative

Then, there are some specialized functions that are useful when you have `Applicative` objects in a `Foldable` structure or want to apply them over a `Foldable` structure. *(Notice the underscores)* 

```haskell
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()      -- flip . traverse_

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()

asum :: (Foldable t, Alternative f) => t (f a) -> f a                 -- Alternative is described in this tutorial
```

```
Prelude Data.Foldable> traverse_ print [1..3]
1
2
3
Prelude Data.Foldable> :t traverse_ print [1..3]
traverse_ print [1..3] :: IO ()
Prelude Data.Foldable> for_ [1..3] print
1
2
3
Prelude Data.Foldable> :t for_ [1..3] print
for_ [1..3] print :: IO ()
Prelude Data.Foldable> sequenceA_ [print 1, print 2, print 3]
1
2
3
Prelude Data.Foldable Data.Traversable> sequenceA_ [getLine, getLine, getLine]
ahoj
hello
ciao
Prelude Data.Foldable> :t sequenceA_ [getLine, getLine, getLine]
sequenceA_ [getLine, getLine, getLine] :: IO ()
Prelude Data.Foldable> asum [print 1, print 2, print 3]
1
Prelude Data.Foldable> :t asum [print 1, print 2, print 3]
asum [print 1, print 2, print 3] :: IO ()
Prelude Data.Foldable> asum [Just "a", Nothing, Just "b"]
Just "a"
Prelude Data.Foldable> asum [Nothing, Just "b"]
Just "b"
```

### Foldable and Monad

Similarly, there are also same folds for `Monad`s, just naming is a bit different:

```haskell
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()  -- flip . mapM_

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a          -- Alternative is described in this tutorial
```

## Traversable

A `Traversable` type is a kind of upgraded `Foldable` with use of `Functor`. Where Foldable gives you the ability to go through the structure processing the elements (*catamorphism*) but throwing away the shape, `Traversable` allows you to do that whilst preserving the shape and, e.g., putting new values in. Traversable is what we need for `mapM` and `sequence`: note the apparently surprising fact that the versions ending with underscore (e.g., `mapM_`) are in a different typeclass - in `Foldable`.

```haskell
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
```

`Traversable` has, unlike `Foldable`, a few laws (naturality, identity, composition, ...). For more, see [Data.Traversable](https://hackage.haskell.org/package/base/docs/Data-Traversable.html).

### No more underscore

Indeed, some functions from `Foldable` are in `Traversable` without trailing `_` and it means "preserving the structure":

```haskell
for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
```

```
Prelude Data.Traversable> traverse print [1..3]
1
2
3
[(),(),()]
Prelude Data.Traversable> for [1..3] print
1
2
3
[(),(),()]
Prelude Data.Foldable Data.Traversable> sequenceA [print 1, print 2, print 3]
1
2
3
[(),(),()]
Prelude Data.Foldable Data.Traversable> sequenceA [getLine, getLine, getLine]
ahoj
hello
ciao
["ahoj","hello","ciao"]
```

## State

As you might have noticed, in Haskell it is not so easy to maintain state because the lack of immutable global variable. For working with state is here the State monad used as follows:

```haskell
newtype State s a = State { runState :: s -> (a, s) }
-- TODO: example
```

## Parser

Typical example where you use State is when you want to parse something. So for this purpose we have Parser monadic combinator as follows:

```haskell
newtype Parser a = Parser (String -> [(a,String)])
-- TODO: example
```

## Alternative and MonadPlus

We are used use type `Maybe` when the result can be something or fail/nothing, and lists when there are many results of the same type and arbitrary size. Typeclasses `Alternative` and `Monad` are here to provide generic way of aggregating results together. `Maybe` and `[]` are its instances - read more: [Control.Applicative#Alternative](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative) and [Control.Monad#MonadPlus](https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:MonadPlus). You might find this very useful for [parsing](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus#Example:_parallel_parsing).

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
```

```
Prelude Control.Applicative> (Just 5) <|> (Just 7)
Just 5
Prelude Control.Applicative> Nothing <|> (Just 7)
Just 7
Prelude Control.Applicative> [1..5] <|> [3..7]
[1,2,3,4,5,3,4,5,6,7]
Prelude Control.Applicative> getLine <|> getLine
a
"a"
```

### `guard` (don't mix with guards!)

An interesting function related to `Alternative` is `guard :: Alternative f => Bool -> f ()`. What it does? It works like guard in sequence of actions!

```haskell
import Control.Monad

getIntGt100 :: IO Int
getIntGt100 = do
                putStrLn "Enter number > 100:"
                number <- (read :: String -> Int) <$> getLine
                guard (number > 100)
                return number

main = do
         x <- getIntGt100
         print "OK, it is bigger than 100"
```

## Monad Transformers

## Category and Arrow

Recall what was told about Category Theory in the last tutorial. In Haskell, we have also typeclasses `Category` and `Arrow` (Do you remember? Alias for *morphisms*.). We mention it here just as interesting part of Haskell and let you explore it if you are interested...

Arrows are a new abstract view of computation, defined by John Hughes. They serve much the same purpose as monads -- providing a common structure for libraries -- but are more general. In particular they allow notions of computation that may be partially static (independent of the input) or may take multiple inputs. If your application works fine with monads, you might as well stick with them. But if you're using a structure that's very like a monad, but isn't one, maybe it's an arrow. (see [https://www.haskell.org/arrows/])

```haskell
class Category (cat :: k -> k -> *) where
  id  :: forall (a :: k). cat a a
  (.) :: forall (b :: k) (c :: k) (a :: k). cat b c -> cat a b -> cat a c
  {-# MINIMAL id, (.) #-}

class Category a => Arrow (a :: * -> * -> *) where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  (&&&) :: a b c -> a b c' -> a b (c, c')
  {-# MINIMAL arr, (first | (***)) #-}
```

A simple example (from [Haskell Wiki](https://wiki.haskell.org/Arrow_tutorial)):

```haskell
import Control.Category
import Control.Arrow

newtype SimpleFunc a b = SimpleFunc { runF :: (a -> b) }

instance Arrow SimpleFunc where
  arr f = SimpleFunc f
  first  (SimpleFunc f) = SimpleFunc (mapFst f)
                           where mapFst g (a,b) = (g a, b)
  second (SimpleFunc f) = SimpleFunc (mapSnd f)
                           where mapSnd g (a,b) = (a, g b)
 
instance Category SimpleFunc where
  (SimpleFunc f) . (SimpleFunc g) = SimpleFunc (f . g)
  id = arr id
```

```
*Main> func1 x = x + 2
*Main> func2 = show
*Main> func3 = reverse
*Main> arrow1 = SimpleFunc { run
runF        runKleisli
*Main> arrow1 = SimpleFunc { runF = func1 }
*Main> :type arrow1
arrow1 :: Num b => SimpleFunc b b
*Main> arrow2 = SimpleFunc { runF = func2 }
*Main> arrow3 = arr func3
*Main> :type arrow3
arrow3 :: Arrow a => a [a1] [a1]
*Main> arrow4 = first arrow1
*Main> runF arrow4 (2, 4)
(4,4)
*Main> runF arrow4 (2, "Hello")
(4,"Hello")
*Main> arrow5 = second arrow1
*Main> runF arrow4 (2, 4)
(4,4)
*Main> runF arrow5 (2, 4)
(2,6)
*Main> arrow6 = arrow1 *** arrow3
*Main> runF arrow6 (5, "Hello")
(7,"olleH")
*Main> arrow7 = arrow1 &&& arrow2
*Main> runF arrow7 5
(7,"5")
*Main> arrow8 = arrow1 >>> arrow2
*Main> runF arrow8 5
"7"
```

Good explanation with nice visualization is in the chapter [Understanding Arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows) at wikibooks.

## Lens (and Template Haskell intro)

The combinators in [Control.Lens](https://hackage.haskell.org/package/lens) provide a highly generic toolbox for composing families of getters, folds, isomorphisms, traversals, setters and lenses and their indexed variants.

A lens is a first-class reference to a subpart of some data type. For instance, we have `_1` which is the lens that "focuses on" the first element of a pair. Given a lens there are essentially three things you might want to do

1. View the subpart
2. Modify the whole by changing the subpart
3. Combine this lens with another lens to look even deeper

The first and the second give rise to the idea that lenses are getters and setters like you might have on an object. This intuition is often morally correct and it helps to explain the lens laws.

```haskell
-- TODO: example
```

## Task assignment

The homework to practice typeclasses from this tutorial is in repository [MI-AFP/hw08](https://github.com/MI-AFP/hw08).

## Further reading

* [Haskell - Foldable](https://en.wikibooks.org/wiki/Haskell/Foldable)
* [Haskell - Traversable](https://en.wikibooks.org/wiki/Haskell/Traversable)
* [Haskell - State monad](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
* [Haskell - Monad transformers](https://en.wikibooks.org/wiki/Haskell/Monad_transformers)
* [Haskell - Arrow tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
* [Haskell - Lenses and functional references](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references)
* [Haskell Wiki - Foldable and Traversable](https://wiki.haskell.org/Foldable_and_Traversable)
* [Haskell Wiki - State monad](https://wiki.haskell.org/State_Monad)
* [Monadic parsing combinators](http://eprints.nottingham.ac.uk/223/1/pearl.pdf)
* [LYAH - For a Few Monads More](http://learnyouahaskell.com/for-a-few-monads-more)
* [Arrows](https://www.haskell.org/arrows/)
* [Lens](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)

