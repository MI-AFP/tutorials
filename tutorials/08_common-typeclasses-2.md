# Common typeclasses 2

## Foldable

A Foldable type is a container. The class does not require Functor superclass in order to allow containers like Set or StorableVector that have additional constraints on the element type. But many interesting Foldables are also Functors. A foldable container is a container with the added property that its items can be 'folded' to a summary value. Recall what `foldr` and `foldl` do...

In other words, it is a type which supports "foldr". Once you support foldr, of course, it can be turned into a list, by using `toList = foldr (:) []`. This means that all Foldables have a representation as a list, but the order of the items may or may not have any particular significance. However, if a Foldable is also a Functor, parametricity and the Functor law guarantee that `toList` and `fmap` commute. Further, in the case of [Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html), there is a well defined order and it is exposed as expected by `toList`. A particular kind of fold well-used by Haskell programmers is `mapM_`, which is a kind of fold over `(>>)`, and Foldable provides this along with the related `sequence_`.

```haskell
-- TODO: example
```

## Traversable

A Traversable type is a kind of upgraded Foldable. Where Foldable gives you the ability to go through the structure processing the elements (`foldr`) but throwing away the shape, Traversable allows you to do that whilst preserving the shape and, e.g., putting new values in. Traversable is what we need for mapM and sequence : note the apparently surprising fact that the "_" versions are in a different typeclass.

```haskell
-- TODO: example
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

## Monad Transformers

## Arrow (and lifting)

Arrows are a new abstract view of computation, defined by John Hughes. They serve much the same purpose as monads -- providing a common structure for libraries -- but are more general. In particular they allow notions of computation that may be partially static (independent of the input) or may take multiple inputs. If your application works fine with monads, you might as well stick with them. But if you're using a structure that's very like a monad, but isn't one, maybe it's an arrow.

```haskell
class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b,d) (c,d)
-- TODO: example
```

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

* [State monad](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
* [Monadic parsing combinators](http://eprints.nottingham.ac.uk/223/1/pearl.pdf)
* [Arrows](https://www.haskell.org/arrows/)
* [Lens](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)

