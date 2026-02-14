# Typeclasses - custom and predefined

## Typeclasses

A **typeclass** is Haskell’s mechanism for **ad-hoc polymorphism**: it lets you write functions that work for *many different types*, as long as those types support a required set of operations.

In other words:

> A typeclass is a *contract* (a set of functions and laws) that a type may implement.

⚠️ Note: the word *class* here means a **mathematical class** (set-like collection of things), not an object-oriented class. In Haskell, typeclasses do not define data or state — they define *capabilities*.

### Why typeclasses matter (engineering view)

Typeclasses solve a very common engineering problem:

> I want one generic algorithm, but I need the input type to support certain operations.

For example:

- sorting needs ordering (`Ord`)
- printing needs formatting (`Show`)
- equality checks need equality (`Eq`)
- numeric algorithms need arithmetic (`Num`, `Fractional`, ...)

This is expressed directly in types:

```haskell
sort   :: Ord a  => [a] -> [a]
show   :: Show a => a -> String
(==)   :: Eq a   => a -> a -> Bool
(+)    :: Num a  => a -> a -> a
(/)    :: Fractional a => a -> a -> a
```

### Kinds

In Haskell, not everything at the type level is a *fully specified type*.

Some things are **type constructors** — meaning: they still need one or more type arguments before they become a real type you can have values of. Recall from the previous tutorial that `Maybe` is a type constructor, and `Maybe Int` is a fully specified type. The same goes for `Either`, `[]`, etc. This is related to partial application of type constructors, which is a powerful feature of Haskell.

A **kind** is simply the *type of a type-level thing*.

You can think of it as:

- **types** classify *values*
- **kinds** classify *types and type constructors*

The simplest kind is:

- `Type` (historically written as `*`)

It is the kind of normal, concrete types such as `Int`, `Bool`, or `Maybe Int`.

You can inspect kinds in GHCi:

```
ghci> :kind Int
Int :: *

ghci> :kind Maybe
Maybe :: * -> *

ghci> :kind Either
Either :: * -> * -> *

ghci> :kind (Either Int)
(Either Int) :: * -> *

ghci> :kind (Either Int String)
(Either Int String) :: *
```

Kinds behave almost exactly like function types:

* `*` is like a *value type*
* `* -> *` is like a function that takes a type and returns a type
* `* -> * -> *` is like a function that takes two types and returns a type

So:

* `Maybe` is not a type — it is a type constructor
* `Maybe Int` is a type
* `Either` is a type constructor of two arguments
* `Either Int` is a type constructor of one argument (partially applied)
* `Either Int String` is a type

This is the type-level version of partial application you already know from values. We will later on see how this is useful for typeclasses and polymorphism.

### Polymorphism

[Polymorphism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) (Greek πολύς “many” + μορφή “shape”) means writing code that works for multiple types.

In Haskell, you will encounter polymorphism in two everyday forms:

* **Parametric polymorphism** (unconstrained): This is the classic *generic* polymorphism: the code works for any type because it makes **no assumptions about that type**. In the following example, the function `length` works for any list of any type, and `id` works for any type at all; `a` and `b` are unconstrained type variables:

```haskell
length :: [a] -> Int
map :: (a -> b) -> [a] -> [b]
id :: a -> a
```

* **Ad-hoc polymorphism** (constrained): This is the kind of polymorphism that typeclasses provide: the code works for any type that **implements a certain interface** (i.e., supports certain operations). In the following example, `sort` works for any list of any type `a`, as long as `a` is an instance of the `Ord` typeclass (i.e., it supports ordering):

```haskell
(==)  :: Eq a   => a -> a -> Bool
sort  :: Ord a  => [a] -> [a]
show  :: Show a => a -> String
```

Unlike in many OO languages, this is not subtyping. It’s closer to dictionary-passing: the compiler chooses the right implementation based on the type and inserts it implicitly for you.

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

## Common typeclasses

These are the typeclasses you will encounter constantly in real Haskell code.

They describe *very basic structure* that many types naturally support:

- equality
- ordering
- textual representation
- enumeration
- bounds

Understanding these well is crucial before moving to more abstract ones.

### Deriving

Haskell can automatically generate instances for several standard typeclasses using `deriving`.

```haskell
data Person = Person
  { name :: String
  , age  :: Int
  }
  deriving (Eq, Ord, Show, Read)
```

This works when:

* all fields already have instances of those classes
* the instance can be derived structurally

Deriving is structural:

* `Eq` compares fields
* `Ord` compares fields lexicographically
* `Show` prints constructor syntax
* `Read` parses constructor syntax

If the generated behavior is what you want — use it. If not, write your own instance (you will see soon how).

### Eq — equality

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

This defines structural equality. Minimal requirement for custom instances: define either (==) or (/=).

**Laws** (that are expected but not enforced by the compiler), for all `x`, `y`, `z`:

* Reflexivity: `x == x`
* Symmetry: `x == y  ⇒  y == x`
* Transitivity: `(x == y && y == z) ⇒ x == z`

Example of custom instance:

```haskell
data User = User Int String

instance Eq User where
  (User id1 _) == (User id2 _) = id1 == id2
```

### Ord — ordering

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}

data Ordering = LT | EQ | GT
```

Notice that `Ord` is a subclass of `Eq`, so you need to have an instance of `Eq` to be able to create an instance of `Ord`. The minimal requirement for custom instances: define either `compare` or `(<=)`.

**Laws** (expected but not enforced), for all `x`, `y`, `z`:

* Totality: `compare x y` is always `LT`, `EQ`, or `GT`
* Transitivity: `x <= y && y <= z ⇒ x <= z`
* Consistency with `Eq`: `x == y ⇒ compare x y == EQ`

Again, custom instance:

```haskell
data Person = Person String Int

instance Ord Person where
  compare (Person _ age1) (Person _ age2) =
    compare age1 age2
```

### Show — textual representation as a String

```haskell
class Show a where
  show :: a -> String
```

This is for converting values to human-readable strings. The minimal requirement for custom instances: define `show`.

Derived `Show` produces a string that looks is a valid Haskell code. Custom instances can produce any string representation you want. But then you should preferably implement both Show and Read, they should round-trip: `read (show x) == x`. General recommendation: do not abuse `show` for pretty-printing.

### Read — parsing from String

```haskell
class Read a where
  read :: String -> a
```

This is for parsing values from strings. Derived `Read` expects the string to be in the same format as produced by `Show`. Typically, you write custom parsers using dedicated libraries if needed, rather than implementing `Read` directly.

### Enum — enumeration

```haskell
class Enum a where
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

data Day = Mon | Tue | Wed | Thu | Fri
  deriving (Enum, Show)

[Mon .. Fri]
-- [Mon,Tue,Wed,Thu,Fri]
```

Naturally, `Enum` is for types that have a sequential ordering. The minimal requirement for custom instances: define either `toEnum` and `fromEnum`, or `enumFrom` and `enumFromTo`. Derived `Enum` assigns consecutive integers starting from 0 to the constructors. Custom instances can define any mapping between the type and integers, as well as any enumeration behavior.

### Bounded — upper and lower bounds

```haskell
class Bounded a where
  minBound :: a
  maxBound :: a

data Direction = North | South | East | West
  deriving (Bounded, Enum)

minBound :: Direction
-- North

maxBound :: Direction
-- West
```

With this, you can enumerate all values of a type using `enumFrom minBound` or `enumFromTo minBound maxBound` (or `[ minBound .. maxBound ]`). Derived `Bounded` assigns the first constructor as `minBound` and the last constructor as `maxBound`. Custom instances can define any values as bounds. Notice that you need to specify the type when using `minBound` and `maxBound`, as they are polymorphic.


## Custom typeclasses

So far, we have used predefined typeclasses like `Eq`, `Ord`, and `Show`. But one of the most powerful features of Haskell is that **you can define your own typeclasses** to describe reusable abstractions.

A custom typeclass answers the question:

> What structure must a type provide to participate in this abstraction?

### When should you define a typeclass?

Use a typeclass when:

- multiple unrelated types should support the same *concept*
- you want to write generic algorithms constrained by capabilities
- the abstraction is about *behavior*, not data

Do **not** use a typeclass:

- when there will be only one instance
- when you need runtime dispatch
- when the behavior depends on configuration/state

Typeclasses are about **static capability-based polymorphism**.

### Basic syntax

A typeclass defines:

- a name
- a type variable
- a set of function signatures
- optional default implementations

```haskell
class Located a where
  getLocation :: a -> (Int, Int)
```

> Any type a that is an instance of Located must provide a function getLocation.

### Creating instances

```haskell
data NamedPoint = NamedPoint
  { pointName :: String
  , pointX    :: Int
  , pointY    :: Int
  }

instance Located NamedPoint where
  getLocation p = (pointX p, pointY p)
```

Now any function (including constructors) requiring `Located a` can work with `NamedPoint`.

### Default implementations

Typeclasses may define default behavior which instances can override. This allows you to provide a common implementation that works for many types, while still allowing customization when needed.

```haskell
class (Located a) => Movable a where
  setLocation :: (Int, Int) -> a -> a

  move :: (Int, Int) -> a -> a
  move (dx, dy) obj =
    let (x, y) = getLocation obj
    in setLocation (x + dx, y + dy) obj
```

Only `getLocation` (from `Located`) and `setLocation` must be implemented while `move` is automatically available.

This is extremely powerful:

* you define minimal core operations
* derive richer behavior from them

This pattern appears everywhere in the standard library.

### Minimal complete definition

Many typeclasses document a minimal complete definition. This is the smallest set of functions you must implement to create a valid instance. The rest can be derived from these. For example, for `Eq`, you can implement either `(==)` or `(/=)`, and the other will be automatically defined in terms of it.

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

This means:

* You can implement `(==)` and get `(/=)` for free.
* You can implement `(/=)` and get `(==)` for free.
* You can also implement both if you want, but it’s not required.

If you violate the minimal complete definition, compilation will fail. You can define such `MINIMAL` pragma in your own typeclasses to guide users on what they need to implement.

### Typeclass inheritance

A typeclass can inherit from another typeclass, meaning that any type that is an instance of the child class must also be an instance of the parent class. This allows you to build more complex abstractions on top of simpler ones.

```haskell
class Located a => Movable a where
  setLocation :: (Int, Int) -> a -> a
```

### One instance per type per class

> For each (Type, Typeclass) pair, there may be only one instance.

This ensures **coherence**: the compiler can always determine which implementation to use based on the type. This is different from some other languages that allow multiple implementations and require explicit disambiguation.

If you need multiple behaviors for the same type, you can use **newtypes** to create distinct types that wrap the original type, and then define different instances for those newtypes.

```
newtype ByLength = ByLength String

instance Eq ByLength where
  s1 == s2 = length s1 == length s2
```

This allows alternative behavior safely without breaking coherence.

### Multi-parameter typeclasses

Sometimes behavior can relate to multiple types:

```haskell
class Convertible a b where
  convert :: a -> b
```

This requires the `MultiParamTypeClasses` language extension. It allows you to define relationships between different types, but it also introduces complexity in instance resolution, so it should be used with care. You may encounter this pattern in libraries that need to express conversions or interactions between different types.

### Mental model

A typeclass defines:

* a **set of types**
* that provide certain **operations**
* and obey certain *laws**

In mathematical language:

> A typeclass describes a structure.

In engineering language:

> A typeclass describes a capability contract.


## Mathematical typeclasses

The typeclasses in this section describe **algebraic structure**. Unlike `Eq` or `Show`, which describe basic capabilities,
these typeclasses describe deeper structure about:

- combination
- transformation
- composition
- sequencing

They are the foundation of idiomatic Haskell.

### Semigroup and Monoid — combining values

The simplest algebraic structure in Haskell is a **Semigroup**.

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

This defines a single binary operation `(<>)` that takes two values of type `a` and returns a new value of type `a`. The operation must be associative, meaning that the grouping of operations does not matter:

```haskell
(x <> y) <> z == x <> (y <> z)
```

A `Monoid` extends `Semigroup` by adding an identity element.

```haskell
class Semigroup a => Monoid a where
  mempty  :: a
  mconcat :: [a] -> a
```

This means that there is a value `mempty` such that for any value `x` of type `a` (identity laws):

```haskell
mempty <> x == x
x <> mempty == x
```

You already know some instances of `Monoid`:

- `Sum` and `Product` for numbers (with addition and multiplication, respectively)
- `[]` for lists (with concatenation)
- `Maybe` for optional values (with `First` and `Last` variants)
- `String` (with concatenation) just as lists
- `All` and `Any` for booleans (with conjunction and disjunction, respectively)

Apart from basic `Monoid` from algebra, there are also other variants. You might find interesting to learn more about:

* [semigroupoids](https://hackage.haskell.org/package/semigroupoids/docs/Data-Groupoid.html) (Semigroupoid, Grupoid),
* [groups](https://hackage.haskell.org/package/groups/docs/Data-Group.html) (Group, Abelian),
* etc.

It is possible to write own instances of `Monoid` or other typeclasses. However, mind that compiler *won't* check if laws are valid in your instance. For such checks, you can use testing frameworks (esp. property testing), which will be covered later on.

### Functor — structure-preserving transformation

A `Functor` lets you **apply a function inside a structure**.

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
ghci> fmap (*2) [1..5]   -- just like map!
[2,4,6,8,10]

ghci> fmap (show . (*2)) [1..5]   -- just like map!
["2","4","6","8","10"]

ghci> fmap (*2) (Just 7)
Just 14

ghci> fmap (*2) Nothing
Nothing

ghci> fmap (+10) (Left 5)
Left 5     -- no change!

ghci> fmap (+10) (Right 5)
Right 10   -- changed, because "Either c" is functor for whatever "c" - it doesn't care
```

Just as with Monoid, you can take a look at the documentation of [Data.Functor](https://hackage.haskell.org/package/base/docs/Data-Functor.html). Again, there is an operator alias, in this case `(<$>)` for `fmap` (denoting a sort of "wrapped" or "inside" apply). There are two more -- `<$` and `$>` (just flipped `<$`). Flipped version of `(<$>)` is `(<&>)`.

```
ghci> (*2) <$> [1..5]
[2,4,6,8,10]

ghci>  [1..5] <&> (*2)
[2,4,6,8,10]

ghci> 2 <$ [1..5]
[2,2,2,2,2]

ghci> [1..5] $> 2
[2,2,2,2,2]

ghci> (*2) <$> (Just 7)
Just 14

ghci> 2 <$ (Just 7)
Just 2

ghci> (Just 7) $> 2
Just 2
```

These examples might seem a bit too simple, but you can have any instance of `Functor` without knowing the structure and implementation of it and affect what is inside by these two (four if counting also flipped) simple operators.

### Applicative

`Applicative` extends `Functor` and functions now can be wrapped as well:

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
ghci> linfunc x = 2 * x + 10
ghci> (Just lin
lines    linfunc
ghci> (Just linfunc) <*> (Just 5)
Just 20
ghci> pure linfunc <*> (Just 5)
Just 20
ghci> pure linfunc <*> Nothing
Nothing
ghci> Nothing <*> (Just 5)
Nothing
ghci> (Just 5) <* (Just 10)
Just 5
ghci> (Just 5) *> (Just 10)
Just 10

ghci> pure linfunc <*> (Left 7)
Left 7
ghci> pure linfunc <*> (Right 15)
Right 40
ghci> (Right 5) *> (Left 10)
Left 10
ghci> (Right 5) <* (Left 10)
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

### Monad — sequencing dependent effects

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
ghci> Just 7 >>= (\x -> Just (show x ++ "!"))
Just "7!"
ghci> Just 7 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
Just "7!"
ghci> print 3 >> print 5 >> print 7
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

### Intution for Functors, Applicatives, and Monads

* **Functor** = transform values inside a structure
* **Applicative** = combine independent computations in a structure
* **Monad** = combine dependent computations in a structure

## Haskell and Category Theory

Throughout this chapter, we have repeatedly mentioned algebra and category theory. Let’s now connect the dots clearly.

## The Hask category

Category theory studies abstract structure. A **category** consists of:

1. Objects
2. Morphisms (arrows) between objects
3. Composition of morphisms
4. Identity morphisms

In Haskell, there is a natural (informal) category called **Hask**:

- Objects → types (`Int`, `Bool`, `Maybe Int`, ...)
- Morphisms → functions (`Int -> String`, etc.)
- Composition → `(.)` (function composition, associative)
- Identity → `id` (for each type, there is an identity function)

## Functor in categorical terms

In category theory, a **functor** is a mapping between categories that:

- maps objects to objects
- maps morphisms to morphisms
- preserves identity
- preserves composition

In Haskell:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

This does exactly that:

* maps type `a` to `f a`
* maps function `a -> b` to `f a -> f b`
* preserves identity (`fmap id == id`)
* preserves composition (`fmap (f . g) == fmap f . fmap g`)

So Functor is not just inspired by category theory — it is the direct encoding of the categorical definition.

### Monad in categorical terms

In category theory, a monad is:

* an endofunctor (a functor from a category to itself)
* equipped with two natural transformations:

  * unit
  * multiplication (join)

In Haskell:

* endofunctor → `m :: * -> *`
* unit → `return :: a -> m a`
* multiplication → `join :: m (m a) -> m a`

The bind operator:

```haskell
(>>=) :: m a -> (a -> m b) -> m b
```

is just a convenient formulation of `join`.

Monad laws correspond exactly to categorical monad laws. You do not need the formalism — but you should recognize that Monads are not just a random invention, but a deep mathematical structure that has been studied for decades.

### Does it matter?

Category theory gives Haskell programmers a powerful language for describing and reasoning about abstractions. It helps us understand why certain patterns work, how different abstractions relate to each other, and how to design new ones. While you can write Haskell code without knowing category theory, learning it can deepen your understanding and open up new ways of thinking about programming.

As software engineers, we often deal with complex systems and abstractions. Category theory provides a unifying framework that can help us manage this complexity and design better software. It’s like having a map of the landscape of programming concepts, which can guide us in our journey as developers.

Real takeaway: You don’t need to be a category theory expert to be a good Haskell programmer, but learning the basics can give you a deeper understanding of the language and its abstractions (abstractions are essential for writing good code). It's a powerful tool in your mental toolbox.

## Task assignment

For the assignment, navigate to the `hw05` project and follow the instructions in the `README.md` file there. It will test your skills in defining typeclasses, creating instances, and using them in practice.

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
