# IO, Exceptions, and More Typeclasses

In this tutorial, we will take a brief look at IO including exceptions and then at few more advanced typeclasses that you might want to use in some projects. They are not described in high detail, but just in an introductory manner, so when you encouter some problem - you should know what you can use and learn specific details for your case.

## Working with IO

When you need to incorporate input and output (CLI, files, sockets, etc.), you bring impureness into your program. Obviously, IO brings side effects (it interacts with the environment and changes the global state). It can be a bit complicated and so we won't go deep into theory this time and instead, we will just show how to use it. Theoretical part will be covered in the future.

```haskell
Prelude> :info IO
newtype IO a
  = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
                  -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
        -- Defined in ‘GHC.Types’
instance Monad IO -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
```

It is instance of `Monad`, but also `Functor`, `Aplicative`, and `Monoid` (iff `a` is also `Monoid`):

```haskell
import System.Random
import Control.Applicative

main0 :: IO ()
main0 = mempty

main1 :: IO ()
main1 = putStrLn "a" `mappend` putStrLn "b"

main2 :: IO ()
main2 = mconcat (map print [1..5])

main3 :: IO ()
main3 = do
     rname <- reverse <$> getLine  -- fmap reverse getLine
     print rname

main4 :: IO ()
main4 = print 1 *> print 2 *> print 3

main5 :: IO ()
main5 = print 1 <* print 2 <* print 3

main6 :: IO ()
main6 = do
     res <- (+) <$> randomInt <*> randomInt
     print res
       where randomInt = randomRIO (1, 10) :: IO Integer

main7 :: IO ()
main7 = do
    res <- liftA2 (\x y -> x + read y) randomInt getLine
    print res
      where randomInt = randomRIO (1, 10) :: IO Integer
```

A lot of confusion comes from ideas such as "Monad is IO", "To do something impure I need a monad", "Monad brings imperative style to FP", or "Monad is something hard and weird". No, `Monad` is just a type class with defined operations and laws, just as `Monoid` (so pretty simple, right?!). IO actions manipulate and output, this is their essence. And BY THE WAY, they are (very conveniently) an instance of `Monad`, `Applicative`, and `Functor`. Those  allow you to do some pure composition and other tricks with `IO` type and actions. A great and detailed explanation can be found on [HaskellWiki - IO inside](https://wiki.haskell.org/IO_inside).

### The main and gets + puts

If you know C/C++, Python, or other programming languages, you should be familiar with "main". As in other languages, `main` is defined to be the entry point of a Haskell program. For Stack projects, it is located in a file inside `app` directory and can be defined in `package.yaml` in `executables` section (it is possible to have multiple entrypoints per program). The type of `main` is `IO ()` -- it can do something (some actions) with `IO` and nothing `()` is returned. You may wonder why it is not `IO Int` (with a return code). It is because giving a return code is also an IO action and you can do it from `main` with functions from `System.Exit`.

Now, let's take a look at basic IO examples:

```haskell
main1 :: IO ()
main1 = putStr "Hello, Haskeller!"     -- putStr :: String -> IO ()

main2 :: IO ()
main2 = putStrLn "Hello, Haskeller!"   -- putStrLn :: String -> IO ()

main3 :: IO ()
main3 = do
          putStr "Haskell "
          putChar 'F'                   -- putChar :: Char -> IO ()
          putChar 'T'
          putChar 'W'
          putStrLn "! Don't you think?!"

-- pure function
sayHello :: String -> String
sayHello name = "Hello, " ++ name ++ "!"

main4 :: IO ()
main4 = do
          putStrLn "Enter your name:"
          name <- getLine                -- getLine :: IO String, see getChar & getContents
          putStrLn . sayHello $ name

-- custom IO action
promptInt :: IO Int
promptInt = do
              putStr "Enter single integer: "
              inpt <- getLine       -- unwraps from IO (inpt :: String)
              return (read inpt)    -- return wraps with IO, read :: String -> Int

compute x y = 50 * x + y

main5 :: IO ()
main5 = do
          intA <- promptInt
          intB <- promptInt
          putStrLn ("Result: ++ show . compute $ intA intB)

main6 :: IO ()
main6 = print 1254                  -- print = putStrLn . show
```

### What does `do` do?

It doesn't look so weird if you recall how imperative programming works... But we are in the functional world now, so what is going on? Haskell provides [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation), which is just a syntactic sugar for chaining actions and bindings (not just IO, in general!) in a simple manner instead of using `>>` (*then*) and `>>=` (*bind*) operators of the typeclass `Monad`. We cover this topic in detail in the next lecture, right now you can remember that although `do` looks imperative, it is actually still pure thanks to a smart "trick".

When you use the binding operator `<-`, it means that the result of a bound action can be used in following actions. In the example with `main4`, IO action `getLine` is of type `IO String` and you want to use the wrapped `String` - you *bind* the result to name `name` and then use it in combination with pure function `sayHello` for the following action that will do the output. The `do` block consists of actions and bindings and binding cannot be the last one!

You might have noticed the `return` in custom `promptInt` action. This is a confusing thing for beginners, as `return` here has **nothing to do** with imperative languages return. The confusing thing is that it *looks* very much like it. However, conceptually it is not a control-flow expression, but just a function of the typeclass `Monad` which is used for wrapping back something, in this case `return :: String -> IO String`. This is one of the reasons why PureScript got rid of `return` and uses `pure` instead. Again, we will look at this in detail in the next lecture.

### Be `interact`ive

A very interesting construct for building a simple CLI is `interact :: (String -> String) -> IO ()`. The interact function takes a function of type `String -> String` as its argument. The **entire** input from the standard input device is passed to this function as its argument, and the resulting string is output on the standard output device. Btw. this is a nice example of a higher-order function at work, right?

```haskell
import Data.Char

main1 :: IO ()
main1 = interact (map toUpper)

main2 :: IO ()
main2 = interact (show . length)

main3 :: IO ()
main3 = interact reverse
```

As is emphasized, it works with an entire input. If you've tried the examples above, you could observe a difference made by lazy evaluation in the first case. If you need to interact by lines or by words, you can create helper functions for that easily.

```haskell
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . f . lines

eachWord :: (String -> String) -> (String -> String)
eachWord f = unwords . f . words

main5 :: IO ()
main5 = interact (eachLine reverse)

main6 :: IO ()
main6 = interact (eachWord reverse)

chatBot "Hello" = "Hi, how are you?"
chatBot "Fine" = "Lucky you... bye!"
chatBot "Bad" = "Me too!"
chatBot _ = "Sorry, I'm too dumb to understand this..."

main7 :: IO ()
main7 = interact (eachLine chatBot)
```

### IO with files

Working with files is very similar to working with console IO. As you may already know, most of IO for consoles is built by using IO for files with system "file" stdin and stdout. Such thing is called a `Handle` in Haskell and it is well described in [System.IO](http://hackage.haskell.org/package/base/docs/System-IO.html#t:Handle).

```haskell
main1 :: IO ()
main1 = withFile "test.txt" ReadMode $ \handle -> do
           fileSize <- hFileSize handle
           print fileSize
           xs <- getlines handle
           sequence_ $ map (putStrLn . reverse) xs

main2 :: IO ()
main2 = do
          handle <- openFile  "test.txt" ReadMode    -- :: IO Handle
          fileSize <- hFileSize handle
          print fileSize
          hClose handle
```

In a similar manner, you can work with binary files (you would use `ByteString`s) and temporary files. To work with sockets (network communication), you can use a library like [network](hackage.haskell.org/package/network/) or specifically for HTTP [wreq](https://hackage.haskell.org/package/wreq) and [req](https://hackage.haskell.org/package/req).

For some well-known file formats there are libraries ready, so you don't have to work with them over and over again just with functions from `Prelude`:

* JSON: [aeson](https://hackage.haskell.org/package/aeson)
* YAML: [yaml](https://hackage.haskell.org/package/yaml)
* XML: [xml](https://hackage.haskell.org/package/xml), [hxt](https://hackage.haskell.org/package/hxt), or [xeno](https://hackage.haskell.org/package/xeno)
* CSV: [cassava](https://hackage.haskell.org/package/cassava) or [csv](https://hackage.haskell.org/package/csv/docs/Text-CSV.html)
* INI: [ini](https://hackage.haskell.org/package/ini)

... and so on. Also, you probably know the fabulous [pandoc](https://pandoc.org), which is written in Haskell -- and you can use it as a [library](https://hackage.haskell.org/package/pandoc)!

Hmmm, who said that Haskell is just for math and mad academics? ;-)

### Arguments and env variables

Another way of interacting with a program is via its command-line arguments and environment variables. Again, there is a little bit clumsy but simple way in [System.Environment](https://hackage.haskell.org/package/base/docs/System-Environment.html) and then some fancy libraries that can help you with more complex cases...

```haskell
main :: IO ()
main = do
         progName <- getProgName    -- IO String
         print progName
         path <- getExecutablePath  -- IO String
         print path
         args <- getArgs            -- :: IO [String]
         print args
         user <- lookupEnv "USER"   -- :: IO (Maybe String), vs. getEnv :: IO String
         print user
         env <- getEnvironment      -- :: IO [(String, String)]
         print env
```

The most used library for [command line option parser](https://wiki.haskell.org/Command_line_option_parsers) is [cmdargs](http://hackage.haskell.org/package/cmdargs):

```haskell
{-# LANGUAGE DeriveDataTypeable #-}
module Sample where
import System.Console.CmdArgs

data Sample = Hello {whom :: String}
            | Goodbye
              deriving (Show, Data, Typeable)

hello = Hello{whom = def}
goodbye = Goodbye

main = do
         args <- cmdArgs (modes [hello, goodbye])
         print args
```

For a more complex example, visit their documentation -- for example, `hlint` or `diffy` use this one.

## Handling errors

As we saw, a very elegant way way how to handle errors is using `Maybe` or `Either` types. This is a preferred way with obvious advantages, however, in practice, it may still come to a more explosive situation.

### error

`error` is a special function which stops execution with given message:

```
Prelude> error "Stop now"
*** Exception: Stop now
CallStack (from HasCallStack):
  error, called at <interactive>:1:1 in interactive:Ghci1
```

There is another quite similar one - `errorWithoutStackTrace`:

```
Prelude> errorWithoutStackTrace "Stop now without stack trace"
*** Exception: Stop now without stack trace
```

It is obviously even worse than just `error` because you somewhere deep in your code say something about rendering the error...

### undefined

Special case of error is that something is `undefined` and it does not accept any message:

```
Prelude> undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:5:1 in interactive:Ghci1
```

Semantically, it can be used where the value is not defined (for example when you want to divide by zero). Sometimes you can see it used as a basic placeholder with meaning "Not implemented yet". For such things, you can use custom `error` or some specialized package like [Development.Placeholders](hackage.haskell.org/package/placeholders/docs/Development-Placeholders.html), which are more suitable.

### throw, try and catch

We have `throw`, `try` and `catch`, but those are functions - not keywords!

```
Prelude> import Control.Exception
Prelude Control.Exception> :type try
try :: Exception e => IO a -> IO (Either e a)
Prelude Control.Exception> :type throw
throw :: Exception e => e -> a
Prelude Control.Exception> :type catch
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

If you are interested you can read the documentation of [Control.Exception](https://hackage.haskell.org/package/base/docs/Control-Exception.html), however, exceptions are considered an anti-pattern in Haskell and you should always try to deal with potential errors in a more systematic way using types. We will slightly get back to these after getting the notion of Monads.

```haskell
import System.IO
import Control.Exception

myHandler exc = do
  putStrLn "Oops, error occured while trying to read the file"
  putStrLn $ "It failed with: " ++ show (exc :: SomeException)

main = handle myHandler $ do
        fp <- openFile "test.txt" ReadMode
        fileSize <- hFileSize fp
        print fileSize
        hClose fp
```

## Advanced Typeclasses

### Foldable

Recall the time when we were talking about folds... The `Foldable` type class provides a generalization of list folding (`foldr` and friends) and operations derived from it to arbitrary data structures. The class does not require the Functor superclass in order to allow containers like Set or StorableVector that have additional constraints on the element type. But many interesting Foldables are also Functors. A foldable container is a container with the added property that its items can be 'folded' to a summary value. Recall what `foldr` and `foldl` do...

In other words, it is a type which supports "foldr". Once you support foldr, of course, it can be turned into a list, by using `toList = foldr (:) []`. This means that all foldables have a representation as a list, but the order of the items may or may not have any particular significance. However, if a Foldable is also a Functor, parametricity, and the Functor law guarantee that `toList` and `fmap` commute. Further, in the case of [Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html), there is a well-defined order and it is exposed as expected by `toList`. A particular kind of fold well-used by Haskell programmers is `mapM_`, which is a kind of fold over `(>>)`, and Foldable provides this along with the related `sequence_`.

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

This class is very straight-forward, it has no specific laws, but it is very powerful as we've already known... It allows you to create new or use various containers with same generic functions like `null`, `length`, `elem`, `minimum`, `maximum`, and others seamlessly and without any problems. For more, see [Data.Foldable](https://hackage.haskell.org/package/base/docs/Data-Foldable.html).

#### Specialized folds

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

#### Foldable and Applicative

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

#### Foldable and Monad

Similarly, there are also same folds for `Monad`s, just naming is a bit different:

```haskell
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()  -- flip . mapM_

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a          -- An alternative is described in this tutorial
```

### Traversable

A `Traversable` type is a kind of upgraded `Foldable` with use of `Functor`. Where Foldable gives you the ability to go through the structure processing the elements (*catamorphism*) but throwing away the "shape", `Traversable` allows you to do that whilst preserving the "shape" and, e.g., putting new values in. Traversable is what we need for `mapM` and `sequence`: note the apparently surprising fact that the versions ending with an underscore (e.g., `mapM_`) are in a different typeclass - in `Foldable`.

```haskell
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
```

`Traversable` has, unlike `Foldable`, a few laws (naturality, identity, composition, ...). For more, see [Data.Traversable](https://hackage.haskell.org/package/base/docs/Data-Traversable.html).

#### No more underscore

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

### State

As you know, Haskell is great, there is no mutability, which results in reference transparency, everything has a mathematical foundations, and life is perfect. Or not? Using a mutable state is clearly over-used in imperative and object-oriented programming, at the same time, the concept of "state" may be inherently present in the modelled domain and so we need to deal with it.

In the simplest case, the solution is to pass the state (or context) to the function, do something, and get the result with **new** (*new - next one, no mutability*) state that you can pass further. This creates a pattern, which is embodied in the [State Monad](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State). You can write your own or you can use [Control.Monad.State](https://hackage.haskell.org/package/mtl/docs/Control-Monad-State.html) and (little bit different) [Control.Monad.Trans.State](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-State.html)

```haskell
import Control.Monad

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap = Control.Monad.liftM

instance Applicative (State s) where
  pure = return
  (<*>) = Control.Monad.ap

instance Monad (State s) where
  return x  = State (\s -> (x, s))
  p >>= k   = State $ \s0 ->                       -- Sequencing:
                      let (x, s1) = runState p s0  -- Running p on s0.
                      in runState (k x) s1         -- Running k on s1.
```

There are two interesting things. First, `State` is a record type with one field of type `s -> (a, s)`. Then `(>>=)` operator returns a `State` with a function that first runs `p` on given state `s0`, get intermediary result `(x, s)` and returns the result of running `k x` on `s1`

#### Example: simple counter

Let's look at a simple example:

```haskell
import Control.Monad.State

type Counter = State Int Int

tick :: Counter
tick = state (\x -> (x + 1, x + 1))

tick3 :: Counter
tick3 = do
          tick
          tick
          tick

main = do
          print (evalState tick3 0)
          print (evalState tick3 5)
```

If still not clear, try to read about `Reader` and `Writer` monads and look [here](http://adit.io/posts/2013-06-10-three-useful-monads.html) or into the classic [LYAH](http://learnyouahaskell.com/for-a-few-monads-more#state).

#### Random in Haskell

When you are using `System.Random`, you work with `State`: `State` is the generator for pseudorandom numbers (some equation with "memory").

```haskell
import System.Random

main = do
   gen <- newStdGen   -- state
   let ns = randoms gen :: [Int]
   print $ take 10 ns
```

#### Parser

Another typical example where you use `State` is when you want to parse something. So for this purpose, we have Parser monadic combinator as follows:

```haskell
newtype Parser a = Parser (parse :: String -> [(a,String)])
```

A very nice example is [here](http://dev.stephendiehl.com/fun/002_parsers.html).

For custom `Read` instance, you can do something simple, but it still works as a parser and uses `ReadS` (S ~ state):

```haskell
import Data.Char

data Time = Time Int Int Int

timePart x
  | x < 10    = '0' : show x
  | otherwise = show x

instance Show Time where
  show (Time hours minutes seconds) = timePart hours ++ ":" ++ timePart minutes ++ ":" ++ timePart seconds

instance Read Time where
  readsPrec _ (h1:h2:':':m1:m2:':':s1:s2:remaining)
    | all isDigit [h1,h2,m1,m2,s1,s2] = [(Time h m s, remaining)]
    | otherwise = []
    where
      h = mkTimePart h1 h2
      m = mkTimePart m1 m2
      s = mkTimePart s1 s2
      mkTimePart x1 x2 = 10 * digitToInt x1 + digitToInt x2
  readsPrec _ _ = []
```

Notice that you have to return list of tuples of type `(a, String)` just like in `parse`. The `readsPrec` gets and `Int` and then `String` where the number serves for the operator precedence of the enclosing context (can be often omitted).

### Alternative and MonadPlus

We are used use type `Maybe` when the result can be something or fail/nothing, and lists when there are many results of the same type and arbitrary size. Typeclasses `Alternative` and `MonadPlus` are here to provide a generic way of aggregating results together. `Maybe` and `[]` are its instances - read more: [Control.Applicative#Alternative](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative) and [Control.Monad#MonadPlus](https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:MonadPlus). You might find this very useful for [parsing](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus#Example:_parallel_parsing).

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

#### `guard` (don't mix with guards!)

An interesting function related to `Alternative` is `guard :: Alternative f => Bool -> f ()`. What does it do? It works like a guard in a sequence of actions!

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

### Monad Transformers

We have seen how monads can help handling IO actions, Maybe, lists, and state. With monads providing a common way to use such useful general-purpose tools, a natural thing we might want to do is using the capabilities of several monads at once. For instance, a function could use both I/O and Maybe exception handling. While a type like `IO (Maybe a)` would work just fine, it would force us to do pattern matching within `IO` do-blocks to extract values, something that the `Maybe` monad was meant to spare us from. Sounds like a dead end, right?!

Luckily, we have monad transformers that can be used to combine monads in this way, save us time, and make the code easier to read.

#### MaybeT

Consider following simple program:

```haskell
getPassphrase :: IO (Maybe String)
getPassphrase = do s <- getLine
                   if isValid s then return $ Just s
                                else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

askPassphrase :: IO ()
askPassphrase = do putStrLn "Insert your new passphrase:"
                   maybe_value <- getPassphrase
                   case maybe_value of
                       Just value -> do putStrLn "Storing in database..."  -- do stuff
                       Nothing -> putStrLn "Passphrase invalid."
```

Not nice even for a simple example. Now, we will get rid of the complexity with [MaybeT](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Maybe.html).

```haskell
-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

getPassphrase :: MaybeT IO String
getPassphrase = do s <- lift getLine
                   guard (isValid s) -- Alternative provides guard.
                   return s

askPassphrase :: MaybeT IO ()
askPassphrase = do lift $ putStrLn "Insert your new passphrase:"
                   value <- getPassphrase
                   lift $ putStrLn "Storing in database..."
```

For more about monad transformers visit [this](https://en.wikibooks.org/wiki/Haskell/Monad_transformers) and the [transformers](https://hackage.haskell.org/package/transformers) package. There is also a very nice chapter about them in http://haskellbook.com.

### Category and Arrow

Recall what was told about Category Theory in the last tutorial. In Haskell, we have also typeclasses `Category` and `Arrow` (Do you remember? Alias for *morphisms*.). We mention it here just as an interesting part of Haskell and let you explore it if you are interested...

Arrows are a new abstract view of computation, defined by John Hughes. They serve much the same purpose as monads -- providing a common structure for libraries -- but are more general. In particular, they allow notions of computation that may be partially static (independent of the input) or may take multiple inputs. If your application works fine with monads, you might as well stick with them. But if you're using a structure that's very like a monad, but isn't one, maybe it's an arrow. (see [https://www.haskell.org/arrows])

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

A good explanation with nice visualization is in the chapter [Understanding Arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows) at wikibooks.

### Lens (and the taste of Template Haskell)

The last thing we are going to get into this time is *Lens*. It is something that can make you a way more productive while working with records and especially nested records - which is something really common in non-trivial programs.

At the same time, you should know that there is some controversy about Lens, as they bring quite heavy dependencies (including Template Haskell, see below). As with every dependency, it is neccesary to weigh the pros and cons, so do not use Lens just for everything, because they are so nice ;-).

The combinators in [Control.Lens](https://hackage.haskell.org/package/lens) provide a highly generic toolbox for composing families of getters, folds, isomorphisms, traversals, setters and lenses and their indexed variants. A lens is a first-class reference to a subpart of some data type. For instance, we have `_1` which is the lens that "focuses on" the first element of a pair. Given a lens there are essentially three things you might want to do:

1. View the subpart
2. Modify the whole by changing the subpart
3. Combine this lens with another lens to look even deeper

If you are interested in `Control.Lens`, follow links in *Further reading* sections...

#### Lens example

First, let's try example without *lens*:

```haskell
data Point2D = Point2D { x, y :: Int } deriving Show
data Line = Line { pA, pB :: Point2D } deriving Show
```

```
Main*> :type x
x :: Point2D -> Int
Main*> :type pA
pA :: Line -> Point2D
Main*> line = Line { pA = Point2D { x = 0, y = 0 }, pB = Point2D { x = 5, y = 7 } }
Main*> line
Line {pA = Point2D {x = 0, y = 0}, pB = Point2D {x = 5, y = 7}}
Main*> pA line
Point2D {x = 0, y = 0}
Main*> x . pA $ line
0
Main*> x . pB $ line
5
Main*> line { pA = ((pA line) { y = 2 }) }  -- change y of first point
Line {pA = Point2D {x = 0, y = 2}, pB = Point2D {x = 5, y = 7}}
```

And now with *lens* - it will help us:

```haskell
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Control.Lens

data Point2D = Point2D { _x, _y :: Int } deriving Show
data Line = Line { _pA, _pB :: Point2D } deriving Show

makeLenses ''Point2D -- magic
makeLenses ''Line    -- magic
```

```
*Main> :type x
x :: Functor f => (Int -> f Int) -> Point2D -> f Point2D
*Main> :type pA
pA :: Functor f => (Point2D -> f Point2D) -> Line -> f Line
*Main> view pA line       -- view~get
Point2D {_x = 0, _y = 0}
*Main> view (pA.x) line   -- view~get
0
*Main> set (pA.y) 2 line
Line {_pA = Point2D {_x = 0, _y = 2}, _pB = Point2D {_x = 5, _y = 7}}
*Main> set (pA.y) 2 line
Line {_pA = Point2D {_x = 0, _y = 2}, _pB = Point2D {_x = 5, _y = 7}}
*Main> over (pB.x) (+5) line
Line {_pA = Point2D {_x = 0, _y = 0}, _pB = Point2D {_x = 10, _y = 7}}
```

#### What is `makeLenses`

The function `makeLenses` indeed does some magic! From its type signature `makeLenses :: Language.Haskell.TH.Syntax.Name -> Language.Haskell.TH.Lib.DecsQ`, you can see it has something to do with [Template Haskell](https://wiki.haskell.org/Template_Haskell). It is GHC extension that allows metaprogramming. In this case, the function `makeLenses` builds lenses (and traversals) with a sensible default configuration. You need to provide the data type name where the record starts with an underscore and it will basically generate lenses for you.

Template Haskell is very powerful and allows you to do interesting stuff, but it is pretty advanced and we will leave it up to you if you want to look at it... Also, Template Haskell is relevant just for GHC, other compilers do not support it. Last, but not least, Template-Haskell programmes compile (even) longer.

#### Use lenses or not?

Using lenses in Haskell for record types offers several advantages. Firstly, lenses provide a convenient and concise way to access and manipulate deeply nested fields within records. This helps in writing more readable and maintainable code, as it eliminates the need for manual pattern matching or accessor functions. Additionally, lenses support composition, enabling developers to chain multiple operations together seamlessly, which enhances code modularity and reusability. Moreover, lenses facilitate immutable updates by generating functions that produce new copies of records with modified fields, promoting a functional programming style and ensuring referential transparency. There are also additional libraries such as [optics](https://hackage.haskell.org/package/optics/docs/Optics.html) that go beyond lenses and have more advantages.

However, there are some drawbacks to using lenses in Haskell. One of the main criticisms is the perceived complexity introduced by lenses, especially for beginners. The syntax for defining and using lenses may appear unfamiliar and daunting to those new to the language or functional programming paradigm. Furthermore, lenses can sometimes incur performance overhead compared to manual record manipulation, although this may not be significant in many cases. Lastly, while lenses excel at accessing and modifying individual fields, they may not be the best choice for more complex transformations or operations involving multiple records, where alternative approaches like monadic or applicative style may be more appropriate. Overall, while lenses offer powerful abstractions for working with records in Haskell, developers should carefully consider their use case and weigh the trade-offs involved.

#### OverloadedDotRecord (since GHC 9.2)

If you need to simply access fields of nested records, GHC 9.2.0 and newer allows you to use `OverloadedDotRecord` extension. With that, you simply use `.` as accessor to fields of records. It is also good to combine it with `DuplicateRecordFields` as shown in the following example.

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

data Person = Person
            { firstName :: String
            , lastName :: String
            , idNumber :: String
            } deriving (Show, Read)


data Organization = Organization
                  { name :: String
                  , owner :: Person
                  , idNumber :: String
                  } deriving (Show, Read)


marek = Person "Marek" "Suchánek" "123"
acme = Organization "ACME" marek "456"

acmeOwnerName = acme.owner.firstName ++ " " ++ acme.owner.lastName
```

## Task assignment

The homework to practice typeclasses from this tutorial is in repository [MI-AFP/hw06](https://github.com/MI-AFP/hw06).

## Further reading

* [Haskell - Introduction to IO](https://wiki.haskell.org/Introduction_to_IO)
* [Haskell - Handling errors in Haskell](https://wiki.haskell.org/Handling_errors_in_Haskell)
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
* [Lens](http://lens.github.io/tutorial.html)
* [Control.Lens.Tutorial](https://hackage.haskell.org/package/lens-tutorial/docs/Control-Lens-Tutorial.html)
* [SchoolOfHaskell - Lens tutorial](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
* [Lenses In Pictures](http://adit.io/posts/2013-07-22-lenses-in-pictures.html)
* [Next Level MTL - George Wilson - BFPG 2016-06 (Lens, Monad transformers)](https://www.youtube.com/watch?v=GZPup5Iuaqw)
