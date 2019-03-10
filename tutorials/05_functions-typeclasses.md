# Typeclasses, IO, and exceptions

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

## Basic IO

When you need to incorporate input and output (CLI, files, sockets, etc.), you bring impureness into your program. Obviously, IO brings side effects (it interacts with the environment and changes the global state). It can be a bit complicated and so we won't go deep into theory this time and instead, we will just show how to use it. Theoretical part will be covered in the future.

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

## Task assignment

The homework to practice working with advanced functional patterns, operators, and typeclasses is in repository [MI-AFP/hw05](https://github.com/MI-AFP/hw05).

## Further reading

* [Haskell - Currying](https://wiki.haskell.org/Currying)
* [Haskell - Pointfree](https://wiki.haskell.org/Pointfree)
* [Haskell - Higher order function](https://wiki.haskell.org/Higher_order_function)
* [Haskell - Fold](https://wiki.haskell.org/Fold)
* [Learn You a Haskell for Great Good](http://learnyouahaskell.com) (chapters 3, 6, 8)
* [Haskell - Polymorphism](https://wiki.haskell.org/Polymorphism)
* [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
* [Haskell - OOP vs type classes](https://wiki.haskell.org/OOP_vs_type_classes)
* [WikiBooks - Haskell: Classes and types](https://en.wikibooks.org/wiki/Haskell/Classes_and_types)
