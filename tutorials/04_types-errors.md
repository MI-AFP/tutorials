# Types, containers, and errors

## Evaluation

First, we briefly get back to laziness in Haskell as mentioned in the previous lesson, because it is also related to lazy/strict types which will be discussed in this lesson.

### List comprehensions

Creation of lists using a constructor (or its syntactic sugar) is pretty straightforward, but there are two more interesting ways for that. First is used basically for basic ranges and it is called "dot dot notation". You have seen it already. It works with types that are instances of type class `Enum` (you can check within GHCi by `:info Enum`. You can specify start, step and end of the range (inclusive), but you need to be careful with floats and doubles because of their precision - the error cumulatively grows.

```
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> [0,5..20]
[0,5,10,15,20]
Prelude> ['a' .. 'z']
"abcdefghijklmnopqrstuvwxyz"
Prelude> [1.0,1.05 .. 1.2]
[1.0,1.05,1.1,1.1500000000000001,1.2000000000000002]
```

A more flexible way is offered by [list comprehensions](https://wiki.haskell.org/List_comprehension). This concept/construct is nowadays used in many other programming languages, as well, such as Python. In "list" you first specify an expression with variables and then after pipe `|`, there are specifications of bindings and restrictions. It is also possible to define local names with `let`.

```
Prelude> [n*2+1 | n <- [1..5]]
[3,5,7,9,11]
Prelude> [(i, j) | i <- [1..5], j <- [0,1]]
[(1,0),(1,1),(2,0),(2,1),(3,0),(3,1),(4,0),(4,1),(5,0),(5,1)]
Prelude> [x | x <- [0..10], x `mod` 3 == 1, x /= 7]
[1,4,10]
Prelude> take 10 [(i, j) | i <- [1..5], let k=i-5, j <- [k..6]]
[(1,-4),(1,-3),(1,-2),(1,-1),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5)]
```

### Lazy Haskell

As we've already seen, Haskell has lazy non-strict evaluation strategy. It means that no expression is evaluated, unless the value is needed. One of the possibilities is creating infinite lists. You may use `undefined` for testing when the expression is evaluated.

```
Prelude> let x = 1:x
Prelude> take 10 x
[1,1,1,1,1,1,1,1,1,1]
Prelude> take 20 x
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
Prelude> x
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,...^C Interrupted.
Prelude> [1,2..]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,...^C Interrupted.
Prelude> let x = undefined
Prelude> let y = 1 + x
Prelude> let z = y * 2 + 15
Prelude> :type y
y :: Num a => a
Prelude> :type x
x :: a
Prelude> z
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:37:5 in interactive:Ghci23
```

(For stopping output press CTRL+C in GHCi)

### Strictness with types

In the previous lesson, we touched the topic of enforcing strictness with `!` in patterns ([bang patterns](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html)) and in function application with `$!` operator. Similarly, we can use `!` with type fields like this:

```haskell
data MyType = MyConstr Int !Int

data MyRec = MyRecConstr { xA ::  Int
                         , xB :: !Int
                         }
```

For both cases it means that when data constructor is evaluated, it must fully evaluate ([weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)) the second parameter, but the first one will stay unevaluated in a lazy way. All depends on language implementation in the used compiler.

In order to achieve laziness, Haskell wraps all types with some additional information = *boxed types*. If you don't need laziness and other related properties, it is more efficient to use raw *unboxed types*. We will talk about that as a bonus or at the end of course in the *Performance* section. Now you just need to know roughly what is it about, because it is used in some textual types...

## Textual types

Textual types [(strings)](https://wiki.haskell.org/Strings) are kind of pain in Haskell. This is mostly because of its long legacy and also laziness/strictness trade-offs. However, as everything, they are not really insidious, just not convenient as they could be (and actually are in newer "Haskells", such as PureScript).

### String

[String](https://hackage.haskell.org/package/base/docs/Data-String.html) is the string type in the `base` package. It is just a type synonym for `[Char]`, so it comes with all properties of a [list](https://hackage.haskell.org/package/base/docs/Data-List.html), and as such, it is the most common one, especially for non-performance-sensitive applications. But when it comes to performance (and sometimes even Unicode behavior), then problems arise - `String` has big overhead in time and space.

### Text

[Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html) from [text](https://hackage.haskell.org/package/text) package is a time and space-efficient implementation of Unicode text. You can convert between `Text` and `String` with functions `pack` and `unpack`. The `Data.Text` package exports functions with same names as there are for `String` (`head`, `length`, `map`, `replace`, etc.), so the advised import style is `import qualified Data.Text as T`.

```
Prelude> import qualified Data.Text as T
Prelude T> txt = T.pack "my effective text"
Prelude T> :type txt
txt :: T.Text
Prelude T> T.index txt 1
'y'
Prelude T> T.replace "my" "your" txt

<interactive>:13:11: error:
    • Couldn't match expected type ‘T.Text’ with actual type ‘[Char]’
    • In the first argument of ‘T.replace’, namely ‘"my"’
      In the expression: T.replace "my" "your" txt
      In an equation for ‘it’: it = T.replace "my" "your" txt

<interactive>:13:16: error:
    • Couldn't match expected type ‘T.Text’ with actual type ‘[Char]’
    • In the second argument of ‘T.replace’, namely ‘"your"’
      In the expression: T.replace "my" "your" txt
      In an equation for ‘it’: it = T.replace "my" "your" txt
Prelude T> T.replace (T.pack "my") (T.pack "your") txt
"your effective text"
Prelude T> length txt

<interactive>:11:8: error:
    • Couldn't match expected type ‘[a0]’ with actual type ‘T.Text’
    • In the first argument of ‘length’, namely ‘txt’
      In the expression: length txt
      In an equation for ‘it’: it = length txt
Prelude T> T.length txt
17
```

There is another variant of the Text package, which is [Data.Text.Lazy](https://hackage.haskell.org/package/text/docs/Data-Text-Lazy.html), which exports same operations and thanks to laziness, it can work with huge texts and it may provide better performance under the right circumstances. [Data.Text.Encoding](https://hackage.haskell.org/package/text/docs/Data-Text-Encoding.html) (and its lazy alternative) may be also useful.

### ByteString

Last of the types mentioned here is [Data.ByteString](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html) from [bytestring](https://hackage.haskell.org/package/bytestring) package. Byte vectors are encoded as strict Word8 arrays of bytes and they are used for interoperability between Haskell and C or other lower-level situations. In many ways, the usage is similar to [text](https://hackage.haskell.org/package/text) package (again `pack` and `unpack`, same basic functions, `Lazy` alternative, and so on). Next, there is an option to use vectors with `Char8` instead of `Word8`, which works as Unicode subset (0-255) strings and it is used when working with pure ASCII string representations.

```
Prelude T> import Data.ByteString as B
Prelude T B> bstr = B.pack [97, 98, 99]
Prelude T B> bstr
"abc"
Prelude T B> index bstr 2
99
Prelude T B> B.map (+1) bstr
"bcd"

Prelude T B> import qualified Data.ByteString.Char8 as C
Prelude T B C> C.pack "abc"
"abc"
Prelude T B C> B.pack "abc"

<interactive>:28:8: error:
    • Couldn't match type ‘Char’ with ‘GHC.Word.Word8’
      Expected type: [GHC.Word.Word8]
        Actual type: [Char]
    • In the first argument of ‘pack’, namely ‘"abc"’
      In the expression: pack "abc"
      In an equation for ‘it’: it = pack "abc"
Prelude T B C> cstr = C.pack "abc"
Prelude T B C> C.index cstr 2
'c'
```

In other cases you need to use encoding to encode/decode bytes to/from text:

```
E.encodeUtf8 (T.pack "život, жизнь, lífið, ਜੀਵਨ, ,حياة")
"\197\190ivot, \208\182\208\184\208\183\208\189\209\140, l\195\173fi\195\176, \224\168\156\224\169\128\224\168\181\224\168\168, ,\216\173\217\138\216\167\216\169"
Prelude T B C E> x = E.encodeUtf8 (T.pack "život, жизнь, lífið, ਜੀਵਨ, ,حياة")
Prelude T B C E> x
"\197\190ivot, \208\182\208\184\208\183\208\189\209\140, l\195\173fi\195\176, \224\168\156\224\169\128\224\168\181\224\168\168, ,\216\173\217\138\216\167\216\169"
Prelude T B C E> index x 0
197
Prelude T B C E> index x 2
105
```

### OverloadedStrings

As needing to pack all string literals when using non-base string representations is cumbersome, there is a handy [GHC] language extension [OverloadedStrings](https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html).

Generally, [GHC] language extensions can be enabled in the source file using pragma `LANGUAGE` as the first line in the file::

```haskell
{-# LANGUAGE OverloadedStrings #-}

module XY ...
```

In GHCi, the extension can be enabled using the `:set` directive:

```haskell
Prelude> :set -XOverloadedStrings
```

After that, a string literal type can be inferred by its usage in the source code:

```haskell
Prelude> import qualified Data.Text as T
Prelude T> :type "abc"
"abc" :: [Char]
Prelude T> :set -XOverloadedStrings
Prelude> :type "abc"
"abc" :: Data.String.IsString p => p
Prelude T> T.length "abc" -- no need to pack the literal any more!
3
```

## Important "base" types

We already know basic data types (from `base` package) such as [Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html), [Bool](https://hackage.haskell.org/package/base/docs/Data-Bool.html), or [Data.Int](https://hackage.haskell.org/package/base/docs/Data-Int.html) and structures like [Data.List](https://hackage.haskell.org/package/base/docs/Data-List.html) and [Data.Tuple](https://hackage.haskell.org/package/base/docs/Data-Tuple.html) pretty well. But of course, there are more widely used types and we are going to know some more now.

### Maybe

In most programming languages, there is a notion of `null` or `nil` or even `None` value. Such a value is usable, but it leads often to undesired crashes of "Null pointer exception". As Haskell is type-safe, it does not allow such rogue surprises to happen, but instead deals with a possible "null" situation in a managed way.

If we were to design such a solution, we may use ADTs like that:

```haskell
data IntOrNull = I Int | Null
data StringOrNull = S String | Null
data ValueOrNull a = Value a | Null

myDiv     :: Int -> Int -> ValueOrNull Int
myDiv x 0 = Null
myDiv x y = Value (x `div` y)

divString     :: Int -> Int -> String
divString x y = case (myDiv x y) of
                  Null -> "Division by zero is not allowed!"
                  Value res -> "Result: " ++ (show res)
```

In Haskell, we have a pretty structure called `Maybe` which does exactly that for us and there are some functions helping with common usage. It is a very important structure and you will be dealing with it very often. You can find more about in the documentation of [Data.Maybe](https://hackage.haskell.org/package/base/docs/Data-Maybe.html).

It is defined as:

```haskell
data Maybe a = Nothing | Just a
```

```
Prelude Data.Maybe> :type Just 10
Just 10 :: Num a => Maybe a
Prelude Data.Maybe> :type Nothing
Nothing :: Maybe a
Prelude Data.Maybe> fromJust (Just 10)
10
Prelude Data.Maybe> fromJust Nothing
*** Exception: Maybe.fromJust: Nothing
Prelude Data.Maybe> fromMaybe "default" Nothing
"default"
Prelude Data.Maybe> fromMaybe "default" (Just "something")
"something"
Prelude Data.Maybe> catMaybes [Just 6, Just 7, Nothing, Just 8, Nothing, Just 9]
[6,7,8,9]
```

```haskell
-- Communicator interface
data Message = Message { msgSender    :: String
                       , msgRecipient :: String
                       , msgMetadata  :: [(String, String)]
                       , msgBody      :: String
                       }

sendAndReceive :: Communicator -> Message -> Maybe Message
sendAndReceive comm msg = sendSync comm msg  -- some library "magic"

printReceivedMessage :: Maybe Message -> String
printReceivedMessage Nothing    = "Unknown error occured during communication."
printReceivedMessage (Just msg) = msgSender msg ++ ": " ++ msgBody msg

myCommunicator = printReceivedMessage . sendAndReceive comm
```

### Either

`Maybe` is also used to signal two types of results: an error (`Nothing`) and a success (`Just value`). However, it does not tell what is the error in case of `Nothing`. There is a standard type for such use cases, and it is called `Either`:

```haskell
data Either a b = Left a | Right b
```

The `Left` variant holds an error value (such as a message) and the `Right` variant holds the success result value. There are again several utility functions available (see [Data.Either](https://hackage.haskell.org/package/base/docs/Data-Either.html))


```
Prelude Data.Either> :type Left 7
Left 7 :: Num a => Either a b
Prelude Data.Either> :type Right "Message"
Right "Message" :: Either a [Char]
Prelude Data.Either> lefts [Left 7, Right "Msg1", Left 8, Right "Msg2"]
[7,8]
Prelude Data.Either> rights [Left 7, Right "Msg1", Left 8, Right "Msg2"]
["Msg1","Msg2"]
Prelude Data.Either> partitionEithers [Left 7, Right "Msg1", Left 8, Right "Msg2"]
([7,8],["Msg1","Msg2"])
```

```haskell
-- Communicator interface
data Message = Message { msgSender    :: String
                       , msgRecipient :: String
                       , msgMetadata  :: [(String, String)]
                       , msgBody      :: String
                       }

data CommError = Timeout | Disconnected | UnkownRecipient | IncorrectMetadata | UnknownError
                deriving Show

sendAndReceive :: Communicator -> Message -> Either CommError Message
sendAndReceive comm msg = sendSync comm msg  -- some library "magic"

printReceivedMessage :: Either CommError Message -> String
printReceivedMessage (Left  err) = "Error occured during communication: " ++ show err
printReceivedMessage (Right msg) = msgSender msg ++ ": " ++ msgBody msg

myCommunicator = printReceivedMessage . sendAndReceive comm
```

### Unit

Although we said there is no `null`, `nil` or `None`, we still have one dummy value/type called "Unit" and it is designated as an empty tuple `()`.

```
Prelude> :info ()
data () = ()    -- Defined in ‘GHC.Tuple’
Prelude> :type ()
() :: ()
```

It is semantically more similar to `void` from other languages and you can use it wherever you don't want to use an actual type. For example, using `Either` to simulate  `Maybe`, you could do `Either a ()`. For more about [unit type, read wikipedia](https://en.wikipedia.org/wiki/Unit_type).

## Other containers

As in other programming languages or programming theory, there are various types of containers - data types/structures, whose instances are collections of other objects. As for collections with an arbitrary number of elements, we talked about lists, which are simple to use and have a nice syntactic-sugar notation in Haskell. However, there are also other versatile types of containers available in the package [containers] and others, such as [array](https://hackage.haskell.org/package/array), [vector](https://hackage.haskell.org/package/vector), and more (use [Hoogle], [Hayoo], [Hackage]).

### Sequence

The `Seq a` is a type from [Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html) that represents a **finite** sequence of values of type `a`. Sequences are very similar to lists, working with sequences is not so different, but some operations are more efficient - constant-time access to both the front and the rear and Logarithmic-time concatenation, splitting, and access to any element. But in other cases, it can be slower than lists because of overhead created for making operations efficient. The size of a `Seq` must not exceed `maxBound::Int`!

```
Prelude> import Data.Sequence
Prelude Data.Sequence> seq1 = 1 <| 2 <| 15 <| 7 <| empty
Prelude Data.Sequence> seq1
fromList [1,2,15,7]
Prelude Data.Sequence> :type seq1
seq1 :: Num a => Seq a
Prelude Data.Sequence> 3 <| seq1
fromList [3,1,2,15,7]
Prelude Data.Sequence> seq1 |> 3
fromList [1,2,15,7,3]
Prelude Data.Sequence> seq1 >< (fromList [2, 3, 4])
fromList [1,2,15,7,2,3,4]
Prelude Data.Sequence> sort seq1
fromList [1,2,7,15]
```

### Set

The `Set e` type represents a set of elements of type `e`. Most operations require that `e` be an instance of the `Ord` class. A `Set` is strict in its elements. If you know what is *set* in math and/or programming, you can be very powerful with them.

```
Prelude> import Data.Set
Prelude Data.Set> set1 = insert 4 $ insert 2 $ insert 0 $ singleton 2
Prelude Data.Set> set1
fromList [0,2,4]
Prelude Data.Set> delete 2 set1
fromList [0,4]
Prelude Data.Set> delete 3 set1
fromList [0,2,4]
Prelude Data.Set> mem
member  mempty
Prelude Data.Set> member 4 set1
True
Prelude Data.Set> member (-6) set1
False
Prelude Data.Set> Data.Set.filter (>3) set1
fromList [4]
Prelude Data.Set> set2 = insert 5 (insert 3 (singleton 2))
Prelude Data.Set> set2
fromList [2,3,5]
Prelude Data.Set> set1
fromList [0,2,4]
Prelude Data.Set> intersection set1 set2
fromList [2]
Prelude Data.Set> union set1 set2
fromList [0,2,3,4,5]
```

There is an efficient implementation of integer sets, which uses big-endian Patricia trees (works better mainly with union and intersection). Use qualified import like `import qualified Data.IntSet as IntSet` to work with it.

### Map

The `Map k v` type represents a finite map (sometimes called a dictionary) from keys of type `k` to values of type `v`. A Map is strict in its keys but lazy in its values (by default we use [Data.Map.Lazy](https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html). You may use [Data.Map.Strict](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html) instead if you will eventually need all the values stored and/or the stored values are not so complicated to compute (no big advantage of laziness).

```
Prelude> import Data.Map
Prelude Data.Map> map1 = insert "suchama4" "Marek Suchanek" (singleton "perglr" "Robert Pergl")
Prelude Data.Map> map1 ! "suchama4"
"Marek Suchanek"
Prelude Data.Map> map1 ! "suchamar"
"*** Exception: Map.!: given key is not an element in the map
CallStack (from HasCallStack):
  error, called at ./Data/Map/Internal.hs:610:17 in containers-0.5.11.0-K2TDqgYtGUcKxAY1UqVZ3R:Data.Map.Internal
Prelude Data.Map> map1 !? "suchamar"
Nothing
Prelude Data.Map> map1 !? "suchama4"
Just "Marek Suchanek"
Prelude Data.Map> size map1
2
Prelude Data.Map> delete "suchama4" map1
fromList [("perglr","Robert Pergl")]
Prelude Data.Map> delete "suchamar" map1
fromList [("perglr","Robert Pergl"),("suchama4","Marek Suchanek")]
Prelude Data.Map> map2 = insert "suchama4" "Marek Suchanek" (singleton "stengvac" "Vaclav Stengl")
Prelude Data.Map> map2 = insert "suchama4" "Marek Sushi Suchanek" (singleton "stengvac" "Vaclav Stengl")
Prelude Data.Map> union map1 map2
fromList [("perglr","Robert Pergl"),("stengvac","Vaclav Stengl"),("suchama4","Marek Suchanek")]
Prelude Data.Map> union map2 map1
fromList [("perglr","Robert Pergl"),("stengvac","Vaclav Stengl"),("suchama4","Marek Sushi Suchanek")]
Prelude Data.Map> intersection map1 map2
fromList [("suchama4","Marek Suchanek")]
```

Again, there is an efficient implementation of maps, where the keys are of `Int`. It uses same mechanisms as `Data.IntSet` - use `import qualified Data.IntMap as IntMap`.

### Graph and Tree

Finally, [containers] specify also [Data.Tree](https://hackage.haskell.org/package/containers/docs/Data-Tree.html) and [Data.Graph](https://hackage.haskell.org/package/containers/docs/Data-Graph.html), both in a very generic manner. If you ever need to work with trees or graphs, it is convenient to use those instead of reinventing the wheel yourself.

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

The homework to practice working with new types, list comprehensions, containers, and errors is in repository [MI-AFP/hw04](https://github.com/MI-AFP/hw04).

## Further reading

* [Oh my laziness](http://alpmestan.com/posts/2013-10-02-oh-my-laziness.html)
* [Haskell - list comprehension](https://wiki.haskell.org/List_comprehension)
* [Haskell - Lazy evaluation](https://wiki.haskell.org/Lazy_evaluation)
* [Haskell String Types](http://www.alexeyshmalko.com/2015/haskell-string-types/)
* [Untangling Haskells strings](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings)
* [Haskell containers](https://haskell-containers.readthedocs.io/en/latest/)
* [Haskell - Handling errors in Haskell](https://wiki.haskell.org/Handling_errors_in_Haskell)
* [Haskell - error](https://wiki.haskell.org/Error)
* [8 ways to report errors in Haskell](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)

[containers]: https://hackage.haskell.org/package/containers
[GHC]: https://www.haskell.org/ghc/
[Hackage]: https://hackage.haskell.org
[Hayoo!]: https://hayoo.fh-wedel.de
[Hoogle]: https://www.haskell.org/hoogle/