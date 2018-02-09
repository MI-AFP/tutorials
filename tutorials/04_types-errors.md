# Types, containers, and errors

## Evaluation

First we briefly get back to lazyness in Haskell as mentioned in the previous lesson. Because it is also related to lazy/strict types which will be discussed in this lesson.

### List comprehensions

Creation of lists with using the constructor or better with syntactic sugar is pretty simple but there are two more interesting ways for that. First is used basically for basic ranges and is called "dot dot notation. It is not a surprising at all and you have seen it already. It works with types that are instances of type class `Enum` (you can check withing GHCi by `:info Enum`. You can specify start, step and end of range (inclusive), but you need to be careful with floats and doubles because of their precision - error cumulatively grows.

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

More flexible are [list comprehensions](https://wiki.haskell.org/List_comprehension). This concept/construct is nowadays used in many other programming languages as well. In "list" you first specify expresion with variables and then after pipe `|` are specifications of bindings and restrictions. It is also possible to define local names with `let`.

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

As we've already seen, Haskell has lazy non-strict evaluation strategy. It means that no expression is evaluated unless the value is needed. One of possibilities is creating infinite lists. For testing when the expression is evaluated is good to use `undefined`.

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

### Boxed vs. Unboxed type

## Textual types

### String

### Text

### ByteString

### OverloadedStrings

## Important "base" types

We already know basic data types (from `base` package) such as [Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html), [Bool](https://hackage.haskell.org/package/base/docs/Data-Bool.html), or [Data.Int](https://hackage.haskell.org/package/base/docs/Data-Int.html) and structures like [Data.List](https://hackage.haskell.org/package/base/docs/Data-List.html) and [Data.Tuple](https://hackage.haskell.org/package/base/docs/Data-Tuple.html) pretty well. But of course there are more widely used types and we are going to know some more now.

### Maybe

As you can see in some other languages if you don't want to return actual value (object) you can use `null` or `nil` or even `None`. What would it mean in Haskell? That every type should contain such value (there are no such weird things as references and dynamic typing in Haskell). Even worse thing to do is returning some dummy value and checking it then with `if`s.

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


In Haskell we have pretty stucture called `Maybe` which does exactly that for us and there are some functions helping with common usage. More about it you can find in the documentation of [Data.Maybe](https://hackage.haskell.org/package/base/docs/Data-Maybe.html).

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

### Either

But what to do if we need to pass some value if there is error, for example some error message? One possibility would be to always return a tuple with two elements. But again there is standard type for such use cases and it is called `Either` (again documentation in [Data.Either](https://hackage.haskell.org/package/base/docs/Data-Either.html))

```haskell
data Either a b = Left a | Right b
```

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

### Unit

Although we said there is no `null`, `nil` or `None` we still have one dummy value/type called "Unit" and it is designated as empty tuple `()`.

```
Prelude> :info ()
data () = () 	-- Defined in ‘GHC.Tuple’
Prelude> :type ()
() :: ()
```

It is semantically more similar to `void` from other languages and you can use it wherever you don't want to use actual type. For example if you don't know about `Maybe` and want to use `Either` instead in same way, you could do `Either a ()`. For more about [unit type read wikipedia](https://en.wikipedia.org/wiki/Unit_type).

## Other containers

As in other programming languages or programming theory there are various types of containers - data types/structures whose instances are collections of other objects. If we talk about collections with arbitrary number of element, then we talked so far just about lists which are pretty simple to use and have a nice syntactic sugar notation in Haskell. But you might notice that for some use cases is not list optimal (when you need to access by index, find element in it, etc.). 

Luckily there are more and more containers. Various of them which we will mention in this lesson are from package [containers](https://hackage.haskell.org/package/containers), but there are of course many more like [array](https://hackage.haskell.org/package/array), [vector](https://hackage.haskell.org/package/vector), and others (use [Hoogle], [Hayoo], [Hackage]).

### Sequence

### Set

#### IntSet

### Map

#### IntMap

### Graph and Tree

## Working with errors

As we saw very smooth way how to work with errors is with `Either` or `Maybe` types. But still you can work with more complex errors and instead of logically returning some other value you can create error/exception skipping the natural way of computation and causing headaches.

### error

`error` is a special function which stops execution with given message:

```
Prelude> error "Stop now"
*** Exception: Stop now
CallStack (from HasCallStack):
  error, called at <interactive>:1:1 in interactive:Ghci1
```

There is other quite similar one - `errorWithoutStackTrace`:

```
Prelude> errorWithoutStackTrace "Stop now without stack trace"
*** Exception: Stop now without stack trace
```

It is obviously even worse than just `error` because you somewhere deep in your code say something about rendering the error...

### undefined

Special case of error is that something is `undefined` and does not accept any message:

```
Prelude> undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:5:1 in interactive:Ghci1
```

Semantically it can be used where the value is not defined (for example when you want to divide by zero). Definitely you should not use `undefined` in meaning "Not implemented yet".

For such things you can use custom `error` or some specialized package like [Development.Placeholders](hackage.haskell.org/package/placeholders/docs/Development-Placeholders.html).

### throw, try and catch

Even in haskell there are defined `throw`, `try` and `catch`, but those are functions - not a keywords!

```
Prelude> import Control.Exception
Prelude Control.Exception> :type try
try :: Exception e => IO a -> IO (Either e a)
Prelude Control.Exception> :type throw
throw :: Exception e => e -> a
Prelude Control.Exception> :type catch
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

We won't use such exceptions and always will try to deal with errors some other and nicer way. If you are interested you can read documentation of [Control.Exception](https://hackage.haskell.org/package/base/docs/Control-Exception.html).

## Task assignment

The homework to practice working with new types, list comprehensions, containers, and errors is in repository [MI-AFP/hw04](https://github.com/MI-AFP/hw04). 

## Further reading

* [Haskell - list comprehension](https://wiki.haskell.org/List_comprehension)
* [Haskell - Lazy evaluation](https://wiki.haskell.org/Lazy_evaluation)
* [Haskell String Types](http://www.alexeyshmalko.com/2015/haskell-string-types/)
* [Untangling Haskells strings](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings)
* [Haskell containers](https://haskell-containers.readthedocs.io/en/latest/)
* [Haskell - Handling errors in Haskell](https://wiki.haskell.org/Handling_errors_in_Haskell)
* [Haskell - error](https://wiki.haskell.org/Error)
* [8 ways to report errors in Haskell](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)

[Hackage]: https://hackage.haskell.org
[Hayoo!]: https://hayoo.fh-wedel.de
[Hoogle]: https://www.haskell.org/hoogle/
