# Structuration and branching

## If and case

As in other languages, in Haskell you can use `if-then-else` and `case-of` (similar to `switch-case`) expression for branching the computation. But here it is slightly different. Although you've already seen and used `if-then-else` expression during this course, we will look at it in higher detail now.

### Own ifThenElse

First, let's try to implement own function realises the `if-then-else` branching. What would be the type of such function? We need the condition which is obviously of type `Bool` and then there are two expressions, one is evaluated if condition is `True` and the other if it is `False`. We can allow any type of such expression by type variable `a` and it will be the return type as well.

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse condition onTrue onFalse = ...
```

But how to implement it without actually using `if-then-else` keywords? Just by definition and expected behavior - when `condition` is `True`, then result is `onTrue` and if it is `False` then `onFalse`.

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  onTrue onFalse = onTrue
ifThenElse False onTrue onFalse = onFalse
```

We can even simplify it a bit with ignoring the other argument via `_`:

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  onTrue  _ = onTrue
ifThenElse False _ onFalse = onFalse
```

We can test our implementation of `ifThenElse` in GHCi and we can see that it works pretty well:

```
*Main> x = 7
*Main> y = 15
*Main> ifThenElse (x < y) (y - x) (x - y)
8
*Main> x = 50
*Main> ifThenElse (x < y) (y - x) (x - y)
35
```

### If, then, else keywords

We can see that we are able to implement very simply our own `ifThenElse` but it is not very readable and using nested ifs would be even worse. So from now on we will use keywords instead:

```
*Main> if (x < y) then (y - x) else (x - y)
35
*Main> if (x < y) then (y - x) else if (x == y) then 0 else (x - y)
35
*Main> y = 50
*Main> if (x < y) then (y - x) else if (x == y) then 0 else (x - y)
0
```

It is good to realize that nested `if` is not a magic but just and expression used for `else` branch:

```
*Main> if (x < y) then (y - x) else (if (x == y) then 0 else (x - y))
0
```

Finally, `if-then-else` is expression as any other, has some type and thus you can not have type mismatch in the branches:

```
*Main> :t (if 8 > 5 then 5 else 0)
(if 8 > 5 then 5 else 0) :: Num t => t
*Main> :t (if 8 > 5 then (5 :: Int) else 0)
(if 8 > 5 then (5 :: Int) else 0) :: Int
*Main> if 8 > 5 then 5 else "No"

<interactive>:1:16: error:
    • No instance for (Num [Char]) arising from the literal ‘5’
    • In the expression: 5
      In the expression: (if 8 > 5 then 5 else "No")
```

### Case of

Instead of `switch-case` Haskell offers `case-of` expression:

```haskell
data Color = Black | White | RGB Int Int Int

badDescribeBW :: Color -> String
badDescribeBW c = case c of
       Black           -> "black"
       White           -> "white"

describeBW :: Color -> String
describeBW c = case c of
       Black           -> "black"
       White           -> "white"
       RGB 0 0 0       -> "black"
       RGB 255 255 255 -> "white"
       _               -> "unknown"  -- "default" match
```

You need to be careful that you cover all the cases with `case-of`. If you hit some case which is not covered an exception will come up in runtime:

```
*Main> badDescribeBW (RGB 0 0 0)
"*** Exception: files/03_caseColors.hs:(4,19)-(6,33): Non-exhaustive patterns in case
```

For that you can use underscore `_` wildcard which acts as default case matching everything:

```
*Main> describeBW (RGB 0 0 0)
"black"
*Main> describeBW (RGB 7 7 7)
"unknown"
```

As you can see, it uses pattern matching as it was introduced in the previous lesson. Without any surprise it is then possible to use pattern matching for lists as well. When you need to match against multiple patterns, you can use tuple. 

```haskell
describeList :: [a] -> String  
describeList xs = "Given list has " ++ case xs of []  -> "no item."  
                                                  [x] -> "just one item."   
                                                  xs  -> "more than one item."  
```

## Guards and patterns

Another widely used way how create branches is by guards in function declarations. That allows you much more than matching shown in `ifThenElse`. Then you can also use patterns to easily work with some structures in branching.

### Guards

Guards are done by `|` operator-like keyword after introducing a function name and arguments as in following example. Then you can write boolean expressions instead of nested ifs and what should be the result in such case. The `otherwise` is the same meaning as `True` and so as `_` in `case-of` expression, you can notice the similarity with mathematical definitions of some functions.

```haskell
myMax :: (Ord a) => a -> a -> a
myMax a b
    | a > b     = a
    | otherwise = b
```

The order of testing guards is top-bottom and first which is `True` will be applied. Try it with:

```haskell
guardsOrder x
    | x < 5 = "x < 5"
    | x < 0 = "x < 0"
    | x > 2 = "x > 1"
    | otherwise = "otherwise"
```

Also remember the difference between `_` and `otherwise` (it is something totally different!).

```
Prelude> :t otherwise
Prelude> otherwise == True
True
Prelude> not otherwise
False
otherwise :: Bool
Prelude> :t _

<interactive>:1:1: error:
    • Found hole: _ :: t
      Where: ‘t’ is a rigid type variable bound by
               the inferred type of it :: t at <interactive>:1:1
    • In the expression: _
```

### Patterns are syntactic sugar

It is good to notice that pattern matching in function declaration is the same as pattern matching with `case-of`. Actually it is just [syntactic sugar](https://en.wikibooks.org/wiki/Haskell/Syntactic_sugar#Function_Bindings) and following two functions are equivalent. Moreover, you can combine pattern matching with guards (no surprise).

```haskell
myHead1 :: [a] -> a
myHead1 []    = error "Empty list"
myHead1 (x:_) = x

myHead2 :: [a] -> a
myHead2 list = case list of
                  []    -> error "Empty list"
                  (x:_) -> x

myHead3 :: [a] -> a
myHead3 = \list -> case list of   -- lambda expression
                  []    -> error "Empty list"
                  (x:_) -> x
```

### Named patterns

In some cases, if you are matching a pattern with a value, it may be useful to bind a name to the whole value being matched (when you need to use decomposition and whole as well). As-patterns allow exactly this: they are of the form `name@pattern` and in addition to acting as regular pattern it binds the `name` to the whole value being matched by `pattern`. Again, it can be used not just with lists but also with any other types.

```haskell
duplicateFirstElement1 [] = []
duplicateFirstElement1 (x:xs) = x:x:xs

duplicateFirstElement2 [] = []
duplicateFirstElement2 list@(x:xs) = x:list

duplicateFirstElement3 [] = []
duplicateFirstElement3 list@(x:_) = x:list
```

## Let in, where

You can define an expression (function or constant) at module (file) level but then it can be used everywhere in that module/file. If you want to structure your code well and define some local expressions you have two basic ways how to do it - with `let-in` or `where` keywords.

### Let in

Firstly, `let ... in ...` is an expression that can be written in any place you can write expressions. After `let` you can define expressions (called bindings) you will then use in following one (the one after `in`). It is one of the ways how to locally introduce named expression and reuse it. 

```haskell
circleArea radius = let pi = 3.14159
                    in pi * radius^2

cylinderVolume radius height = let area = circleArea radius
                               in height * area

blockVolume width height depth = let area a b = a * b
                                 in height * area width height

blockSurface width height depth = let area a b = a * b
                                  in let areaA =  area width height
                                         areaB =  area width depth
                                         areaC =  area height depth
                                     in 2 * (areaA + areaB + areaC)
```

Don't forget that everything is immutable. Try following (and guess what will be the results):

```haskell
testLetIn1 = let a = 7
                 b = 5
                 c = 10
             in a

testLetIn2 = let a = 7
                 b = 5
             in let a = 2 * b
                in a

testLetIn3 = let a = 7
             in let a = a + 3
                in a
```

Sometimes you need to use `let-in` on one line, then you should use `;` or tuple-like binding. Again it is nothing else than expression.

```
*Main> let a = 10; b = 20; c = 30 in a*b*c
600000
*Main> let (a, b, c) = (10, 20, 30) in a*b*c
6000
*Main> :t let (a, b, c) = (10, 20, 30) in a*b*c
let (a, b, c) = (10, 20, 30) in a*b*c :: Num a => a
```

### Where

In contrast to `let-in`, `where` is bound to a surrounding syntactic construct, like the pattern matching line of a function definition and thus cannot be used everywhere as expression. You can understand it little as `let-in` but vice versa (first expression and then bindings of used expressions in it). If you are fan of math, it is similar to mathematical definitions - you first have some complex expression with unspecified symbols (variables, functions, constants, etc.) and then there is explanation part starting with *where*.

```haskell
circleArea radius = pi * radius^2
                  where pi = 3.14159
                  
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = low ++ [x] ++ high
                 where low  = quicksort (filter lqPivot xs)
                       high = quicksort (filter gtPivot xs)
                       lqPivot y = y <= x
                       gtPivot y = y >  x
```

You can also create nested `where` (use nice visual indentation):

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = low ++ [x] ++ high
                 where low  = quicksort (filter lqPivot xs)
                            where lqPivot y = y <= x
                       high = quicksort (filter gtPivot xs)
                            where gtPivot y = y >  x
          
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname   
```

### Where with guards

Typical and pretty use is with guards to achieve DRY (Don't repear yourself):

```haskell
birthYearToTitle :: Int -> String
birthYearToTitle year
    | age <= 12 = "Kid"
    | age <= 19 = "Teenager"
    | age <= 65 = "Adult"
    | otherwise = "Senior"
    where age = currentYear - year
          currentYear = 2018       -- or from Data.Time
```

## Evaluation, patterns and wildcards

We will now slightly switch from branching to evaluation strategy of Haskell to understand why is good to use such things as wildcards and also to be able to understand bang patterns.

### Bottom

To talk about laziness we often use term [bottom](https://wiki.haskell.org/Bottom) (&#8869; or `_|_`) which means *computation which never completes successfully*. Various things can happen - computation can fail due to some kind of error or a computation that just goes into an infinite loop. Term bottom is related to value as well as to type. Examine those with GHCi:

```haskell
data BottomType = BottomType   -- same as unit "()"

bottomValue = bottomValue -- endless 
errorBottom = error "Reached some error"
undefBottom = undefined
```

### Haskell is lazy

Haskell has lazy non-strict evaluation strategy. It means that no expression is evaluated unless the value is needed. One of possibilities is creating infinite lists. For testing when the expression is evaluated is good to use `undefined`.

```
Prelude> let x = 1:x   -- same as "repeat 1" or "[1,1..]"
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

### Haskell can be strict

For enforcing strictness there is the `!` symbol and you can read more about the usage [here](https://wiki.haskell.org/Performance/Strictness). Obviously bad things will happen if your code contains infinite list or recursion which never ends - you will need to terminate the program!

There is an operator `$` called function application and its function is not very magical. As left operand it takes a function and on right side is operand for the function. It can avoid using brackets with function composition (will be covered later). Then there is strict application `$!$`, see the difference:

```
Prelude> :type ($)
($) :: (a -> b) -> a -> b
Prelude> :type ($!)
($!) :: (a -> b) -> a -> b
Prelude> :type (take 0)
(take 0) :: [a] -> [a]
Prelude>
Prelude> take 0 $ undefined
[]
Prelude> take 0 $! undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:20:11 in interactive:Ghci9
Prelude> take 0 $ undefined
[]
```

In similar way you can enforce strictness when declaring data type.

```haskell
data Pet = Pet !String Int

data Person = Person { username :: !String
                     , realname :: !String
                     }
```

### Wildcard's advantage

Similar problem you can avoid with wildcard, where the parameter is evaluated (strictly) till it can be matched with the pattern. If in pattern is `x`, nothing is evaluated, but if there is something like `[]` or `MyType a b 4` it will be evaluated until match can be confirmed or denied. Consider two following functions which differ only in order of matching rules (recall that patterns are tested top-bottom):

```haskell
myTake1 :: Int -> [a] -> [a]
myTake1 _ [] = []
myTake1 0 _ = []
myTake1 n (x:xs) = x : myTake1 (n-1) xs

myTake2 :: Int -> [a] -> [a]
myTake1 0 _ = []
myTake2 _ [] = []
myTake2 n (x:xs) = x : myTake1 (n-1) xs
```

```
*Main> myTake1 0 undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:32:11 in interactive:Ghci1
*Main> myTake2 0 undefined
[]
```

### Bang patterns

Bang patterns are GHC extension (and part of Haskell Prime) that can be used for strict evaluation in pattern matching and is written like `!pat` (uses exclamation mark). To enable it, use `{-# LANGUAGE BangPatterns #-}` pragma or flag `-XBangPatterns`.

Opposite to classical matching the pattern `!pat` against a value `v` behaves as follows:

* if `v` is bottom, the match diverges
* otherwise, `pat` is matched against `v`

```haskell
{-# LANGUAGE BangPatterns #-}

myAppend1 :: [a] -> a -> [a]
myAppend1 [] y = [y]
myAppend1 (x:xs) y = x : (myAppend1 xs y)

myAppend2 :: [a] -> a -> [a]
myAppend2 [] y = [y]
myAppend2 (x:xs) !y = x : (myAppend2 xs y)  -- Bang
```

```
*Main> myAppend1 [1..] undefined
[1, 2, 3, 4, 5, 6, ... Ctrl^C
*Main> myAppend2 [1..] undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:55:17 in interactive:Ghci1
```

Further information can be found in the [documentation](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/bang-patterns.html).

## Modules and imports

A Haskell program consists of a collection of modules (similar to other programming language). In the top level you can declare and define data types, function, typeclasses and their instances, pattern bindings and so on.

### Module specification

Every file forms a module, if there is no specification of module name, `Main` is used by default as we saw. Module name must start with capital letter and then it is alphanumeric string. Although there is no formal connection between filesystem and modules, if you use GHC (and we do) you should name your canonical module name should reflect FS. For example module `FPCourse.Lesson3.TestModule` would be places in file `FPCourse/Lesson3/TestModule.hs` with content:

```haskell
module FPCourse.Lesson3.TestModule (
    myFunc1, myFunc3
) where

myFunc1 x = myFun2 7 x

myFunc2 x y = x - y

myFunc3 x y z = x * y + z
```

Notice that after module name there is an optional list of stuff which can be imported from this module. In this case you can import `myFunc1` and `myFunc3` but not `myFunc2`.

### Import something

How to import something from different module? As in other languages use `import` keyword:

```haskell
import FPCourse.Lesson3.TestModule

x = myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc1 25
```

Plain import will allow you to use all exposed from module with unqualified and qualified (with module name) names.

You can also specify what do you want to import explicitly:

```haskell
import FPCourse.Lesson3.TestModule ( myFunc1 )

x = myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc1 25
```

### Qualified import

If you want/need to import just with qualified names, you can use `qualified` keyword:

```haskell
import qualified FPCourse.Lesson3.TestModule ( myFunc1 )

x = FPCourse.Lesson3.TestModule.myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc1 25
```

If you need to import something with same name from different modules, you must use just the qualified names to distinguish it. As you can see writing qualified name can be bothersome but needed. Luckily you can introduce an alias for the module name with keyword `as`:

```haskell
import qualified FPCourse.Lesson3.TestModule as FPTM ( myFunc1 )

x = FPTM.myFunc1 10
y = FPTM.myFunc1 25
```

### Hiding import

Last thing you can do with import is to hide something.
It can be useful if there are name clashes. If you want to import everything from module except one (or several) functions it is the right way to use keyword `hiding`:

```haskell
import FPCourse.Lesson3.TestModule hiding ( myFunc3 )

x = FPTM.myFunc1 10
y = FPTM.myFunc1 25
```

## Task assignment

The homework to practice branching and slightly working with modules is in repository [MI-AFP/hw03](https://github.com/MI-AFP/hw03). 

## Further reading

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com) (chapters 4, 7)
* [Haskell: Pattern matching](https://en.wikibooks.org/wiki/Haskell/Pattern_matching)
* [Haskell: Control structures](https://en.wikibooks.org/wiki/Haskell/Control_structures)
* [Haskell: Modules](https://en.wikibooks.org/wiki/Haskell/Modules)
* [Haskell: Import](https://wiki.haskell.org/Import)
* [24 Days of GHC Extensions: Bang Patterns](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html)
