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

As you can see, it uses pattern matching as it was introduced in the previous lesson. Without any surprise it is then possible to use pattern matching for lists as well.

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

### Wildcards and patterns

### List guards

### Named patterns

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
