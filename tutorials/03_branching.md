# Structuration and branching

## If and case

As in other languages, also in Haskell you can use `if-then-else` and `case-of` (similar to `switch-case`) expression for branching the computation. But here it has a different meaning: as we are in a side-effect-free environment, it decides about a result variant. Although you've already seen and used `if-then-else` expression during this course, we will look at it in higher detail now.

### Own ifThenElse

First, let's try to implement own function realising the `if-then-else` branching. What would be the type of such function? We need the condition which is obviously of type `Bool` and then there are two expressions, one is evaluated in case the condition is `True` and the other one in case it is `False`. We can allow any type of such expression by type variable `a` and it will represent the returned type, as well.

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse condition onTrue onFalse = ...
```

But how to implement it without actually using `if-then-else` keywords, so we do not get into a cycle? By pattern matching:

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  onTrue onFalse = onTrue
ifThenElse False onTrue onFalse = onFalse
```

We can even simplify it a bit with ignoring the unneeded argument using `_`:

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

We can see that we are able to implement very simply our own `ifThenElse` but it is not very readable and using nested "ifs" would be even worse. Fortunately, there is the syntax you would expect available in Haskell:

```
*Main> if (x < y) then (y - x) else (x - y)
35
*Main> if (x < y) then (y - x) else if (x == y) then 0 else (x - y)
35
*Main> y = 50
*Main> if (x < y) then (y - x) else if (x == y) then 0 else (x - y)
0
```

And nested conditions:

```
*Main> if (x < y) then (y - x) else (if (x == y) then 0 else (x - y))
0
```

However, our little exercise enabled us to see the important fact: `if-then-else` constructs are NOT control flow, but expressions providing "conditioned results". It has some type and thus you can not allow type mismatch in the branches:

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

Instead of `switch-case` Haskell offers `case-of` expression as a convenient syntactic sugar of `if-then-else`:

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

You need to be careful that you cover all the cases with `case-of`. If you hit some case which is not covered an exception will come up at runtime:

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

As you can see, it uses pattern matching, as it was introduced in the previous lesson. It is the full-featured pattern matching, so matching for lists works, as well.

```haskell
describeList :: [a] -> String
describeList xs = "The given list has " ++ case xs of []  -> "no item."
                                                  [x] -> "just one item."
                                                  _  -> "more than one item."
```

## Guards and patterns

Another widely used way how to create branches is by guards in function declarations. That allows you much more than matching shown in `ifThenElse`. Then you can also use patterns to easily work with some structures in branching.

### Guards

Guards are written using `|` operator-like keyword after introducing a function name and arguments as in the following example. It may remind you of mathematical definitions of functions. You can write clearly readable Boolean expressions and expected results instead of nested "ifs". The `otherwise` has the same meaning as `True` and it provides a default branch similarly to `_` in the `case-of` expression.

```haskell
myMax :: (Ord a) => a -> a -> a
myMax a b
    | a > b     = a
    | otherwise = b
```

The order of testing guards is from top to bottom and the first being `True` will be applied. Try it with:

```haskell
guardsOrder x
    | x < 5 = "x < 5"
    | x < 0 = "x < 0"
    | x > 2 = "x > 1"
    | otherwise = "otherwise"
```

It is just important not to forger the difference of `_` ("anything" in `case-of`) and `otherwise` ("True" in guards).

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

As we said, pattern matching in a function declaration is the same as pattern matching of `case-of` and of guards. Actually, it is just [syntactic sugar](https://en.wikibooks.org/wiki/Haskell/Syntactic_sugar#Function_Bindings) and the following two functions are equivalent.

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

In some cases, if you are matching a pattern with a value, it may be useful to bind a name to the whole value being matched (when you need to use decomposition and the whole as well). As-patterns allow exactly this: they are in the form `name@pattern` and in addition to acting as a regular pattern, it binds the `name` to the whole value being matched by `pattern`. Again, it can be used not just with lists, but also with any other types, such as records.

```haskell
duplicateFirstElement1 [] = []
duplicateFirstElement1 (x:xs) = x:x:xs

duplicateFirstElement2 [] = []
duplicateFirstElement2 list@(x:xs) = x:list

duplicateFirstElement3 [] = []
duplicateFirstElement3 list@(x:_) = x:list

foo person@Person{name, age} = -- something
```

## Let in, where

You can define an expression (function or constant) at module (file) level, but then it can be used everywhere in that module/file. If you want to structure your code well and define some local expressions you have two basic ways how to do it - with `let-in` or `where` keywords.

### Let in

`let ... in ...` is an expression that can be written in any place where you can write expressions. After `let` you define expressions (called bindings) that you can then use in the following expression (the one after `in`). It is one of the ways how to locally introduce named expression:

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

As you can see, a local binding serves two purposes:

1. Enables reuse ([DRY!](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself))
2. Provides a way how to give a name to an expression, thus improving readability of your code.

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

Sometimes you need to use `let-in` on one line, then you should use `;` or a tuple-like binding. Again it is nothing else than expression.

```
*Main> let a = 10; b = 20; c = 30 in a*b*c
600000
*Main> let (a, b, c) = (10, 20, 30) in a*b*c
6000
*Main> :t let (a, b, c) = (10, 20, 30) in a*b*c
let (a, b, c) = (10, 20, 30) in a*b*c :: Num a => a
```

Semicolon is generally a way, how to put more expressions on a single line. However, use it sparingly, it is rather a readability anti-pattern.

### Where

This construct is similar to `let-in`, but the bindings come *after* the expression, similar to mathematical definitions:

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

In contrast to `let-in`, `where` cannot be used everywhere as an expression.

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

Local bindings after `where` can also be type-annotated and it is generally a good practice, which results in a more expressive code and better type error messages for more complex expressions.

```haskell
circleArea radius = pi * radius^2
  where
    pi :: Float
    pi = 3.14159
```

Do not be afraid to nest `where` several levels, this is a nice way how to make your code nicely readable top-down. At some point, you may find your local bindings usable outside, so you just take it and make a global function from it:

```haskell
pi :: Float
pi = 3.14159

circleArea radius = pi * radius^2
```

### Where with guards

Typical and pretty use is with guards to achieve DRY:

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

Please be aware that indentation is important in Haskell; Try to move where one space and right and see what happens. This is a common (not only) beginners' trouble. The same holds for `let-in`. We encourage you to take all the previous examples and play with indentation to see, how wrong indentation breaks the expression.

## Evaluation, patterns, and wildcards

Let us know touch evaluation strategy of Haskell to understand, why it is good to use such things as wildcards and also to be able to understand "bang patterns".

### Bottom

To talk about laziness we often use the term [bottom](https://wiki.haskell.org/Bottom) (&#8869; or `_|_`) which means *a computation that never completes successfully*. Various things can happen - a computation can fail due to some kind of error or it goes into an infinite loop. The term bottom is related to value, as well as to type. Examine those with GHCi:

```haskell
data BottomType = BottomType   -- same as unit "()"

bottomValue = bottomValue -- endless
errorBottom = error "Reached some error"
undefBottom = undefined
```

### Haskell is lazy

Haskell has a lazy non-strict evaluation strategy. It means that no expression is evaluated unless the value is needed. This brings amazing possibilites, mostly creating infinite structures, typically lists:

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

Sometimes, you may want to enforce a strict evaluation because of performance. There is the `!` symbol and you can read more about it [here](https://wiki.haskell.org/Performance/Strictness). Obviously, bad things will happen if your code contains infinite list or recursion which never ends - you will need to terminate the program!

In a similar way, you can enforce strictness when declaring a data type:

```haskell
data Pet = Pet !String Int

data Person = Person { username :: !String
                     , realname :: !String
                     }
```

However, strictness is a rather advanced topic, which you do not need to worry about now ;-).

### Function application operator

There is a binary operator `$` called function application and at first sight, it looks quite lame: As its left operand, it takes a function and on right side, there is the operand for the function. So instead of: `func operand`, we write `func $ operand`. Huh?! The point is that '$' has a very low priority, so it is used to avoid brackets for function composition (a very common thing in Haskell, will be covered later). For completeness, there is also a strict application `$!$`.

```haskell
show (getSiblingsOf (getParentOf (head people)))

show $ getSiblingsOf $ getParentOf $ head people
```

## Modules and imports

A Haskell program consists of a collection of modules (similar to other programming languages). In the top level, you can declare and define data types, function, typeclasses and their instances, pattern bindings and so on.

### Module specification

Every file forms a module. If there is no specification of the module name, `Main` is used by default, as we saw. A module name must start with a capital letter and then it is an alphanumeric string. Although there is no formal connection between filesystem and modules, if you use GHC (and we do), your module name should reflect the filesystem name. For example module `FPCourse.Lesson3.TestModule` would be placed in `FPCourse/Lesson3/TestModule.hs` with content:

```haskell
module FPCourse.Lesson3.TestModule (
    myFunc1, myFunc3
) where

myFunc1 x = myFun2 7 x

myFunc2 x y = x - y

myFunc3 x y z = x * y + z
```

Notice that after the module name, there is an optional list of stuff that can be imported from this module. In this case, you can import `myFunc1` and `myFunc3`, but not `myFunc2`.

### Import something

How to import something from a different module? As in other languages, use the `import` keyword:

```haskell
import FPCourse.Lesson3.TestModule

x = myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc1 25
```

Plain import will allow you to use all functions exposed from the module with unqualified or qualified (with module name) names.

You can also specify what do you want to import explicitly:

```haskell
import FPCourse.Lesson3.TestModule ( myFunc1 )

x = myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc1 25
```

In this case, just `myFunc1` is imported. It is a recommended best practice *always* import just things you need, otherwise you pollute your namespace and you may get into name clashes. Also, explicit imports enhance code readability ("Where the heck has this function come from?!")

### Qualified import

Another option how to avoid polluting your namespace is using qualified imports:

```haskell
import qualified FPCourse.Lesson3.TestModule

x = FPCourse.Lesson3.TestModule.myFunc1 10
y = FPCourse.Lesson3.TestModule.myFunc2 25
```

You then need to access the exported functions just by the fully qualified name. In this variant, you may also name explicit imported functions, however it is not that important, as you do not pollute your namespace and do not loose track of origins of functions:

```haskell
import qualified FPCourse.Lesson3.TestModule ( myFunc1 )

x = FPCourse.Lesson3.TestModule.myFunc1 10
```

As you can see, writing qualified name can be lenghty. This is when you introduce an alias for the module name with the keyword `as`:

```haskell
import qualified FPCourse.Lesson3.TestModule as FPTM

x = FPTM.myFunc1 10
y = FPTM.myFunc2 25
```

This is a usual way when importing a package, which overrides some standard (Prelude) functions:

```haskell
import Data.Set (size) -- name clash!
```

```haskell
import qualified Data.Set as S
size [1, 2, 2, 3]

size (fromList [1, 2, 2, 3]) -- wrong size!

S.size (fromList [1, 2, 2, 3]) -- correct!
```

### Hiding import

Another thing you can do with import is to hide something, which can be also used to solve name clashes: If you want to import everything from module except one (or several) functions it is the right way to use keyword `hiding`:

```haskell
import FPCourse.Lesson3.TestModule hiding ( myFunc3 )

myFunc3 :: Int -> Int -- now our function does not clash
myFunc3 = ...

x = myFunc1 10 -- a function from TestModule
y = FPTM.myFunc1 25
```

## Task assignment

The homework to practice branching and slightly working with modules is in repository [MI-AFP/hw03](https://github.com/MI-AFP/hw03).

## Further reading

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com) (chapters 4, 7)
* [Haskell: Pattern matching](https://en.wikibooks.org/wiki/Haskell/Pattern_matching)
* [Haskell: Control structures](https://en.wikibooks.org/wiki/Haskell/Control_structures)
* [Haskell: Laziness](https://en.wikibooks.org/wiki/Haskell/Lazines)
* [Haskell: Modules](https://en.wikibooks.org/wiki/Haskell/Modules)
* [Haskell: Import](https://wiki.haskell.org/Import)
* [Haskell: Import modules properly](https://wiki.haskell.org/Import_modules_properly).
* [24 Days of GHC Extensions: Bang Patterns](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html)
