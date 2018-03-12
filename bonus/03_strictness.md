
### Haskell can be strict

For enforcing strictness there is the `!` symbol and you can read more about the usage [here](https://wiki.haskell.org/Performance/Strictness). Obviously, bad things will happen if your code contains infinite list or recursion which never ends - you will need to terminate the program!

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

In a similar way, you can enforce strictness when declaring data type.

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

### Lazy pattern matching

On the other hand, sometimes the other extreme might be useful - i.e. being lazier (or irrefutable). When a pattern is being matched, for example, `func (Just x) = "It is just x"`,  passed argument is evaluated to form when it can be matched with a pattern. So, if you call `foo undefined` it will turn into error even if you won't use `x` on the right side at all. To avoid that, you can use `~` (tilde) like `!`. Try to test with these:

```haskell
func1  (Just x) = "It is just x"

func2  (Just _) = "It is just something"

func3 ~(Just x) = "It is just x (or something else?)"

func4 ~(Just x) = "It is just " ++ show x

func5 ~Nothing  = "It is nothing"
func5 ~(Just x) = "It is just " ++ show x
```
