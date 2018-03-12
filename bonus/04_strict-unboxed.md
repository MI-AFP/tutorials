### Boxed vs. Unboxed types

Last theoretical topic that we are going to briefly mention is the difference between boxed and unboxed types. Although it is a low-level concern and with regular Haskell programming, you can avoid these terms, it is good to know what is it about when you see it in other's code or in a documentation.

To support laziness, parametric polymorphism, and other properties, by default Haskell data types are represented uniformly as a pointer to a closure on the heap. These are "boxed" values. An unboxed is represented directly by raw value (i.e., without any indirection). Using unboxed types can lead to time/space optimizations. Having always pointers to a heap-allocated object is fairly slow, so compilers attempt to replace these boxed values with unboxed raw values when possible. Unboxed values are a feature of some compilers that allow directly manipulating these low-level values. Since they behave differently than normal Haskell types, generally the type system is extended to type these unboxed values.

In [GHC], unboxed values have a hash mark as a suffix to their name. For instance, the unboxed representation of 42 is 42#. However, you can't pass them to polymorphic functions (like `show` for instance). To allow that, you need to use constructor `I#` that takes an unboxed integer and returns the `Int` (wraps). You can observe [kind](https://wiki.haskell.org/Kind) (*kind of type*, we will look again at kinds with typeclasses) of boxed and unboxed types:

* By default, kind of type is `*` (try in GHCi: `:kind Int`)
* Kind of unboxed type is `#` (try in GHCi: `:kind Int#`, first do `:set -fglasgow-exts`)

```haskell
import GHC.Exts

showUnboxedInt   :: Int# -> String
showUnboxedInt n = "Unboxed: " ++ (show $ I# n) ++ "#"
```

(If you find kinds interesting, try to examine `:kind Maybe` and `:kind Either`.)

### Strictness with types

In the previous lesson, we touched the topic of enforcing strictness with `!` in patterns ([bang patterns](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html)) and in function application with `$!` operator. Similarly, we can use `!` with type fields like this:

```haskell
data MyType = MyConstr Int !Int

data MyRec = MyRecConstr { xA ::  Int
                         , xB :: !Int
                         }
```

For both cases it means that when data constructor is evaluated, it must fully evaluate ([weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)) the second parameter, but the first one will stay unevaluated in a lazy way. All depends on language implementation in the used compiler.

#### Unpacking strict fields

One of the most used optimization techniques when talking about unboxed types and strictness with [GHC] is [unpacking strict fields](https://wiki.haskell.org/Performance/Data_types#Unpacking_strict_fields). When a constructor field is marked strict, and it is a single-constructor type, then it is possible to ask GHC to unpack the contents of the field directly in its parent with `{-# UNPACK #-}` pragma:

```haskell
data T1 = T1 {-# UNPACK #-} !(Int, Float)  -- => T1 Int Float
data T2 = T2 Double {-# UNPACK #-} !Int    -- => T2 Double Int#
```

We mention this just because of differences in performance of types we are going to describe now. You don't need to use strict or unboxed types within your work if you don't need to have time/space optimizations...
