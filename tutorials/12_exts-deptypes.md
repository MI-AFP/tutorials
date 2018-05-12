# GHC Extensions and Dependent Types

## GHC Language Extensions

Language extensions are used to enable language features in Haskell that may seem useful in certain cases. They can be used to loosen restrictions in the type system or add completely new language constructs to Haskell. As you already know, they can be enabled using the `{-# LANGUAGE <ext> #-}` pragma or using flags `-X<ext>`. You should always consider using those extensions over normal Haskell, because it may also bring some risks.

### TypeFamilies

### RankNTypes

### GADTs

### DeriveGeneric

### QuasiQuotes

[Quasiquoting](https://wiki.haskell.org/Quasiquotation) allows programmers to use custom, domain-specific syntax to construct fragments of their program. Along with Haskell's existing support for domain specific languages, you are now free to use new syntactic forms for your EDSLs.

### Template Haskell

## Dependent and Refinement Types

A dependent type is a type whose definition depends on a value. Such types can be for example:

* pair of integers where the second is greater than the first,
* people with age between 18 and 65,
* string in email format (matches given regex).

Dependent types add complexity to a type system. Deciding the equality of dependent types in a program may require computations. If arbitrary values are allowed in dependent types, then deciding type equality may involve deciding whether two arbitrary programs produce the same result; hence type checking may become undecidable.

### Agda

[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) is a dependently typed functional programming language originally developed by Ulf Norell at Chalmers University of Technology with implementation described in his PhD thesis. But current version, Agda 2, is completely rewritten previous Agda from 1999.

Visit https://github.com/agda/agda where are some examples as well!

### Idris

[Idris](https://www.idris-lang.org) is a general-purpose purely functional programming language with dependent types, strict or optional lazy evaluation and features such as a totality checker. Idris is highly affected by Haskell and Agda which is also possible to see in the syntax.

Its features are influenced by Haskell and ML, and include:

* Full dependent types with dependent pattern matching
* Simple foreign function interface (to C)
* Compiler-supported interactive editing: the compiler helps you write code using the types
where clauses, with rule, simple case expressions, pattern matching let and lambda bindings
* Dependent records with projection and update
* Interfaces (similar to type classes in Haskell)
* Type-driven overloading resolution
* `do` notation and idiom brackets
* Indentation significant syntax
* Extensible syntax
* Cumulative universes
* Totality checking
* Hugs style interactive environment

On their website you can find a documentation with [examples](https://www.idris-lang.org/example/) such as Vectors:

```idris
infixr 5 ::

data Vect : Nat -> Type -> Type where
    Nil  : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

app : Vect n a -> Vect m a -> Vect (n + m) a
app Nil       ys = ys
app (x :: xs) ys = x :: app xs ys
```

### LiquidHaskell

## Further reading

* [Haskell Wiki - Language extensions](https://wiki.haskell.org/Language_extensions)
* [24 Days of GHC Extensions](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html)
