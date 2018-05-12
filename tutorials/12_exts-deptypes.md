# GHC Extensions and Dependent Types

## GHC Language Extensions

Language extensions are used to enable language features in Haskell that may seem useful in certain cases. They can be used to loosen restrictions in the type system or add completely new language constructs to Haskell. As you already know, they can be enabled using the `{-# LANGUAGE <ext> #-}` pragma or using flags `-X<ext>`. You should always consider using those extensions over normal Haskell, because it may also bring some risks.

### TypeFamilies

This extension allows use and definition of indexed type and data families to facilitate type-level programming. Indexed type families, or type families for short, are type constructors that represent sets of types. Set members are denoted by supplying the type family constructor with type parameters, which are called type indices. The difference between vanilla parametrized type constructors and family constructors is much like between parametrically polymorphic functions and (ad-hoc polymorphic) methods of type classes. Parametric polymorphic functions behave the same in all type instances, whereas class methods can change their behavior in dependence on the class type parameters. Similarly, vanilla type constructors imply the same data representation for all type instances, but family constructors can have varying representation types for varying type indices. (see [GHC docs](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families))

```haskell
{-# LANGUAGE TypeFamilies #-}

-- Declare a list-like data family
data family XList a

-- Declare a list-like instance for Char
data instance XList Char = XCons !Char !(XList Char) | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int

class XLength a where
  xlength :: XList a -> Int

instance XLength Char where
  xlength XNil = 0
  xlength (XCons _ r) = 1 + xlength r

instance XLength () where
  xlength (XListUnit _) = 1
```

### GADTs

[Generalized algebraic data type](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type) are a generalization of the algebraic data types that you are familiar with. Basically, they allow you to explicitly write down the types of the constructors. In this chapter, you'll learn why this is useful and how to declare your own. GADTs are mainly used to implement domain-specific languages, and so this section will introduce them with a corresponding example.

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Expr Int -> Expr Int -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool

eval :: Expr a -> a
eval (I n) = n  -- return Int
eval (B b) = b  -- returns bool
eval (Add e1 e2) = eval e1 + eval e2 -- return Int
eval (Mul e1 e2) = eval e1 * eval e2 -- return Int
eval (Eq  e1 e2) = eval e1 == eval e2  -- returns bool
eval (And e1 e2) = eval e1 && eval e2  -- returns bool
```

Complete example: [Haskell - GADT](https://en.wikibooks.org/wiki/Haskell/GADT)

### QuasiQuotes

[Quasiquoting](https://wiki.haskell.org/Quasiquotation) allows programmers to use custom, domain-specific syntax to construct fragments of their program. Along with Haskell's existing support for domain specific languages, you are now free to use new syntactic forms for your EDSLs. We've already seen it used in Yesod or Debug. Another simple use is with [Text.RawString.QQ](http://hackage.haskell.org/package/raw-strings-qq/docs/Text-RawString-QQ.html) to allow multiline strings:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ

multiline :: String
multiline = [r|<HTML>
<HEAD>
<TITLE>Auto-generated html formated source</TITLE>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1252">
</HEAD>
<BODY LINK="800080" BGCOLOR="#ffffff">
<P> </P>
<PRE>|]
```

You can, of course, write your own DSL or simplify syntax for yourself. All you have to do is implement your [QuasiQuoter](http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Quote.html#t:QuasiQuoter) (part of Template Haskell). For example, you can create a simple string-string map with semicolon and newlines:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module MapBuilder (mapBuilder) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Map Builder quasiquoter
mapBuilder = QuasiQuoter
    { quoteExp  = mapBuilderParser -- expressions
    , quotePat  = undefined        -- patterns
    , quoteType = undefined        -- types
    , quoteDec  = undefined        -- declarations
    }

-- | Split string to two parts by given char
splitByFirst :: String -> Char -> (String, String)
splitByFirst str sep = splitByFirst' "" str
  where
    splitByFirst' a [] = (a, "")
    splitByFirst' a (x:xs)
       | x == sep  = (a, xs)
       | otherwise = splitByFirst' (a++[x]) xs

-- | Trim spaces and tabs from left of the string
trimLeft :: String -> String
trimLeft "" = ""
trimLeft (x:xs)
  | x `elem` [' ', '\t'] = trimLeft xs
  | otherwise        = x : trimLeft xs

-- | Parse [(String, String)] map from String
mapBuilderParser :: String -> Q Exp
mapBuilderParser = return . ListE . map parseTuples . filter (/="") . map trimLeft . lines
  where
    parseTuples :: String -> Exp
    parseTuples xs = TupE [LitE . StringL $ key, LitE . StringL $ val]
      where
        parts = splitByFirst xs ':'
        key = fst parts
        val = snd parts
```

Then you can simply import defined quasiquoter and use it:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import MapBuilder

mymap1 :: [(String, String)]
mymap1 = [mapBuilder|
           a:10
           b:22
           c:hello
           it:has:no:problem
         |]

mymap2 :: [(String, Int)]
mymap2 = map strstr2strint [mapBuilder|
           suchama4:1210
           perglr:1535
         |]

strstr2strint :: (String, String) -> (String, Int)
strstr2strint (x, y) = (x, read y)
```

Beautiful, right?!

### Template Haskell

[Template Haskell](http://hackage.haskell.org/package/template-haskell) is a GHC extension to Haskell that adds compile-time metaprogramming facilities. The original design can be found here: http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/. You could have seen part of it in action in the previous section about quasiquoting but it can do much more although quasiquotes are an important part of it. Great explanation are [here](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html) and [here](https://markkarpov.com/tutorial/th.html).

## Dependent and Refinement Types

A dependent type is a type whose definition depends on a value. Such types can be for example:

* pair of integers where the second is greater than the first,
* people with age between 18 and 65,
* string in email format (matches given regex).

Dependent types add complexity to a type system. Deciding the equality of dependent types in a program may require computations. If arbitrary values are allowed in dependent types, then deciding type equality may involve deciding whether two arbitrary programs produce the same result; hence type checking may become undecidable.

### Agda

[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) is a dependently typed functional programming language originally developed by Ulf Norell at Chalmers University of Technology with the implementation described in his PhD thesis. But current version, Agda 2, is completely rewritten in previous Agda from 1999.

Visit https://github.com/agda/agda where are some examples as well!

### Idris

[Idris](https://www.idris-lang.org) is a general-purpose purely functional programming language with dependent types, strict or optional lazy evaluation and features such as a totality checker. Idris is highly affected by Haskell and Agda which is also possible to see in the syntax.

Its features are influenced by Haskell and ML, and include:

* Full dependent types with dependent pattern matching
* Simple foreign function interface (to C)
* Compiler-supported interactive editing: the compiler helps you write code using the types
where clauses, with a rule, simple case expressions, pattern matching let and lambda bindings
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

LiquidHaskell is a static verifier for Haskell, based on Liquid Types. It allows annotating code with invariants that complement the invariants imposed by the types. These invariants are checked with an SMT solver. It is not about dependent types but [refinement types](https://en.wikipedia.org/wiki/Refinement_(computing)#Refinement_types) (you refine some defined type with rules not build it dependent from scratch).

Visit: https://ucsd-progsys.github.io/liquidhaskell-blog/

```haskell
{--! run liquid with no-termination -}

module SimpleRefinements where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude


-- |Simple Refinement Types

{-@ zero :: {v:Int | v = 0} @-}
zero     :: Int
zero     =  0

{-@ type Even = {v:Int | v mod 2 = 0} @-}

{-@ zero'' :: Even @-}
zero''     :: Int
zero''     =  0

-- |Lists

infixr `C`
data L a = N | C a (L a)

{-@ natList :: L Nat @-}
natList     :: L Int
natList     =  0 `C` 1 `C` 3 `C` N

{-@ evenList :: L Even @-}
evenList     :: L Int
evenList     =  0 `C` 2 `C` 8 `C` N
```

## Further reading

* [Haskell Wiki - Language extensions](https://wiki.haskell.org/Language_extensions)
* [24 Days of GHC Extensions](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html)
* [Agda](https://github.com/agda/agda)
* [Idris](https://www.idris-lang.org)
* [Idris - tutorial](http://docs.idris-lang.org/en/latest/tutorial/)
* [LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell-blog/)
