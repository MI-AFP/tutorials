# Template Haskell

Template Haskell is an experimental language extension to the Haskell programming language implemented in the Glasgow Haskell Compiler (version 6 and later). In early incarnations it was also known as Template Meta-Haskell. It allows **compile-time metaprogramming** and **generative programming** by means of manipulating abstract syntax trees and 'splicing' results back into a program. The abstract syntax is represented using ordinary Haskell data types and the manipulations are performed using ordinary Haskell functions.

## Why Bother?

With Template Haskell (TH) you can write code that generates code (i.e. metaprogramming). Sometimes it is considered as bad and result of failing to address some problem in a normal programming way. On the other hand, in some situations it can really simplify your work when you need to really generate amount of similar and complex code from simpler specification. There are some typical use cases of Template Haskell:

* **Deriving of type class instances** - You know keyword `deriving` and we said that you can use it for basic typeclasses such as `Show`, `Read`, or `Ord`. Well, there are ways how to derive also othes. One is using [GHC.Generics](https://wiki.haskell.org/GHC.Generics) but that might slow down the compilation significantly. The other is TH and that way is used for example by [aeson](http://hackage.haskell.org/package/aeson) library when deriving instances for transformation from and to JSON.
* **Domain Specific Languages (DSLs)** - DSLs are trendy, nice, and cool way how to code things without actually writing the code itself but using different syntax in special quotes. They are integrated into systems built in Haskell. Examples of such DLSs are the language for model declaration used in [persistent](http://hackage.haskell.org/package/persistent), and various other mini-languages used in the [yesod](http://hackage.haskell.org/package/yesod) web framework that we have already shown.
* **Compile-time construction of values of refined types** - It simply turns invalid inputs into compilation failures.
* **Compile-time loading and processing of data from external files** - This is very useful sometimes to avoid loading resources on the beginning of every run of the application. Even though it involves running IO during compilation, it’s a relatively innocent use case of that dangerous feature.

You should always reconsider using Template Haskell if there is no easier way because it is considered as dark magic (with many black boxes), might slow down the compilation and can introduce hard-to-debug problems.

## Q Monad and Splicing

The core of TH is the `Q` monad (short for “quotation”) that hosts all functions provided by TH:

* Generating new unique names that cannot be captured.
* Retrieving information about a thing by its name. Usually we want to know about functions and types, but there are also ways to learn about a module, get collection of instances of a particular type class, etc.
* Putting and getting some custom state that is then shared by all TH code in the same module.
* Running IO during compilation, so we can e.g. read something from a file.

Everything needed for basic TH is in [template-haskell](http://hackage.haskell.org/package/template-haskell) package, including the definition of the `Q` monad and other types and typeclasses that we will mention afterwards.

```haskell
newtype Q a = Q { unQ :: forall m. Quasi m => m a }

runQ :: Quasi m => Q a -> m a
runQ (Q m) = m

instance Monad Q where
  Q m >>= k  = Q (m >>= \x -> unQ (k x))
  (>>) = (*>)
  fail       = Fail.fail

instance Fail.MonadFail Q where
  fail s     = report True s >> Q (Fail.fail "Q monad failure")

instance Functor Q where
  fmap f (Q x) = Q (fmap f x)

instance Applicative Q where
  pure x = Q (pure x)
  Q f <*> Q x = Q (f <*> x)
  Q m *> Q n = Q (m *> n)
```

What is the `Q a` good for? - To use `a` in a Haskell program somehow. It can be anything, but we want to insert something into the Haskell code and it can be one of the following:

* **Declaration** ([Dec](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Dec)) = top-level things like function and data type definitions
* **Expression** ([Exp](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Exp)) = some expression such as `if` statement, `5 + 3`, or literal `"Hello"`
* **Typed expression** ([TExp](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:TExp)) = identical to **Expression** but also defines a type of the expression "inside"
* **Type** ([Type](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Type)) = type of any kind, for example, `Integer`, `Maybe`, `Int -> Double`, or `Either String`, including type constraints and wildcards such as `Num a => a`
* **Pattern** ([Pat](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Pat)) = pattern for matching in function, or `case`, `where`, and `let in`

Using those you can construct on meta-level anything that is possible to write in Haskell as a code (including Template Haskell). For example, this is how you can write in TH Haskell lambda expression `\x -> x + 1`:

```haskell
lambdaXplus1 :: Q Exp
lambdaXplus1 = do
  x <- newName "x" -- unique variable name
  return $ LamE    -- lambda expression
    [VarP x]       -- pattern matching on 'x'
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
```

Such TH expression can be then used in Haskell:

```
GHCi> :set -XTemplateHaskell
GHCi> $(lambdaXplus1) 3
4
GHCi> $lambdaXplus1 3
4
GHCi> let f = (* 2) . $myFunc
GHCi> f 10
22
```

This is called **splicing** and the expression immediately following the `$` is called **splice**. If splice is parametric, you have to use brackets (e.g. `$(mySplice arg1 arg2) 7`), otherwise you may omit them as we did in the example above. Splice is ten evaluated to its Haskell form.

A splice can be in four places in the code:

* expression
* pattern
* type
* top-level declaration

The top-level declaration splices can omit `$` since there is no ambiguity (like with the `($)` operator). Well known use of this is with lenses.

```haskell
makeLens ''MyType
-- is the same as:
$(makeLens ''MyType)
```

## All the Quotations

Doing the meta-programming in the way as shown above is not very handy and may lead to uncompilable code. Luckily, there are quotes (different for various types of code that can be generated):

* **Declaration** = `[d| ... |]` of type `Q [Dec]`
* **Expression** = `[e| ... |]` of type `Q Exp`
* **Typed expression** = `[|| ... ||]` of type `Q (TExp a)`
* **Type** = `[t| ... |]` of type `Q Type`
* **Pattern** = `[p| ... |]` of type `Q Pat`

In this way, the `Q`'d expression above can be rewritten easily as:

```haskell
lambdaXplus1 :: Q Exp
lambdaXplus1 = [| \x -> x + 1 |]

lambdasXplus2 :: Q Exp
lambdasXplus2 = [| $lambdaXplus1 . $lambdaXplus1 |
```

If you want to work with typed expression so the compiler can ensure that the phantom type always corresponds to what is inside, you must state the specific type:

```haskell
lambdaXplus1 :: Q (TExp (Integer -> Integer))
lambdaXplus1 = [|| \x -> x + 1 ||]
```

If you want to use typed expression in other expression you must do doubled splicing `$$`. Normal splices cannot be used in quotations for typed expressions and vice versa. Why? Of course, you must know the type in typed. For the other way, you may use `unType :: TExp a -> Exp` that gets rid of the type.

## Quasi and runQ

If you want to play with `Q` monad in GHCi (and apps as well) you might need `runQ` of type `Quasi m => Q a -> m a`. It is because you want to work in `IO` monad and `IO` monad is one of instances of the `Quasi` typeclass. `Quasi` is the type class for monads that provide all the capabilities for meta-programming we have mentioned in the beginning when we introduced `Q`.

```
GHCi> runQ [e| Just x |]
AppE (ConE GHC.Base.Just) (UnboundVarE x)
GHCi> runQ [p| Just x |]
ConP GHC.Base.Just [VarP x_0]
GHCi> runQ [| Just x |]
AppE (ConE GHC.Base.Just) (UnboundVarE x)
```

## Possibilities and Limitations

There are many more options to do with Template Haskell especially when it comes to names (like with `makeLens ''MyType` or defining variables). You can, for example, then also reifying things using `reify :: Name -> Q Info`, `reifyInstances :: Name -> [Type] -> Q [InstanceDec]`, and others. But there are also some limitiations that you might encounter and have to deal with, for example, inside a splice one can only use functions that are already compiled and sometimes order of definition matters in TH (which is not the case for Haskell).

## References

* [Haskell Wiki - A practical Template Haskell Tutorial](https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial)
* [Template Haskell tutorial (Mark Karpov)](https://markkarpov.com/tutorial/th.html)
* [Wikipedia - Template Haskell](https://en.wikipedia.org/wiki/Template_Haskell)
* [Template Haskell Is Not Scary](https://www.parsonsmatt.org/2015/11/15/template_haskell.html)
* [24 Days of GHC Extensions: Template Haskell](https://ocharles.org.uk/guest-posts/2014-12-22-template-haskell.html)
* [Syntax of Template Haskell and Quasiquotes](https://riptutorial.com/haskell/example/18471/syntax-of-template-haskell-and-quasiquotes)
* [Intro to Template Haskell](https://typeclasses.com/news/2018-10-intro-template-haskell)
* [Template Haskell 101](https://www.schoolofhaskell.com/user/marcin/template-haskell-101)
