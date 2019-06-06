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

Using those you can construct on meta-level anything that is possible to write in Haskell as a code.

## All the Quotations

## Possibilities and Limitations

## References

* [Haskell Wiki - A practical Template Haskell Tutorial](https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial)
* [Template Haskell tutorial (Mark Karpov)](https://markkarpov.com/tutorial/th.html)
* [Wikipedia - Template Haskell](https://en.wikipedia.org/wiki/Template_Haskell)
* [Template Haskell Is Not Scary](https://www.parsonsmatt.org/2015/11/15/template_haskell.html)
* [24 Days of GHC Extensions: Template Haskell](https://ocharles.org.uk/guest-posts/2014-12-22-template-haskell.html)
* [Syntax of Template Haskell and Quasiquotes](https://riptutorial.com/haskell/example/18471/syntax-of-template-haskell-and-quasiquotes)
* [Intro to Template Haskell](https://typeclasses.com/news/2018-10-intro-template-haskell)
* [Template Haskell 101](https://www.schoolofhaskell.com/user/marcin/template-haskell-101)
