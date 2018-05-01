# Frontend and FRP

In the previous tutorial, we focused on web frameworks and especially on building a backend and some frontend generation by blaze or hastache on the server-side. This time, we will cover building frontend apps that are standalone or communicate with backend via (REST) API. At the end of this tutorial, there is a section about very interesting concept *Functional Reactive Programming* that is important when building user interfaces.

## Haskell and Haskell-like frontends

### The JavaScript Problem

We all know what is JavaScript -- it is a dynamic, weakly typed, prototype-based and multi-paradigm programming language. Together with HTML and CSS, it is one of the three core technologies of the World Wide Web. JavaScript is used for interactive web pages and thus is an essential part of the most of modern web applications. These days, JavaScript is often also used for the server-side or even desktop applications...

As obvious from above, we need JavaScript. On the other hand, JavaScript has some issues that make working with it inconvenient and make developing software harder. Some things are improving with time (newer versions of [ECMAScript](https://en.wikipedia.org/wiki/ECMAScript)) but most of them remains from the very basic principles of the language: weak-typing, late binding, weird automatic conversions, `this` behaviour, and lack of static types. There are some solutions like [CoffeeScript](http://coffeescript.org) and [TypeScript](https://www.typescriptlang.org) that are dealing with some of those...

But since we are now Haskellists, we would like to have something even better - Haskell-like JavaScript to solve these problems. Luckilly, we are not only ones and there are already many solutions how to compile Haskell to JavaScript or even some other languages based on Haskell that are adapted for this very specific purpose.

Take a look at [Slant - What are the best solutions to "The JavaScript Problem"?](https://www.slant.co/topics/1515/~solutions-to-the-javascript-problem). We are going to look at some now!

### GHCJS

GHCJS is a Haskell to JavaScript compiler that uses the GHC API.

GHCJS supports many modern Haskell features, including:

 * All type system extensions supported by GHC
 * Lightweight preemptive threading with blackholes, MVar, STM, asynchronous exceptions
 * Weak references, CAF deallocation, StableName, StablePtr
 * Unboxed arrays, emulated pointers
 * Integer support through [JSBN](http://www-cs-students.stanford.edu/~tjw/jsbn/), 32 and 64 bit signed and unsigned arithmetic (`Word64`, `Int32` etc.)
 * Cost-centres, stack traces
 * Cabal support, GHCJS has its own package database

And some JavaScript-specific features:

 * new JavaScriptFFI extension, with convenient import patterns, asynchronous FFI and a JSVal FFI type,
 * synchronous and asynchronous threads.
 
- Project: [ghcjs/ghcjs](https://github.com/ghcjs/ghcjs)
- Nice example: [Full stack web Haskell with Servant and GHCJS](http://blog.wuzzeb.org/full-stack-web-haskell/index.html) 

### Haste

[Haste](https://haste-lang.org) is an implementation of the Haskell functional programming language, geared towards web applications. Haste is based on the GHC, which means that it supports the full Haskell language, including GHC extensions and produces highly optimized code but comes with an extended set of standard libraries. Haste support modern web technologies such as WebSockets, LocalStorage, Canvas, etc. out of the box. In addition, Haste comes prepackaged with facilities for preemptive multitasking, working with binary data and other niceties.

A Haste program can be compiled into a single JavaScript file, much like traditional browser-side programs, or into a JavaScript file and a server-side binary, with strongly typed communication between the two. In essence, Haste lets you write your client-server web application as a single, type-safe program, rather than two separate programs that just happen to talk to each other over some web API as is traditional.

You don’t need to throw away all of your old code to start using Haste. In addition to the standard Haskell FFI, Haste provides its own flexible mechanism for easy Haskell-JavaScript integration, using fancy type magic to allow data of any type to be used by both Haskell and JavaScript code with minimal effort.

Haste programs are compact. While a certain increase in code size over hand-rolled JavaScript is unavoidable, an optimized but uncompressed Haste program is normally less than 3x the size of an equivalent hand-written program and the compiler takes special care to produce minifiable code, making the latency penalty of using Haste minimal.

- Examples: [valderman/haste-compiler](https://github.com/valderman/haste-compiler/tree/master/examples)
- API doc: [haste-compiler-0.5.5.0: Haskell To ECMAScript compiler](https://haste-lang.org/docs/haddock/0.5.5/)
- Our example: [DataStewardshipWizard/ds-wizard](https://github.com/DataStewardshipWizard/ds-wizard) and [DataStewardshipWizard/ds-form-engine](https://github.com/DataStewardshipWizard/ds-form-engine)

### Miso

**Miso** is a small "[isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/)" [Haskell](https://www.haskell.org/) front-end framework for quickly building highly interactive single-page web applications. It features a virtual-dom, diffing / patching algorithm, attribute and property normalization, event delegation, event batching, SVG, Server-sent events, Websockets, type-safe [servant](https://haskell-servant.github.io/)-style routing and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). **Miso** is pure by default, but side effects (like `XHR`) can be introduced into the system via the `Effect` data type. **Miso** makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies. **Miso** can be considered a shallow [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language) for modern web programming. ([dmjio/miso](https://github.com/dmjio/miso/edit/master/README.md))

### PureScript

PureScript is a strict, purely functional programming language inspired by Haskell which compiles to readable JavaScript with a simple foreign function interface and no runtime dependency. Although you might this it is just the same as Haskell - there are few [differences](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md) that are good for the to-JavaScript compilation.

- Website: [purescript.org](http://www.purescript.org)
- Guide: [leanpub.com/purescript](https://leanpub.com/purescript/read)

### Elm

Elm is a functional language that compiles to JavaScript. It is not a Haskell but language inspired and in some ways very similar to Haskell (see [main differences](https://gist.github.com/cobalamin/c1b83f5626df1409b512ce2faf05cf84)) - it is more different from Haskell than PureScript. It competes with projects like React as a tool for creating websites and web apps. Elm has a very strong emphasis on simplicity, ease-of-use, and quality tooling. Compiler of Elm is written in Haskell and you can work with Elm in Haskell with [Language.Elm](https://hackage.haskell.org/package/Elm).

- Guide: [guide.elm-lang.org](https://guide.elm-lang.org)
- Examples: [elm-lang.org/examples](http://elm-lang.org/examples)
- Our example: [DataStewardshipWizard/dsw-client](https://github.com/DataStewardshipWizard/dsw-client)

## FRP - Functional Reactive Programming

Functional reactive programming (FRP) is a programming paradigm for asynchronous dataflow programming using the building blocks of functional programming (such as `map`, `filter`, `fold`s, higher-order functions, etc.). It has been used often for programming graphical user interfaces (GUIs), robotics, and music, aiming to simplify these problems by explicitly modeling time. Good example to imagine what is it about is spreadsheet calculator. You have cells that computes something from different cells and when you edit some, related will recalculate - you do not tell what should be recalculated nor recalculate all but just those where the change will propagate. See? It is action and reaction!

There are several libraries for working with the FRP in Haskell with slightly different approaches. You can see the list [here](https://wiki.haskell.org/Functional_Reactive_Programming#Libraries).

### FRP principles

For better understanding what is FRP about and what are the basic concepts, please read [The introduction to Reactive Programming you've been missing (by @andrestaltz)](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)...

### Reactive

[Reactive](https://hackage.haskell.org/package/reactive) is a simple foundation for programming reactive systems functionally. Like Fran/FRP, it has a notions of (reactive) behaviors and events. Unlike most previous FRP implementations, Reactive has a hybrid demand/data-driven implementation, as described in the paper "Push-pull functional reactive programming", http://conal.net/papers/push-pull-frp/.

Sadly the documentation, tutorials and examples are not currently in a good condition.

### Reactive-banana

[Reactive-banana](https://wiki.haskell.org/Reactive-banana) is meant to be used in conjunction with existing libraries that are specific to your problem domain. For instance, you can hook it into any event-based GUI framework, like wxHaskell or Gtk2Hs. Several helper packages like reactive-banana-wx provide a small amount of glue code that can make life easier.

The goal of the library is to provide a solid foundation.

* Programmers interested implementing FRP will have a reference for a simple semantics with a working implementation. The library stays close to the semantics pioneered by Conal Elliott.
* The library features an efficient implementation. No more spooky time leaks, predicting space & time usage should be straightforward.
* A plethora of [example code](https://wiki.haskell.org/Reactive-banana/Examples) helps with getting started.

### Yampa

[Yampa](https://wiki.haskell.org/Yampa) is a domain-specific embedded language for the programming of hybrid (discrete and continuous time) systems using the concepts of Functional Reactive Programming (FRP). Yampa is structured using Arrows, which greatly reduce the chance of introducing space- and time-leaks into reactive, time-varying systems.

![Signals in Yampa](https://wiki.haskell.org/wikiupload/thumb/1/10/Yampa_signal_functions.svg/624px-Yampa_signal_functions.svg.png)

## Reactive programming with Elm

//TODO: example of simple reactive app with Elm (converting units)

## Task assignment

The homework to create a simple frontend for described REST API is in repository [MI-AFP/hw10](https://github.com/MI-AFP/hw10).

## Further reading

* [Haskell on the front end](https://www.reddit.com/r/haskell/comments/7ax2ji/haskell_on_the_front_end/)
* [Zdroják.cz - Elm (czech only)](https://www.zdrojak.cz/clanky/elm-uvod/)
* [gelisam/frp-zoo (FRP libs comparison)](https://github.com/gelisam/frp-zoo)
* [FRP explanation using reactive-banana](https://wiki.haskell.org/FRP_explanation_using_reactive-banana)
