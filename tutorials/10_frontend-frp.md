# Frontend and FRP

In the previous tutorial, we focused on web frameworks and especially on building a backend and some frontend generation by blaze or hastache on the server-side. This time, we will cover building frontend apps that are standalone or communicate with backend via (REST) API. At the end of this tutorial, there is a section about very interesting concept *Functional Reactive Programming* that is important when building user interfaces.

## Haskell and Haskell-like frontends

### The JavaScript Problem

We all know what is JavaScript -- it is a dynamic, weakly typed, prototype-based and multi-paradigm programming language. Together with HTML and CSS, it is one of the three core technologies of the World Wide Web. JavaScript is used for interactive web pages and thus is an essential part of the most of modern web applications. These days, JavaScript is often also used for the server-side or even desktop applications...

As obvious from above, we need JavaScript. On the other hand, JavaScript has some issues that make working with it inconvenient and make developing software harder. Some things are improving with time (newer versions of [ECMAScript](https://en.wikipedia.org/wiki/ECMAScript)) but most of them remains from the very basic principles of the language: weak-typing, late binding, weird automatic conversions, `this` behaviour, and lack of static types. There are some solutions like [CoffeeScript](http://coffeescript.org) and [TypeScript](https://www.typescriptlang.org) that are dealing with some of those...

But since we are now Haskellists, we would like to have something even better - Haskell-like JavaScript to solve these problems. Luckilly, we are not only ones and there are already many solutions how to compile Haskell to JavaScript or even some other languages based on Haskell that are adapted for this very specific purpose.

Take a look at [Slant - What are the best solutions to "The JavaScript Problem"?](https://www.slant.co/topics/1515/~solutions-to-the-javascript-problem). We are going to look at some now!

### GHCJS

### Haste

### Miso

### PureScript

### Elm

## FRP - Functional Reactive Programming

Functional reactive programming (FRP) is a programming paradigm for reactive programming (asynchronous dataflow programming) using the building blocks of functional programming (e.g. map, reduce, filter). FRP has been used for programming graphical user interfaces (GUIs), robotics, and music, aiming to simplify these problems by explicitly modeling time.

There are several libraries for working with the FRP in Haskell with slightly different approaches. You can see the list [here](https://wiki.haskell.org/Functional_Reactive_Programming#Libraries).

### FRP principles

For better understanding what is FRP about and what are the basic concepts, please read https://gist.github.com/staltz/868e7e9bc2a7b8c1f754...

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

## Task assignment

The homework to create a simple frontend for described REST API is in repository [MI-AFP/hw10](https://github.com/MI-AFP/hw10).

## Further reading

* 
