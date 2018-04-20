# Web application in Haskell

Haskell can be (of course) used for network communication and also for building various web applications. In this tutorial, we are going to look at basics of network communication in Haskell, some specialized libraries making it simplet, and then at web frameworks. 

## Network communication

### Sockets

### Server-client demo

### Specialized libraries

## Web Frameworks overview

As with other languages, you usually don't want to build web application from scratch which would bind ports, listen and parse requests and compose responses. For better abstraction you want to use a web framework.

There are several frameworks in Haskell (see [here](https://wiki.haskell.org/Web/Frameworks)). We are going to show briefly Snap and Yesod because they are used quite often and then we will show more with our favorite Scotty. 

### Snap

[Snap](http://snapframework.com) is a simple web development framework for unix systems, written in the Haskell programming language. It consists of:

* A fast HTTP server library
* A sensible and clean monad for web programming
* An HTML-based templating system for generating pages (heist)

More examples are in the [documentation](http://snapframework.com/docs).

#### "Hello World"

```haskell
import Snap

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

main :: IO ()
main = quickHttpServe site
```

#### Snaplets

Snap also has very nice philosophy in form of an optional system for building reusable pieces web functionality called “snaplets”. Snaplets make it easy to share and reuse common code across multiple web apps. The default snaplets let you get a full-featured web application up and running in no time.

If you want to build such application read [this](http://snapframework.com/docs/tutorials/snaplets-tutorial).

### Yesod

[Yesod](https://www.yesodweb.com) is a Haskell web framework for productive development of type-safe, RESTful, high performance web applications. It build on Haskell features such as compile-time errors (instead of runtime), seamlessly asynchronous computation, scalability, good performance and light-weight syntax.

Another advantage of Yesod is comprehensive documentation including:

* [quick start guide](https://www.yesodweb.com/page/quickstart),
* [book](https://www.yesodweb.com/book) (O'Reilly),
* [screencasts](https://www.yesodweb.com/page/screencasts),
* and [cookbook](https://github.com/yesodweb/yesod-cookbook).

If that is not enough you can ask the [community](https://www.yesodweb.com/page/community).

#### "Hello World"

From the following example, you can see that Yesod uses a lot of *Template Haskell* which makes it little bit hard to get at the beginning. 

```haskell
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
``` 

### Scotty

[Scotty](https://github.com/scotty-web/scotty) is another Haskell web framework inspired by Ruby's [Sinatra](http://sinatrarb.com), using [WAI](https://hackage.haskell.org/package/wai) and [Warp](https://hackage.haskell.org/package/warp) (a fast, light-weight web server for WAI applications). You can write your own application just with WAI (Web Application Interface), but Scotty provides you better abstractions from low-level communication. Sadly there is not so much documentation about Scotty, everything is just on [GitHub](https://github.com/scotty-web/scotty). Scotty uses [Blaze HTML](https://hackage.haskell.org/package/blaze-html) for HTML "templates".

#### "Hello World"

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 3000 $ do
  get "/" $ do
    html "Hello World!"
```

Surprisingly easy, right?!

#### Blaze templates

#### Hastache templates

#### Persistence with Persistent

https://wiki.haskell.org/Web/Databases_and_Persistence

## Example app: Simple blog with Scotty

//TODO: GitHub link to application and interesting parts commented here

## Task assignment

The homework to complete a simple web app is in repository [MI-AFP/hw09](https://github.com/MI-AFP/hw09).

## Further reading

* [Haskell web frameworks](https://wiki.haskell.org/Web/Frameworks)
* [The JavaScript problem](https://wiki.haskell.org/The_JavaScript_Problem)
* [Reddit: Web development using Haskell](https://www.reddit.com/r/haskell/comments/2wfap0/web_development_using_haskell/)
* [Is Haskell a Good Choice for Web Applications?](http://jekor.com/article/is-haskell-a-good-choice-for-web-applications)