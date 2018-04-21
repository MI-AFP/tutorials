# Web application in Haskell

Haskell can be (of course) used for network communication and also for building various web applications. In this tutorial, we are going to look at basics of network communication in Haskell, some specialized libraries making it simpler, and then at web frameworks. 

## Network communication

On the way to web applications, it is good to know how you can work with network communication on lower levels than is some web framework.

### Sockets

The most low-level solutions for you are working directly with sockets via [Network.Socket](https://hackage.haskell.org/package/network/docs/Network-Socket.html) module. With that, you have full control over the communication. Essentially the entire C socket API is exposed through this module, so if you are familiar with sockets from C, then it will be easy for you in Haskell: `bind`, `listen`, `receive`, `send`, `getAddrInfo`, etc.

### Server-client demo with sockets

There is a demo with echo server and client in the [Network.Socket](https://hackage.haskell.org/package/network/docs/Network-Socket.html) documentation. We will show a bit simpler example without forking.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketsDo $ do
    addr <- mkAddr "localhost" "3000"
    E.bracket (open addr) close loop
  where
    mkAddr host port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 10
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        msg <- recv conn 1024
        C.putStrLn $ msg
        unless (S.null msg) $ do
          sendAll conn (S.reverse msg)
        close conn
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketsDo $ do
    addr <- mkAddr "localhost" "3000"
    E.bracket (open addr) close talk
  where
    mkAddr host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk sock = do
        putStrLn "What do you want to send?"
        toSend <- getLine
        sendAll sock (C.pack toSend)
        msg <- recv sock 1024
        putStr "You received: "
        C.putStrLn msg
```

### Specialized libraries

Naturally, there are many specialized libraries that provide a simpler interface for network communication than are plain sockets when you want to work with some specific protocol (POP3, SMTP, SSH, or HTTP). Some are listed [here](https://wiki.haskell.org/Applications_and_libraries/Network) but you can find more on [Hackage](https://hackage.haskell.org).

The need of REST API client is something very common. For writing a simple one, you may use [wreq](https://hackage.haskell.org/package/wreq) package. It provides simple but powerful lens-based API, is capable of simple but powerful lens-based API, and supports often used techniques like OAuth, decompression, file upload, etc.

```haskell
-- GitHub API: list public repositories of user
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import Data.ByteString.Char8 hiding (putStrLn, getLine)
import qualified Data.Text as T (unpack)
import Data.Foldable
import Network.Wreq


mkURI :: String -> String
mkURI username = "https://api.github.com/users/" ++ username ++ "/repos"

getRepos :: String -> IO [String]
getRepos username = do
  r <- get (mkURI username)
  return . fmap T.unpack $ r ^.. responseBody . values . key "full_name" . _String

main :: IO ()
main = do
  putStrLn "Enter GitHub username:"
  username <- getLine
  repos <- getRepos username
  putStrLn $ "## First 25 public repos: "
  traverse_ putStrLn repos
```

## Web Frameworks overview

As with other languages, you usually don't want to build a web application from scratch which would bind ports, listen and parse requests and compose responses. For better abstraction, you might want to use a web framework.

There are several frameworks in Haskell (see [here](https://wiki.haskell.org/Web/Frameworks)) and here is our list of well-known:

- [Happstack](http://happstack.com)
- [Scotty](https://github.com/scotty-web/scotty)
- [Servant](https://haskell-servant.github.io)
- [Snap](http://snapframework.com)
- [Spock](https://www.spock.li)
- [Yesod](https://www.yesodweb.com)

We are going to show briefly Snap and Yesod because they are used quite often and then we will show more with our favorite Scotty. Next time, we will look at different one to build quickly REST API for our front-end app(s).

### Snap

[Snap](http://snapframework.com) is a simple web development framework for UNIX systems, written in the Haskell programming language. It consists of:

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

Snap also has a very nice philosophy in form of an optional system for building reusable pieces web functionality called “snaplets”. Snaplets make it easy to share and reuse common code across multiple web apps. The default snaplets let you get a full-featured web application up and running in no time.

If you want to build such application read [this](http://snapframework.com/docs/tutorials/snaplets-tutorial).

### Yesod

[Yesod](https://www.yesodweb.com) is a Haskell web framework for productive development of type-safe, RESTful, high performance web applications. It builds on Haskell features such as compile-time errors (instead of runtime), seamlessly asynchronous computation, scalability, good performance and light-weight syntax.

Another advantage of Yesod is comprehensive documentation including:

* [quick start guide](https://www.yesodweb.com/page/quickstart),
* [book](https://www.yesodweb.com/book) (O'Reilly),
* [screencasts](https://www.yesodweb.com/page/screencasts),
* and [cookbook](https://github.com/yesodweb/yesod-cookbook).

If that is not enough you can ask the [community](https://www.yesodweb.com/page/community).

#### "Hello World"

From the following example, you can see that Yesod uses a lot of *Template Haskell* which makes it a little bit hard to get at the beginning. 

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

As you can see from the last line, Yesod uses web server [warp](https://hackage.haskell.org/package/warp).

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

One of the well-known and widely used solution for HTML templates is [Blaze HTML](https://hackage.haskell.org/package/blaze-html). It is a blazingly fast HTML combinator library for the Haskell programming language. A huge advantage of Blaze is that you write HTML via HTML-like lightweight DSL in Haskell with the great type system. Blaze and Haskell won't allow you to do non-sense HTML.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text as T
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

type User = String

userInfo :: Maybe User -> Html
userInfo u = H.div ! A.id "user-info" $ case u of
    Nothing ->
        a ! href "/login" $ "Please login."
    Just user -> do
        "Logged in as "
        toHtml $ T.pack user

somePage :: Maybe User -> Html
somePage u = html $ do
    H.head $ do
        H.title "Some page."
    H.body $ do
        userInfo u
        "The rest of the page."

main :: IO ()
main = putStr . renderHtml $ somePage (Just "Marek")
```

Interesting tool, that you might find useful, is [blaze-from-html](https://hackage.haskell.org/package/blaze-from-html).

You might ask "What about styles?" or "What if want to have some JavaScript there?". For styles, there is [clay](https://hackage.haskell.org/package/clay) - a CSS preprocessor like LESS and Sass, but implemented as an embedded domain specific language (EDSL) in Haskell. Similarly to Blaze, you write CSS but in Haskell. For JavaScript, stay tuned for next tutorial.

#### Hastache templates

If you are already familiar with some web development, you've probably heard about [{{ mustache }}](http://mustache.github.io) templates. In Haskell, we have Haskell implementation of Mustache templates called [hastache](https://hackage.haskell.org/package/hastache).

```haskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.Text.Lazy.IO as TL 

main = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)
    >>= TL.putStrLn

template = "Hello, {{#reverse}}world{{/reverse}}! We know you, {{name}}!" 

context "reverse" = MuLambda (reverse . decodeStr)
context "name" = MuVariable "Haskell"
```

A useful source of information what can you do in this template are [examples](https://github.com/lymar/hastache/tree/master/examples).

#### Databases

First, you can work with database with low-level approach where you have own *CREATE* script and then other SQL (or other) parametric scripts in the code. For that, you can usually use module `Database.X` where `X` is type of datase:

- [Database.SQLite](http://hackage.haskell.org/package/sqlite-simple)
- [Database.MySQL](http://hackage.haskell.org/package/mysql)
- [Database.PostgreSQL](http://hackage.haskell.org/package/PostgreSQL)
- [Database.MongoDB](http://hackage.haskell.org/package/mongoDB)
- [Database.Redis](http://hackage.haskell.org/package/redis)
- etc.

For higher level, you can then use [Haskell Database Connectivity (HDBC)](http://hackage.haskell.org/package/HDBC) for SQL databases. A good introduction to HDBC is in [Chapter 21 - Using Databases](http://book.realworldhaskell.org/read/using-databases.html) of the [Real World Haskell](http://book.realworldhaskell.org/) book.

```sql
CREATE TABLE test(id INTEGER PRIMARY KEY, str TEXT);\
INSERT INTO test(str) VALUES ('test string 1');
INSERT INTO test(str) VALUES ('test string 2');
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)"
    (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
```

#### Persistence with Persistent

Again, there are several prepared libraries for working with persistence (DB) - take a look [here](https://wiki.haskell.org/Web/Databases_and_Persistence) or search the [Hackage](https://hackage.haskell.org). One of the most used is [persistent](https://hackage.haskell.org/package/persistent) also with various [extensions](https://hackage.haskell.org/packages/search?terms=persistent). There is nice documentation of this package in Yesod [book](https://www.yesodweb.com/book/persistent), but you can use it with any framework or even without any framework - just whenever you need to persist some data in a database.

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
      runMigration migrateAll :: IO ()

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]
```

Persistent uses *Template Haskell* for a declaration of a persistent model.

## WAI and testing web apps

There must be some interface between a web application and the web server where the application is running. You may have heard about something like [CGI](https://en.wikipedia.org/wiki/Common_Gateway_Interface), [FastCGI](https://en.wikipedia.org/wiki/FastCGI), [WSGI](https://cs.wikipedia.org/wiki/Web_Server_Gateway_Interface), or similar. As is [WSGI](https://cs.wikipedia.org/wiki/Web_Server_Gateway_Interface) for Python web applications, we have [Web Application Interface (WAI)](https://www.stackage.org/package/wai) in Haskell.

### Web app with plain WAI

It is possible to write a simple web application with just WAI and without any additional web framework.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
```

### HSpec & WAI

Web applications in Haskell can be tested via WAI. All applications that conform with WAI can be tested in the same way like a black box - send a request and check the response. In our favorite [Hspec](https://hspec.github.io), there is an extension [hspec-wai](https://github.com/hspec/hspec-wai) that allows you to test web applications in very easy and readable way as we are used to with hspec.

```haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Network.Wai (Application)
import qualified Web.Scotty as S
import           Data.Aeson (Value(..), object, (.=))

main :: IO ()
main = hspec spec

app :: IO Application
app = S.scottyApp $ do
  S.get "/" $ do
    S.text "hello"

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with 'hello'" $ do
      get "/" `shouldRespondWith` "hello"

    it "responds with 200 / 'hello'" $ do
      get "/" `shouldRespondWith` "hello" {matchStatus = 200}

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /some-json" $ do
    it "responds with some JSON" $ do
      get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]
```

This is just a simple (but often sufficient) example. Of course, you can test much more:

- https://begriffs.com/posts/2014-10-19-warp-server-controller-test.html
- https://www.spock.li/tutorials/testing

## Example app: a simple blog with Scotty

Now let's put it all together and look at the development of simple blog app with a database, authentication, and simple templates. 

* [MI-AFP/scotty-blog](https://github.com/MI-AFP/scotty-blog)

Next time, we will deal a bit with frontend technologies for Haskell, functional reactive programming and [The JavaScript problem](https://wiki.haskell.org/The_JavaScript_Problem). So you will also see how to develop server-side and client-side separately and connect them thru some (REST) API.

## Task assignment

The homework to complete a simple web app is in repository [MI-AFP/hw09](https://github.com/MI-AFP/hw09).

## Further reading

* [YesodBook - Persistent](https://www.yesodweb.com/book/persistent)
* [adit.io - Making A Website With Haskell](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html)
* [24 Days of Hackage: blaze-html](https://ocharles.org.uk/blog/posts/2012-12-22-24-days-of-hackage-blaze.html)
* [Haskell web frameworks](https://wiki.haskell.org/Web/Frameworks)
* [Reddit: What Haskell web framework do you use and why? ](https://www.reddit.com/r/haskell/comments/332s1k/what_haskell_web_framework_do_you_use_and_why/)
* [Reddit: Web development using Haskell](https://www.reddit.com/r/haskell/comments/2wfap0/web_development_using_haskell/)
* [Is Haskell a Good Choice for Web Applications?](http://jekor.com/article/is-haskell-a-good-choice-for-web-applications)
