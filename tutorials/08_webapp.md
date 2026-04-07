# Web application in Haskell

Haskell can be (of course) used for network communication and also for building various web applications. In this tutorial, we are going to look at basics of network communication in Haskell, some specialized libraries making it simpler, and then at web frameworks.

## Network communication

On the way to web applications, it is good to know how you can work with network communication on lower levels than is some web framework.

### Sockets

The most low-level solutions work directly with sockets via [Network.Socket](https://hackage.haskell.org/package/network/docs/Network-Socket.html) module. With that, you have a full control over the communication. Essentially the entire C socket API is exposed through this module, so if you are familiar with sockets from C, then it will be easy for you in Haskell: `bind`, `listen`, `receive`, `send`, `getAddrInfo`, etc.

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

A you can see, most of the work happens in `do` of `IO` and it highly resembles classic imperative programming. Indeed, networking and also most of UI is inherently not a show room of beautiful Haskell code, but it gets the job done ;-).

### Specialized libraries

Naturally, there are many specialized libraries that provide a higher-level interface for network communication than plain sockets when you want to work with some specific protocol (POP3, SMTP, SSH, or HTTP). Some are listed [here](https://wiki.haskell.org/Applications_and_libraries/Network) but you can find more on [Hackage](https://hackage.haskell.org).

The need of REST API client is something very common. For writing a simple one, you may use [wreq](https://hackage.haskell.org/package/wreq) package. It provides simple but powerful lens-based API and it supports often-used techniques like OAuth, decompression, file upload, etc.

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

As with other languages, you usually don't want to build a web application from the scratch which would bind ports, listen and parse requests and compose responses. For a higher abstraction, you might want to use a web framework.

There are several frameworks in Haskell (see [here](https://wiki.haskell.org/Web/Frameworks)) and here is our list of the most used ones:

- [Happstack](http://happstack.com)
- [Scotty](https://github.com/scotty-web/scotty)
- [Servant](https://haskell-servant.github.io)
- [Snap](http://snapframework.com)
- [Spock](https://www.spock.li)
- [Yesod](https://www.yesodweb.com)
- [IHP](https://ihp.digitallyinduced.com)

As you can see, there is quite an above-average offer of them. They mostly differ at the level of abstraction and scope: Scotty being relatively low-abstraction routing and middleware and Yesod being a complete solution including templating and persistence, everything on a high abstraction level. The choice depends on your preference and needs. As always, a higher abstraction means the system does a lot of job for yourself, you write fewer code, which means a higher effectiveness and less bugs. On the other hand, you may face a [leaky abstraction problem](https://blog.codinghorror.com/all-abstractions-are-failed-abstractions/) at some point.

We are going to show briefly Snap and Yesod, because they are used often and then we will go deeper with our favourite versatile Scotty. Next time, we will look at a different one to build quickly a REST API for our front-end app(s).

However, before we dive into the topic, there is one more note, which may excite you. In the first lecture, we explained what a referential transparency is and that it brings certain qualities to code -- ability to reason about, reusability, testability, parallelism "for free". In the case of web frameworks, you can experience reusability coming from reference transparency very clearly: web frameworks are typically built of certain independent, shared components like a web server ([warp](https://hackage.haskell.org/package/warp)) or a web application interface ([wai](https://hackage.haskell.org/package/wai)). These highly-specialised components are developed independently, tested independently and as such, the whole ecosystem exercises an unparalleled separation of concerns and thanks to it, it is easier evolvable, reliable and lively. Moreover, you can use the low-level components independently and integrate them easily in another framework or even an non-web application -- you can e.g. use [templating from Yesod](https://hackage.haskell.org/package/shakespeare) in Scotty, if you like, or its [persistence layer](https://hackage.haskell.org/package/persistent) in a simple CLI application.

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

Snap also has a very nice philosophy in form of an optional system for building reusable pieces web functionality called “snaplets”. Snaplets make it easy to share and reuse common code across multiple web apps. The default snaplets let you get a full-featured web application up and running very fast.

If you want to build such application, read [this](http://snapframework.com/docs/tutorials/snaplets-tutorial).

### Yesod

[Yesod](https://www.yesodweb.com) is a Haskell web framework for productive development of type-safe, RESTful, high performance web applications. It builds on Haskell features such as compile-time errors (instead of runtime), seamlessly asynchronous computation, scalability, good performance and light-weight syntax.

Another advantage of Yesod is comprehensive documentation including:

* [quick start guide](https://www.yesodweb.com/page/quickstart),
* [book](https://www.yesodweb.com/book) (O'Reilly),
* [screencasts](https://www.yesodweb.com/page/screencasts),
* and [cookbook](https://github.com/yesodweb/yesod-cookbook).

If that is not enough you can ask the [community](https://www.yesodweb.com/page/community).

Yesod is a "Mercedes" of Haskell web frameworks. It means a lot of comfort is prepared for you to enjoy, however it also means that people needing an agile small car for Italian crooked streets may experience troubles ;-).

#### "Hello World"

From the following example, you can see that Yesod uses a lot of *Template Haskell* which makes it a little bit hard to get at the beginning, but it enchants you with a lot of pleasant magic. And yes, every magic has some costs ;-) (see the previous lecture).

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

### IHP (Integrated Haskell Platform)

[IHP](https://ihp.digitallyinduced.com/) is a batteries-included web framework for Haskell. It is designed to be easy to use and to provide a lot of functionality out of the box, including:

* A powerful type system that helps catch errors at compile time
* A built-in ORM (Object-Relational Mapping) for working with databases
* A templating engine for generating HTML
* A development server for testing and debugging

IHP is a good choice for developers who want to build web applications quickly and easily, without having to worry about the underlying details of web development. It is also a good choice for developers who want to take advantage of Haskell's powerful type system and functional programming features.

The architecture of IHP is based on the Model-View-Controller (MVC) pattern, which helps to keep the code organized and maintainable. The framework also provides a lot of built-in functionality for common web development tasks, such as authentication, authorization, and database migrations.

### Scotty

[Scotty](https://github.com/scotty-web/scotty) is another Haskell web framework inspired by Ruby's [Sinatra](http://sinatrarb.com), using [WAI](https://hackage.haskell.org/package/wai) and [Warp](https://hackage.haskell.org/package/warp) (a fast, light-weight web server for WAI applications). You can write your own application just with WAI (Web Application Interface), but Scotty provides you with abstractions from a low-level communication. Sadly, there is not so much documentation about Scotty, everything is just on [GitHub](https://github.com/scotty-web/scotty). Scotty uses primarily [Blaze HTML](https://hackage.haskell.org/package/blaze-html) for HTML "templates", however, as we explained, you may also integrate it with any templating library you like.

#### "Hello World"

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 3000 $ do
  get "/" $ do
    html "Hello World!"
```

Surprisingly easy, right?!

### Templating

Writing HTML fragments as strings and compose them together can be fairly bothersome. For that purpose, there are libraries for templating known from other languages and frameworks as well.

#### Blaze templates

One of the well-known and widely used solution for HTML templates is [Blaze HTML](https://hackage.haskell.org/package/blaze-html). It is "a blazingly fast HTML combinator library for the Haskell programming language". A huge advantage of Blaze is that you write HTML via HTML-like lightweight DSL in Haskell with the great type system. Blaze and Haskell won't allow you to make a non-sense HTML, although it does not check a full conformity, of course.

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

An interesting tool, that you might find useful, is [blaze-from-html](https://hackage.haskell.org/package/blaze-from-html).

You might ask "What about styles?" or "What if want to have some JavaScript there?". For styles, there is [clay](https://hackage.haskell.org/package/clay) - a CSS preprocessor like LESS and Sass, but implemented as an embedded domain specific language (EDSL) in Haskell. Similarly to Blaze, you write CSS in Haskell. For JavaScript, stay tuned for the next tutorial ;-).

#### Hastache templates

If you are already familiar with some web development, you've probably heard about the popular [{{ mustache }}](http://mustache.github.io) templates. In Haskell, we have Haskell implementation of Mustache templates called [hastache](https://hackage.haskell.org/package/hastache).

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

A useful source of information what can you do with Hastache are [examples](https://github.com/lymar/hastache/tree/master/examples).

#### Ginger

If you are familiar with Python and more specifically with Jinja2 templates, then you might be a fan of [Ginger](https://ginger.tobiasdammers.nl/guide/) library. It is implementation of Jinja2 templates in Haskell and it have quite nice documentation. Usage is quite simple, just look at the example:

You have some HTML file with Jinja2 tags:

```html
{# There could be some "extends" for layout #}
<!DOCTYPE html>
<html>
    <head>
        <title>{{ title }}</title>
    </head>
    <body>
        <h1>Hello, {{ name }}!</h1>

        {% if condition %}
            <p>Condition is true</p>
        {% else %}
            <p>Condition is false or undefined</p>
        {% endif %}
    </body>
</html>
```

Then you can load template, supply context into it, and render as text:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.HashMap.Strict (fromList, HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import System.IO.Error (tryIOError)
import Text.Ginger
       (makeContextHtml, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import Text.Ginger.Html (htmlSource)


-- A simple hashmap that we'll use as our template context
sampleContext :: HashMap Text Text
sampleContext = fromList [("name", "Alice")]


-- Given a Template and a HashMap of context, render the template to Text
render :: Template -> HashMap VarName Text -> Text
render template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
  in htmlSource $ runGinger context template


-- Wrapper around HashMap.lookup that applies toGVal to the value found.
-- Any value referenced in a template, returned from within a template, or used
-- in a template context, will be a GVal
scopeLookup
  :: (Hashable k, Eq k, ToGVal m b)
  => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context


loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fn =
  tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing

  where
    loadFile :: FilePath -> IO String
    loadFile fn' = openFile fn' ReadMode >>= hGetContents

main :: IO ()
main = do
  template <- parseGingerFile loadFileMay "base.html"
  case template of
    Left err -> print err >> exitFailure
    Right template' -> print $ render template' sampleContext
```

### Databases

Again, several abstraction levels are available. First, you can employ a low-level approach where you incorporate SQL in the code. For that, you can usually use module `Database.X` where `X` is type of datase:

- [Database.SQLite](http://hackage.haskell.org/package/sqlite-simple)
- [Database.MySQL](http://hackage.haskell.org/package/mysql)
- [Database.PostgreSQL](http://hackage.haskell.org/package/PostgreSQL)
- [Database.MongoDB](http://hackage.haskell.org/package/mongoDB)
- [Database.Redis](http://hackage.haskell.org/package/redis)
- etc.

A slightly better services are provided by mid-level libraries:

- [Database.MySQL.Simple](https://hackage.haskell.org/package/mysql-simple)
- [Database.PostgreSQL.Simple](https://hackage.haskell.org/package/postgresql-simple)

Going higher with the abstraction, you can then use [Haskell Database Connectivity (HDBC)](http://hackage.haskell.org/package/HDBC) for SQL databases. A good introduction to HDBC is in [Chapter 21 - Using Databases](http://book.realworldhaskell.org/read/using-databases.html) of the [Real World Haskell](http://book.realworldhaskell.org/) book.

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

Now skyrocketing the abstraction to the heights of Template Haskell, we get to [persistent](https://hackage.haskell.org/package/persistent) that comes also with various [extensions](https://hackage.haskell.org/packages/search?terms=persistent). There is a nice documentation of this package in the Yesod [book](https://www.yesodweb.com/book/persistent), but, as we already explained, you can use it with any framework or even without any framework — just whenever you need to persist some data in a database.

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

As you noticed, Persistent uses *Template Haskell* for a declaration of a persistent model.

For other possibilities of persistence libraries, take a look [here](https://wiki.haskell.org/Web/Databases_and_Persistence) or search the [Hackage](https://hackage.haskell.org).

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

Web applications in Haskell can be tested via WAI. All applications that conform with WAI can be tested in the same way like a black box - send a request and check the response. In our favourite [Hspec](https://hspec.github.io), there is an extension [hspec-wai](https://github.com/hspec/hspec-wai) that allows you to test web applications in a very easy and readable way, as we are used to with hspec.

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

## Example apps:

Here are a few examples of simple and more complex web apps:

* [dbushenko/scotty-blog](https://github.com/dbushenko/scotty-blog)
* [ds-wizard/legacy-wizard](https://github.com/ds-wizard/legacy-wizard)
* [ds-wizard/engine-backend](https://github.com/ds-wizard/engine-backend)


## Case study: a simple Servant web application

To connect the concepts from this tutorial with practical usage, we will briefly outline the architecture of a simple web application based on Servant, which you will use in the assignment.

The goal is not to explain every detail, but to show how the pieces fit together.

### Why Servant?

Servant is a web framework that allows you to define your API at the type level.

This has several advantages:

* the API serves as a single source of truth,
* server and client can be derived from the same definition,
* many errors are caught at compile time.

Servant is especially well-suited for building REST APIs with JSON.

### Application architecture

In the assignment, the application follows a simple layered structure:

* **API** (presentation) layer – defines HTTP endpoints (routes, request/response types)
* **Service** (business logic) layer – contains business logic
* **Database** (persistence / data) layer – handles persistence (SQLite)
* Model / DTOs – represent internal data and external API formats

This separation helps keep the code:

* modular,
* testable,
* easier to understand.

We intentionally keep the architecture (and naming) close to what you may know from other languages and frameworks to show how Haskell can be used in a familiar way, while still benefiting from its unique features.

### Application monad (context)

The application uses a custom monad stack (using Monad transformers) to manage the application context, which includes:

```haskell
newtype AppContextM a = AppContextM
    { runAppContextM :: ReaderT AppContext (LoggingT (ExceptT ServerError IO)) a
    }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadError ServerError, MonadLogger)
```

This stack combines several cross-cutting concerns:

* `ReaderT AppContext` = shared environment (configuration, database connection, etc.)
* `LoggingT` = structured logging
* `ExceptT ServerError IO` = error handling compatible with Servant

You have already seen these building blocks — here they are combined into a practical application context.

### Example domain: TODO item

The application typically works with a simple entity such as a TODO item.

You will encounter:

* ``Model`` = internal representation used in business logic
* ``DTOs`` (Data Transfer Objects) = types used for JSON input/output
* ``Conversion functions`` = mapping between internal and external representations

### Model and Database

The model describes how data are stored in the database. We can define models using **Persistent** library, which provides a nice DSL for defining database entities and their fields.

```haskell
--- ... Database/Model.hs

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
TODOItem
  title String
  description String
  isDone Bool
  deriving Show
|]
```

This generates the `TODOItem` type together with database keys and migration support.

Similarly to Java frameworks we can create a `Repository` or `DAO` to abstract database operations, but in this simple application, we will directly use Persistent functions in the service layer.

```haskell
--- ... Database/TODOItemDAO.hs

getById :: String -> AppContextM (Maybe TODOItem)
getById todoId = do
    result <- runDB $ selectList [TODOItemUuid ==. todoId] []
    case result of
        [] -> return Nothing
        ((Entity _ todoItem) : _) -> return (Just todoItem)

create :: TODOItem -> AppContextM TODOItemId
create newTODOItem = runDB $ insert newTODOItem

getAll :: AppContextM [TODOItem]
getAll = do
    result <- runDB $ selectList [] []
    return $ extractTODOItem <$> result
  where
    extractTODOItem (Entity _ todoItem) = todoItem
```

Naturally, this should not contain any business logic, just database access.

### Service

The service layer contains the application logic and composes DAO + mapper functions:

```haskell
-- ... Service/TODOItemService.hs

getAllTODOItems :: AppContextM [TODOItemDTO]
getAllTODOItems = do
    todoItems <- getAll
    return $ toDTO <$> todoItems

createTODOItem :: TODOItemCreateDTO -> AppContextM (Maybe TODOItemDTO)
createTODOItem createDto = do
    newUuid <- liftIO $ toString <$> nextRandom
    let newTODOItem = fromCreateDTO newUuid createDto
    _ <- create newTODOItem
    mTODOItem <- getById newUuid
    return $ toDTO <$> mTODOItem

getTODOItem :: String -> AppContextM (Maybe TODOItemDTO)
getTODOItem todoId = do
    mTODOItem <- getById todoId
    return $ toDTO <$> mTODOItem
```

Compared to the DAO layer, the service layer works with DTOs and application use-cases rather than raw database operations.

### API

Finally, the API layer defines the HTTP endpoints and how they map to service functions. First on the type level:

```haskell
-- ... API/TODOItemAPI.hs

type List_GET
    = "todo" :> QueryParam "q" String :> Get '[JSON] [TODOItemDTO]

type List_POST
    = ReqBody '[JSON] TODOItemCreateDTO
   :> "todo"
   :> Verb 'POST 201 '[JSON] TODOItemDTO

type Detail_GET
    = "todo" :> Capture "todoId" String :> Get '[JSON] TODOItemDTO

type TODOItemAPI
    = List_GET
 :<|> List_POST
 :<|> Detail_GET
```

Then, we implement the server by mapping API endpoints to service functions:

```haskell
-- ... API/TODOItemAPI.hs

list_GET :: Maybe String -> AppContextM [TODOItemDTO]
list_GET _query = getAllTODOItems

list_POST :: TODOItemCreateDTO -> AppContextM TODOItemDTO
list_POST reqDto = do
    mTODOItemDTO <- createTODOItem reqDto
    returnOr404 mTODOItemDTO

detail_GET :: String -> AppContextM TODOItemDTO
detail_GET todoId = do
    mTODOItemDTO <- getTODOItem todoId
    returnOr404 mTODOItemDTO
```

Finally, we can define the complete server by combining all endpoints:

```haskell
todoServer :: ServerT TODOItemAPI AppContextM
todoServer = list_GET :<|> list_POST :<|> detail_GET
```

### Application entry point

The application entry point initializes the application context, runs database migrations, and starts the server:

```haskell
main :: IO ()
main = do
    -- Initialize application context (e.g., database connection)
    appContext <- initializeAppContext
    -- Run database migrations
    runSqlite (appDbPath appContext) $ runMigration migrateAll
    -- Start the server
    let apiProxy = Proxy :: Proxy TODOItemAPI
    run 3000 $ serve apiProxy (hoistServer apiProxy (convertAppContext appContext) todoServer)
```

Functions like `initializeAppContext` and `convertAppContext` are responsible for setting up the application context and converting it to the form expected by Servant.

### Conclusion

This case study shows how to structure a simple web application in Haskell using Servant, Persistent, and a custom application monad. The same principles can be applied to other frameworks and libraries as well. The key takeaway is that Haskell's strong type system and powerful abstractions allow us to build web applications that are modular, testable, and maintainable.

## Task assignment

The homework to complete a simple web app is in repository [MI-AFP/hw08](https://github.com/MI-AFP/hw08).

## Further reading

* [YesodBook: Persistent](https://www.yesodweb.com/book/persistent)
* [adit.io: Making A Website With Haskell](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html)
* [24 Days of Hackage: blaze-html](https://ocharles.org.uk/blog/posts/2012-12-22-24-days-of-hackage-blaze.html)
* [Haskell web frameworks](https://wiki.haskell.org/Web/Frameworks)
* [Reddit: What Haskell web framework do you use and why? ](https://www.reddit.com/r/haskell/comments/332s1k/what_haskell_web_framework_do_you_use_and_why/)
* [Reddit: Web development using Haskell](https://www.reddit.com/r/haskell/comments/2wfap0/web_development_using_haskell/)
* [Is Haskell a Good Choice for Web Applications?](http://jekor.com/article/is-haskell-a-good-choice-for-web-applications)
