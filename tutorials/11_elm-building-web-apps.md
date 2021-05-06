# Elm - Building Web Applications

## Webpack

[Webpack](https://webpack.js.org) is a module bundler for JavaScript applications. It builds a dependency graph of source modules and creates static assets. It uses different loaders for different file types, e.g., to convert new EcmaScript syntax into the one supported by browsers or to convert Sass to CSS. It can also minify those assets.

Besides other loaders, there is also [elm-webpack-laoder](https://github.com/elm-community/elm-webpack-loader). If we require an Elm module from JavaScript code, it will use `elm make` under the hood to build it.

Here's an example of configuration:

```js
module.exports = {
  module: {
    rules: [{
      test: /\.elm$/,  // use the loader only for elm files
      exclude: [/elm-stuff/, /node_modules/],  // exclude project dependencies
      use: {
        loader: 'elm-webpack-loader',
        options: {}
      }
    }]
  }
};
```

## Initializing Elm App

Till now, we used `elm reactor` to run our Elm applications. However, we can't use that in a production environment. If we try to build the Todo list form [elm-examples](https://github.com/MI-AFP/elm-examples) using `elm make`, we can look into the generated JavaScript, how the app is initialized. At the very end of the document, there is the following line of code:

```js
const app = Elm.Todos.init({ node: document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49") });
```

Todos is the name of the module (it was in `src/Todos.elm`). The `init` function is called to initialize the app. We used [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element), therefore there is `node` in init arguments.

When we are using webpack, we can do something very similar ourselves. We need a JavaScript entrypoint, e.g., `index.js`. We can require an Elm module from there, and the elm-webpack-loader will take care of compiling Elm into JavaScript. Then, we can call the init function.

```js
// index.js
const program = require('src/Todos.elm');

const app = program.Elm.Todos.init({
    node: document.getElementById('node-id')
});
```

We also need to create the element in our HTML structure. In case we would use [Browswer.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document) or [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application) instead, we don't need to specify the node since it will load into the whole document body.


## JavaScript Interop

In real-world applications, we usually need at least some interoperability with JavaScript. There are two options in Elm -- using flags or ports.

### Flags

Flags are used to pass some values when initializing the Elm app. For example, we can send a random initial seed generated using JavaScript. The object we pass to the `init` function can have another property called `flags`.

```js
// index.js
const program = require('src/Todos.elm');

const app = program.Elm.Todos.init({
    node: document.getElementById('node-id'),
    flags: Math.random()
});
```

Then, on the Elm side, we receive these flags as the first argument of `init` function.

```elm
init : Float -> (Model, Cmd Msg)
init randomNumber =
    ...
```

The flags can be one of the following types:

- Basic types (`Bool`, `Int`, `Float`, `String`)
- `Maybe`
- Lists (`List`, `Array`)
- Tuples
- Records
- `Json.Decode.Value`

If we use anything else than `Json.Decode.Value` and provide an incorrect type, we get an error on the JavaScript side. Therefore it is safer to use `Json.Decode.Value`, define a decoder and handle possible errors on the Elm side when the decoding fails.

### Ports

While Flags are used for sending initial values to the Elm app, Ports are used for sending messages between JavaScript and Elm. The communication is not based on request/response like in HTTP though. We only have one-way messages send from Elm to JavaScript or vice versa.

#### Outgoing Messages

Sending messages from Elm to JavaScript is realized through commands. We need to define the `port` using a `port module`.

A common example is using localStorage, so for example, here we want to save user data into localStorage.

```elm
port module Ports exposing (saveUser)

import Json.Encode as Encode

port saveUser : Encode.Value -> Cmd msg
```

The port declaration is similar to a function. However, it starts with a keyword `port` and has no function body. The module where we define ports must be a `port module`.

Once we define the port for outgoing messages, we should subscribe to it on the JavaScript side after the app initialization.

```js
const program = require('src/Todos.elm');

const app = program.Elm.Todos.init({
    node: document.getElementById('node-id')
});

app.ports.saveUser.subscribe(function(data) {
    localStorage.setItem('user', JSON.stringify(data));
});
```

And then we can create a command using the port in our update function.

```elm
import Ports


update msg model =
    case msg of
        SaveUser user ->
            (model, Ports.saveUser <| encdeUser user)
        ...
```

#### Incoming messages

Sending messages from JavaScript to Elm is realized through Subscriptions. We need to define the port but this time slightly different again.

For example, we want to send user data back to the Elm app.

```elm
port module Ports exposing (gotUser)

import Json.Encode as Encode

port gotUser : (Encode.Value -> msg) -> Sub msg
```

Then in our subscriptions function, we need to subscribe to use the port with proper message constructor.

```elm
import Json.Encode as Encode
import Ports


type alias Model =
    ()

type Msg
    = GotUser Encode.Value

subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.gotUser GotUser

```

On JavaScript side, we can send a message to the Elm app using the port.

```js
const program = require('src/Todos.elm');

const app = program.Elm.Todos.init({
    node: document.getElementById('node-id')
});

const userData = {}

app.ports.gotUser.send(userData);
```

## Single Page Applications

When we are building real applications, we usually need more than one screen. We also want to have different URLs for different screens, so it is, for example, easier to send links to the application. We need to handle navigation and URL parsing.

### Navigation

We use [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application) which avoids loading new HTML when URL changes. It has more complex annotation:

```elm
application :
  { init : flags -> Url -> Key -> ( model, Cmd msg )
  , view : model -> Document msg
  , update : msg -> model -> ( model, Cmd msg )
  , subscriptions : model -> Sub msg
  , onUrlRequest : UrlRequest -> msg
  , onUrlChange : Url -> msg
  }
  -> Program flags model msg
```

The `init` function now gets not only flags but also initial `Url` and navigation `Key` that is needed for changing URL using navigation commands.

When a link is clicked within the application, it is intercepted as `UrlRequest`, the message is created using `onUrlRequest` and sent to the update function.

When the URL is changed, the message is created using `onUrlChange` and sent to the update function.

Here is a simple example from [An Introduction to Elm](https://guide.elm-lang.org/webapps/navigation.html).

```elm
import Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as  Attributes
import Url



-- MAIN


main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key : Navigation.Key
  , url : Url.Url
  }


type alias Flags = 
  ()


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
  ( { key = key
    , url = url
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model
          , Nav.pushUrl model.key <| Url.toString url
          )

        Browser.External href ->
          ( model
          , Navigation.load href
          )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
  always Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ Html.text "The current URL is: "
      , Html.b [] [ Html.text <| Url.toString model.url ]
      , Html.ul []
          [ viewLink "/home"
          , viewLink "/profile"
          , viewLink "/reviews/the-century-of-the-self"
          , viewLink "/reviews/public-opinion"
          , viewLink "/reviews/shah-of-shahs"
          ]
      ]
  }


viewLink : String -> Html msg
viewLink path =
  Html.li [] [ Html.a [ Attributes.href path ] [ Html.text path ] ]
```


### URL Parsing

In the previous example, we used Url as is. However, we can use [Url.Parser](https://package.elm-lang.org/packages/elm/url/latest/Url-Parser) module from [elm/url](https://package.elm-lang.org/packages/elm/url/latest/) package to parse the URL into useful Elm types.


Here's an example form the documentation converting different routes with parameters into `Route` type.

```elm
import Url.Parser exposing ((</>), Parser, int, s, string)

type Route
  = Topic String
  | Blog Int
  | User String
  | Comment String Int

route : Parser (Route -> a) a
route =
  oneOf
    [ map Topic   (s "topic" </> string)
    , map Blog    (s "blog" </> int)
    , map User    (s "user" </> string)
    , map Comment (s "user" </> string </> s "comment" </> int)
    ]

-- /topic/wolf            ==>  Just (Topic "wolf")
-- /topic/                ==>  Nothing

-- /blog/42               ==>  Just (Blog 42)
-- /blog/wolf             ==>  Nothing

-- /user/sam/             ==>  Just (User "sam")
-- /user/bob/comment/42   ==>  Just (Comment "bob" 42)
-- /user/tom/comment/35   ==>  Just (Comment "tom" 35)
-- /user/                 ==>  Nothing
```

## Opaque type

Opaque types are types that hide their internal implementation details within a module. While this statement seems benign on its surface, it’s an incredibly important concept in an ecosystem that enforces semantic versioning.

*Note*: Taken from [Charlie Koster, medium.com](https://ckoster22.medium.com/advanced-types-in-elm-opaque-types-ec5ec3b84ed2)

```elm
module Email exposing (Email, decodeEmail, toString)

import Json.Decode as Decode

type Email
    = EmailInternal String

decodeEmail : Decode.Decoder Email
decodeEmail =
    Decode.andThen validateEmail Decode.string

validateEmail : String -> Decode.Decoder Email
validateEmail emailString =
    if isEmailValid emailString then
        Decode.succeed <| EmailInternal emailString

    else
        Decode.fail "Invalid email!"

toString : Email -> String
toString (EmailInternal email) =
    email

isEmailValid : String -> Bool

-- in Home page
import Email

emailView : Email.Email -> Html msg
emailView =
  Email.toString
    >> Html.text

emailView2 : String -> Html msg
emailView2 =
  Html.text
```

*Note*: From `Email` module, we expose only `Email` type without variant `EmailInternal`. The only way, how to access email value is in this module, no other module does not have access to `EmailInternal` and can use only access function `toString`.

## Materials

- [elm-spa](https://github.com/deny1994/elm-spa)
- [elm-webpack-boilerplate](https://github.com/MI-AFP/elm-webpack-boilerplate)
- [Examples - Web Application](https://github.com/MI-AFP/elm-examples/tree/master/webapp)

## Further Reading

- [Opaque types](https://ckoster22.medium.com/advanced-types-in-elm-opaque-types-ec5ec3b84ed2)
- [Make impossible states impossible](https://www.youtube.com/watch?v=IcgmSRJHu_8&ab_channel=elm-conf)
- [Elm Europe 2017 - Richard Feldman - Scaling Elm Apps](https://www.youtube.com/watch?v=DoA4Txr4GUs)
- [Richard Feldman real world SPA](https://github.com/rtfeldman/elm-spa-example)
- [create-elm-app](https://github.com/halfzebra/create-elm-app)
- [Webpack Concepts](https://webpack.js.org/concepts)
- [Web Apps · An Introduction to Elm](https://guide.elm-lang.org/webapps/)
- [elm-shared-state](https://github.com/ohanhi/elm-shared-state)
- [elm-awesome](https://github.com/sporto/awesome-elm)