# Elm - The Elm Architecture

## Elm program

Elm program is set up using [Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) module from [elm/browser](https://package.elm-lang.org/packages/elm/browser/latest/) package. There are several functions to do that based on the use case.

- `sandbox` - Program that interacts with the user input but cannot communicate with the outside world.
- `element` - HTML element controlled by Elm that can talk to the outside world (e.g. HTTP requests). Can be embedded into a JavaScript project.
- `document` - Controls the whole HTML document, view controls `<title>` and `<body>` elements.
- `application` - Creates single page application, Elm controls not only the whole document but also Url changes.


## Forms

Form elements are created the same way as other HTML elements using functions from [Html](https://package.elm-lang.org/packages/elm/html/latest/Html) module and attributes from [Html.Attributes](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes) module from [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) package.

We can use `onInput` from [Html.Events](https://package.elm-lang.org/packages/elm/html/latest/Html-Events) module to detect input events and create a message for our update function.

The loop is the following:
- user changes the value in an input field
- a new message is created
- the update function is called with the message and it updates the model with the new value
- input field is re-rendered with the new value


Here is a simple example with a single input field:

```elm
import Browser
import Html exposing (Html, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)

main = Browser.sandbox { init = init, update = update, view = view }

type Msg = NameChanged String

type alias Model = { name : String }

init : Model
init =
    { name = "" }

update : Msg -> Model -> Model
update msg model =
    case msg of
        NameChanged newName ->
            { model | name = newName }

view : Model -> Html Msg
view model =
    input
        [ placeholder "Your name"
        , value model.name
        , onInput NameChanged ]
        []
```

When we need more complex forms in our application, there are packags to handle forms like [etaque/elm-form](https://package.elm-lang.org/packages/etaque/elm-form/latest/).


## JSON

It is very common to use JSON format when communicating with different APIs. In JavaScript, JSON is usually turned into a JavaScript object and used within the application. However, this is not the case in Elm since we have a strong type system. Before we can use JSON data, we need to convert it into a type defined in Elm. There is the [elm/json](https://package.elm-lang.org/packages/elm/json/latest/) package for that.


### Decoders

Elm use decoders for that. It is a declarative way how to define what should be in the JSON and how to convert it into Elm types. Functions for that are defined in [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode) module.

For example, we have this JSON representing a TODO:

```json
{
    "id": 24,
    "label": "Finish the home",
    "completed": false
}
```

To get the `label` field, we can define a decoder like this:

```elm
import Json.Decode as D exposing (Decoder)

labelDecoder : Decoder String
labelDecoder =
    D.field "label" D.string
```

There are functions to decode other primitives, like `bool` or `int`. However, we usually need more than just one field. We can combine decoders using `map` functions from `Json.Decode` module, e.g. `map3`.

```elm
map3 :
    (a -> b -> c -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder value
```

We can then define our own type for TODO and a decoder.

```elm
import Json.Decode as D exposing (Decoder)

type alias Todo =
    { id : Int
    , label : String
    , completed : Bool
    }

todoDecoder : Decoder Todo
todoDecoder =
    D.map3 Todo
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "completed" D.bool)
```

There is a package [NoRedInk/elm-json-decode-pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest) for more convenient JSON decoder. It is especially useful for large and more complex objects. We could rewrite the previous example using pipeline:

```elm
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)

type alias Todo =
    { id : Int
    , label : String
    , completed : Bool
    }

todoDecoder : Decoder Todo
todoDecoder =
    D.succeed Todo
        |> required "id" D.int
        |> required "name" D.string
        |> required "completed" D.bool
```

It is not that big change in this case, however, we only have `map8` function in `Json.Decode` so this library comes handy if we need more. Moreover, it has other functions to define for example [optional](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/Json-Decode-Pipeline#optional) or [hardcoded](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/Json-Decode-Pipeline#hardcoded) values.


### Encoders

When we want to send something to an API we need to do the opposite -- turn the Elm value into JSON value. We use functions form [Json.Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode) package for that. There is a type called `Value` which represents a JavaScript value and functions to convert Elm primitives, lists and objects into `Value` type.

Here's an example using the TODO from decoders example.

```elm
import Json.Encode as E

type alias Todo =
    { id : Int
    , label : String
    , completed : Bool
    }

encodeTodo : Todo -> E.Value
encodeTodo todo =
    E.object
        [ ( "id", E.int todo.id )
        , ( "label", E.string todo.label )
        , ( "completed", E.bool todo.completed )
        ]
```

The object is representend as a list of key value tuples.


## Http

There is [Http](https://package.elm-lang.org/packages/elm/http/latest/Http) module in [elm/http](https://package.elm-lang.org/packages/elm/http/latest/) package for making HTTP requests in Elm. The functions creating requests create a command for Elm runtime which defines what request should be made, what is the expected response and what message should be send to update function when the request is done.

Here is an example for getting TODO using the decoder defined in previous section.

```elm
import Http

type Msg = GotTodo (Result Http.Error Todo)

getTodo : Cmd Msg
getTodo =
    Http.get
        { url = "http://example.com/todo"
        , expect = Http.expectJson GotTodo todoDecoder
        }
```

The function `getTodo` creates a command with HTTP request that expect JSON to be returned and uses `todoDecoder` to get `Todo` type form the returned JSON. Once the request is finished, we get `GotTodo` message containing the `Result` with either `Http.Error` if the request failed or `Todo` if the request was successful.

There are other functions we can use for expected response like `expectString` to get the string as is or `expectWhatever` when we don't really care about the response as long as it's ok.

When we want to do a POST request we also need to define the body. Here's an example of posting TODO to the server, using encoder function from previous section.

```elm
import Http

type Msg = TodoSaved (Result Http.Error ())

postTodo : Todo -> Cmd Msg
postTodo todo =
    Http.post
        { url = "http://example.com/todo"
        , body = Http.jsonBody <| encodeTodo todo
        , expect = Http.expectWhatever TodoSaved
        }
```

Of course, we can send different types of body, not just JSON, e.g., `stringBody` for plain string or `emptyBody` when we don't want to send anything.

When we want to do a different type of request than GET and POST or we want to set headers, we need to use `Http.request` function (`Http.post` and `Http.get` are actually just a shorthand for calling `Http.request`).

```elm
request :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
```


## Subscriptions

[Subscirptions](https://package.elm-lang.org/packages/elm/core/latest/Platform-Sub) are used to tell Elm that we want to be informed if something happend (e.g., web socket message or clock tick).


Here's an example of subscriptions defining that a message `Tick` with current time should be send to update function every 1000 milliseconds.


```elm
import Time


type Msg = Tick Time.Posix


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
```


## Random

There is a [Random](https://package.elm-lang.org/packages/elm/random/latest/Random) module in [elm/random](https://package.elm-lang.org/packages/elm/random/latest/) package for generating pseudo-random values in Elm. It defines a type called `Generator` which can be think of as a recipe for generating random values.

Here is a definition of genrator for random numbers between 1 and 4.

```elm
import Random exposing (Generator)

randomGrade : Generator Int
randomGrade =
    Random.int 1 4
```

If we want to use it, we have two options. The first is using `generate` function to create a command. Then we got the generated value back with a defined message to our update function. Here's an example:


```elm
import Random exposing (Generator)


type Msg = NewGrade Int

generateGrade : Cmd
generateGrade =
    Random.generate NewGrade randomGrade
```

The other option is to use `step` functions. It requires the generator and also a `Seed` and returns a tuple with generated value and a new `Seed`. The initial seed can be hardcoded (but then the generated values are same each time we run the application), send to Elm via Flags (we'll cover those in the next lesson) or using `generate` function first to get the seed and then use it to generate other random values.

```elm
import Random exposing (Seed)


generateGrade : Seed -> (Int, Seed)
generateGrade seed =
    Random.step randomGrade seed
```

## Materials

- [Examples - TODO List](https://github.com/MI-AFP/elm-examples/tree/master/todo)
- [Examples - Timer](https://github.com/MI-AFP/elm-examples/tree/master/timer)

## Further Reading

- [Commands and Subscriptions](https://guide.elm-lang.org/effects/)
- [Elm Europe 2017 - Evan Czaplicki - The life of a file](https://www.youtube.com/watch?v=XpDsk374LDE)
