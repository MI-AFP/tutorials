# Elm - Real World Use Cases

## Complex Forms

Sometimes, we need more complex forms in our application. Using common Elm approach with a message per input field can result in a lot of boilerplate code. Luckily there are some libraries to help with that. One of them is [etaque/elm-form](https://package.elm-lang.org/packages/etaque/elm-form/latest).
At the price of losing some type safety (field names are using strings), we get nice validation API similar to JSON decoders API with some basic validation and a possibility to create our own, support for nested fields and lists.

First, we need to define a type that should be represented in our form, e.g., a person.

```elm
type alias Person =
    { name : String
    , age : Int
    }
```

Then, we define validation (notice that the API is basically the same as using `Json.Decode` module):

```elm
import Form
import Form.Validate as Validate

personValidation : Validate.Validation CustomFormError Person
personValidation =
    Validate.map2 Person
        (Validate.field "name" Validate.string)
        (Validate.field "age" Validate.int)
```

After that, we can initialize the form, either empty:

```elm
initPersonForm : Form.Form CustomFormError Person
initPersonForm =
    Form.initial [] personValidation
```

Or using existing person data (notice that the API for the initial data is basically the same as using `Json.Encode` module):

```elm
import Form.Field as Field

initPersonFormWithPerson : Person -> Form.Form CustomFormError Person
initPersonFormWithPerson { age, name } =
    let
        initials =
            [ ( "name", Field.string name )
            , ( "age", Field.int age )
            ]
    in
    Form.initial initials personValidation
```

The library comes with functions for generating views. We need to create our own functions for converting form errors to string (because we can add our own errors). Also, all fields are represented as string or bool (for checkboxes), so we cannot really use for example input type number.

```elm
import Form
import Form.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events

viewForm : Form.Form () Person -> Html Form.Msg
viewForm form =
    let
        viewError { liveError } =
            case liveError of
                Just error ->
                    Html.p [ Attributes.class "error" ]
                        [ Html.text (errorToString error) ]

                Nothing ->
                    Html.text ""

        nameField =
            Form.getFieldAsString "name" form

        ageField =
            Form.getFieldAsString "age" form
    in
    form [ Events.onSubmit Form.Submit ]
        [ Html.label [] [ Html.text "name" ]
        , Input.textInput nameField []
        , viewError nameField
        , Html.label [] [ Html.text "age" ]
        , Input.textInput ageField []
        , viewError ageField
        , Html.button [ Attributes.type_ "submit" ] [ Html.text "Submit" ]
        ]
```

The last thing is handling Form messages. If the message is `Form.Submit`, and the form is valid, we want to handle submitting the form. To get the output, there is a function `Form.getOutput`, it either returns `Just` the type in the form or `Nothing` if the form contains invalid values.

We want to use `Form.update` function for other messages to update the form model.

```elm
type Msg
    = FormMsg Form.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.personForm ) of
                ( Form.Submit, Just form ) ->
                    -- The output is the original type (Person in this case)
                    -- We get it only if the validation passes
                    let
                        _ =
                            Debug.log "Submit form" form
                    in
                    ( model, Cmd.none )

                _ ->
                    ( { model | personForm = Form.update personValidation formMsg model.personForm }
                    , Cmd.none
                    )
```

## SVG

Elm has a package [elm/svg](https://package.elm-lang.org/packages/elm/svg/latest/) for creating SVG images in Elm. The API looks like corresponding packages for Html. Now we have `Svg` module with functions for SVG elements (e.g., `rect` or `circle`), `Svg.Attributes` for the attributes used by SVG elements (e.g., `strokeWidth` or `x`) and `Svg.Events` for JavaScript events, same as `Html.Events`.

SVG is good for visualisations. We can start at [SVG element reference](https://developer.mozilla.org/en-US/docs/Web/SVG/Element) to find the elements we need. Here's an example from package documentation of drawing a rounded rectangle:

```elm
import Html exposing (Html)
import Svg
import Svg.Attributes as Attributes

roundRect : Html msg
roundRect =
    Svg.svg
      [ Attributes.width "120"
      , Attributes.height "120"
      , Attributes.viewBox "0 0 120 120"
      ]
      [ Svg.rect
        [ Attributes.x "10"
        , Attributes.y "10"
        , Attributes.width "100"
        , Attributes.height "100"
        , Attributes.rx "15"
        , Attributes.ry "15"
        ]
        []
      ]
```

## Files

Since Elm 0.19 there is [elm/file](https://package.elm-lang.org/packages/elm/file/latest/) package for working with files. We can use it for allowing users to download a file generated in Elm, or for uploading files.

Example of download a markdown file generated from a string in Elm:

```elm
import File.Download as Download

save : String -> Cmd msg
save markdown =
  Download.string "draft.md" "text/markdown" markdown
```

Example of file select:

```elm
import File.Select as Select

type Msg
  = ImagesRequested
  | ImagesLoaded File (List File)

requestImages : Cmd Msg
requestImages =
  Select.files [ "image/png", "image/jpg" ] ImagesLoaded
```

If we get `File` from the previous example, we can send it to the server using [elm/http](https://package.elm-lang.org/packages/elm/http/2.0.0/) package. There is a [fileBody](https://package.elm-lang.org/packages/elm/http/latest/Http#fileBody) function for that. We can explore [examples](https://github.com/elm/file/tree/master/examples) in elm/file package to see how it works.

## Graph QL

There is a couple of packages for working with GraphQL in Elm.

### [dillonkearns/elm-graphql](https://github.com/dillonkearns/elm-graphql/tree/4.2.1)

This is one of the most popular GraphQL packages for Elm. It comes with a command-line code generator to create type-safe Elm code for GraphQL endpoint. Then, we get type-safe GraphQL queries. It is also eliminating GraphQL features in favour of Elm language constructs.

Here's an example from the documentation.

The GraphQL query:

```graphql
query {
  human(id: "1001") {
    name
    homePlanet
  }
}
```

It looks like this in the Elm code (`StarWars` packages are auto-generated using the command line tool):

```elm
import Graphql.Operation as Operation
import Graphql.SelectionSet as SelectionSet
import StarWars.Object.Human as HumanObject
import StarWars.Query as Query
import StarWars.Scalar as Scalar

query : SelectionSet.SelectionSet (Maybe Human) Operation.RootQuery
query =
    Query.human { id = Scalar.Id "1001" } humanSelection

type alias Human =
    { name : String
    , homePlanet : Maybe String
    }

humanSelection : SelectionSet.SelectionSet Human HumanObject
humanSelection =
    SelectionSet.map2 Human
        Human.name
        Human.homePlanet
```

### [jamesmacaulay/elm-graphql](https://package.elm-lang.org/packages/jamesmacaulay/elm-graphql/latest)

This is another popular package for GraphQL. It provides an interface for working with GraphQL queries and schemas in Elm. Building requests works similarly as JSON decoders.

Here's an example from the documentation, we need to define the types by ourselves:

```elm
type alias Photo =
    { url : String
    , caption : String
    }

type alias User =
    { name : String
    , photos : List Photo
    }
```

Then we build a query document using the library:

```elm
userQuery : Document Query User { vars | userID : String }
userQuery =
    let
        userIDVar =
            Var.required "userID" .userID Var.id

        photo =
            object Photo
                |> with (field "url" [] string)
                |> with (field "caption" [] string)

        user =
            object User
                |> with (field "name" [] string)
                |> with (field "photos" [] (list photo))

        queryRoot =
            extract
                (field "user"
                    [ ( "id", Arg.variable userIDVar ) ]
                    user
                )
    in
    queryDocument queryRoot
```

The document would be encoded into this string:

```graphql
query ($userID: ID!) {
  user(id: $userID) {
    name
    photos {
      url
      caption
    }
  }
}
```

## WebSockets

There is a package [elm-lang/websockets](https://package.elm-lang.org/packages/elm-lang/websocket/latest), however, it was **not yet updated** to Elm 0.19.1 - it remains in 0.19.0 ELM version. It [should be updated](https://discourse.elm-lang.org/t/updating-packages/1771) at some point in the future. There is a 3rd party package [billstclair/elm-websocket-client](https://package.elm-lang.org/packages/billstclair/elm-websocket-client/latest/) that is converting the original package to Elm 0.19.1.

The other option is to [use ports](https://stackoverflow.com/a/52569683/2492795) and to implement WebSocket interactions on the JavaScript side.

## Materials

- [Examples - Complex Form](https://github.com/MI-AFP/elm-examples/tree/master/form)
- [Examples - SVG Clock](https://github.com/MI-AFP/elm-examples/tree/master/clock)

## Further Reading

- [SVG: Scalable Vector Graphics](https://developer.mozilla.org/en-US/docs/Web/SVG)
- [Line Charts - A library for plotting line charts in SVG. Written in all Elm.](https://github.com/terezka/line-charts)
- [Working with Files](https://elm-lang.org/blog/working-with-files)
- [Type-Safe & Composable GraphQL in Elm](https://medium.com/open-graphql/type-safe-composable-graphql-in-elm-b3378cc8d021)
- [Elm Port Examples](https://github.com/MattCheely/elm-port-examples)
