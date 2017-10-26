module TodoList exposing (..)

import Html exposing (Html, div, form, h1, img, input, span, text)
import Html.Attributes exposing (class, classList, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (WebData, RemoteData(..))

type alias Todo =
  { text : String
  , complete : Bool
  }

type alias Model =
    { todos : WebData (List Todo)
    , nextTodo : Maybe String
    }


init : (Model, Cmd Msg)
init =
    ({ todos = Loading
    , nextTodo = Nothing
    }, getTodos )


type Msg
    = AddTodo
    | RemoveAll
    | ToggleComplete Int
    | UpdateNextTodo String
    | ReceivedTodos (WebData (List Todo))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTodo ->
      (model
      , addTodo (Todo (Maybe.withDefault "" model.nextTodo) False))

    RemoveAll ->
      (model, removeAll)

    ToggleComplete index ->
      (model, toggleTodo index)

    UpdateNextTodo txt ->
      ({ model | nextTodo =
        case txt == "" of
          True -> Nothing
          False -> Just txt }, Cmd.none )

    ReceivedTodos todos ->
      ({ model | todos = todos }
      , Cmd.none )


view : Model -> Html Msg
view model =
  case model.todos of
    Loading -> text "Loading"
    NotAsked -> text "Not asked"
    Failure err -> text ("oh no" ++ (toString err))
    Success todos ->
      div []
          [ h1 [] [ text "My To-do List" ]
          , form
            [ onSubmit AddTodo ]
            [ input
              [type_ "text"
              , value <| Maybe.withDefault "" model.nextTodo
              , placeholder "What do you have to do?"
              , onInput UpdateNextTodo
              ]
              []
            ]
          , actionButtons model
          , div
            [class "todos"]
            (List.indexedMap renderTodo todos)
          ]

renderTodo : Int -> Todo -> Html Msg
renderTodo index todo =
  div
    [class "todo"]
    [ div
      [ classList [("todo-text", True), ("complete", todo.complete)]
      ]
      [ text todo.text ]
    , div
      [ class "todo-toggle"
      , onClick <| ToggleComplete index
      ]
      [ text (if todo.complete then "✓" else "✗") ]
    ]

actionButtons : Model -> Html Msg
actionButtons model =
  div
    [ class "buttons" ]
    [ input
      [ type_ "button"
      , value "Add"
      , onClick AddTodo
      , disabled (model.nextTodo == Nothing)
      ]
      []
    , input
      [ type_ "button"
      , value "Remove All"
      , onClick RemoveAll
      ]
      []
    ]


getTodos : Cmd Msg
getTodos =
    Http.get "http://localhost:4000/api/get" todosDecoder
      |> RemoteData.sendRequest
      |> Cmd.map ReceivedTodos

todosDecoder : Decode.Decoder (List Todo)
todosDecoder =
  Decode.list todoDecoder

todoDecoder : Decode.Decoder Todo
todoDecoder =
  Decode.map2 Todo
    (Decode.field "text" Decode.string)
    (Decode.field "complete" Decode.bool)

addTodo : Todo -> Cmd Msg
addTodo todo =
    Http.post "http://localhost:4000/api/add"
      (Http.jsonBody (encodeTodo todo))
      todosDecoder
      |> RemoteData.sendRequest
      |> Cmd.map ReceivedTodos

removeAll : Cmd Msg
removeAll =
    Http.post "http://localhost:4000/api/remove"
      Http.emptyBody
      todosDecoder
      |> RemoteData.sendRequest
      |> Cmd.map ReceivedTodos

toggleTodo : Int -> Cmd Msg
toggleTodo index =
    Http.post ("http://localhost:4000/api/toggle/" ++ (toString index))
      Http.emptyBody
      todosDecoder
      |> RemoteData.sendRequest
      |> Cmd.map ReceivedTodos

encodeTodo : Todo -> Encode.Value
encodeTodo todo =
  Encode.object
    [ ("text", Encode.string todo.text)
    , ("complete", Encode.bool todo.complete)
    ]

