module Main exposing (..)

import Html exposing (Html, div, form, h1, img, input, span, text)
import Html.Attributes exposing (class, classList, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Todo =
  { text : String
  , complete : Bool
  }

type alias Model =
    { todos : List Todo
    , nextTodo : Maybe String
    }

init : (Model, Cmd Msg)
init =
    ({ todos = []
    , nextTodo = Nothing
    }, getTodos )


type Msg
    = AddTodo
    | RemoveAll
    | ToggleComplete Int
    | UpdateNextTodo String
    | ReceivedTodos (Result Http.Error (List Todo))
    | AddResponse (Result Http.Error (List Todo))
    | RemoveResponse (Result Http.Error (List Todo))
    | ToggleResponse (Result Http.Error (List Todo))


defaultTodo : Maybe String -> Todo
defaultTodo txt =
  (Todo (Maybe.withDefault "" txt) False)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTodo ->
      ( model
      , addTodo <| defaultTodo model.nextTodo)

    RemoveAll ->
      (model, removeAll)

    ToggleComplete index ->
      (model, toggleComplete index)

    UpdateNextTodo txt ->
      ({ model | nextTodo =
        case txt == "" of
          True -> Nothing
          False -> Just txt }
      , Cmd.none)

    ReceivedTodos res ->
      receivedTodos model res

    AddResponse res ->
      receivedTodos model res

    RemoveResponse res ->
      receivedTodos model res

    ToggleResponse res ->
      receivedTodos model res

receivedTodos : Model -> (Result Http.Error (List Todo)) -> (Model, Cmd Msg)
receivedTodos model res =
  case res of
    Ok todos ->
      ({model | todos = todos, nextTodo = Nothing }
      , Cmd.none
      )

    Err err ->
      let
        _ = Debug.log "Error: " <| toString err
      in
      ( model, Cmd.none )



view : Model -> Html Msg
view model =
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
          (List.indexedMap renderTodo model.todos)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

--API
toggleComplete : Int -> Cmd Msg
toggleComplete index =
  Http.send ToggleResponse <|
    Http.post ("http://localhost:4000/api/toggle/" ++ (toString index))
      Http.emptyBody
      todosDecoder

removeAll : Cmd Msg
removeAll =
  Http.send RemoveResponse <|
    Http.post "http://localhost:4000/api/remove"
      Http.emptyBody
      todosDecoder

addTodo : Todo -> Cmd Msg
addTodo todo =
  Http.send AddResponse <|
    Http.post "http://localhost:4000/api/add"
      (Http.jsonBody <| encodeTodo todo)
      todosDecoder

encodeTodo : Todo -> Encode.Value
encodeTodo todo =
  Encode.object
    [("text", Encode.string todo.text)
    , ("complete", Encode.bool todo.complete) ]


getTodos : Cmd Msg
getTodos =
  Http.send ReceivedTodos <|
    Http.get "http://localhost:4000/api/get" todosDecoder

todosDecoder : Decode.Decoder (List Todo)
todosDecoder =
  Decode.list todoDecoder

todoDecoder : Decode.Decoder Todo
todoDecoder =
  Decode.map2 Todo
    (Decode.field "text" Decode.string)
    (Decode.field "complete" Decode.bool)



---- PROGRAM ----


main =
    Html.program
      { view = view
      , init = init
      , update = update
      , subscriptions = always Sub.none
      }
