module Main exposing (..)

import Html exposing (Html, div, form, h1, img, input, span, text)
import Html.Attributes exposing (class, classList, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type alias Todo =
  { text : String
  , complete : Bool
  }

type alias Model =
    { todos : List Todo
    , nextTodo : Maybe String
    }


init : Model
init =
    { todos = []
    , nextTodo = Nothing
    }


type Msg
    = AddTodo
    | RemoveAll
    | ToggleComplete Int
    | UpdateNextTodo String


toggleCompleteAt : Int -> List Todo -> List Todo
toggleCompleteAt index =
  List.indexedMap
    (\i t -> if i == index then {t | complete = not t.complete } else t )


update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTodo ->
      { model | todos = (Todo (Maybe.withDefault "" model.nextTodo) False) :: model.todos
      , nextTodo = Nothing
      }

    RemoveAll ->
      { model | todos = [] }

    ToggleComplete index ->
      { model | todos = toggleCompleteAt index model.todos }

    UpdateNextTodo txt ->
      { model | nextTodo =
        case txt == "" of
          True -> Nothing
          False -> Just txt }


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



---- PROGRAM ----


main =
    Html.beginnerProgram
        { view = view
        , model = init
        , update = update
        }
