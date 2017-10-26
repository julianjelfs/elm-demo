module Main exposing (..)

import Html exposing (Html, div, form, h1, img, input, span, text)
import TodoList

type alias Model =
    { firstList: TodoList.Model
    , secondList: TodoList.Model
    }

init : (Model, Cmd Msg)
init =
  let
    (firstModel, firstCmds) =
      TodoList.init

    (secondModel, secondCmds) =
      TodoList.init
  in
    ({ firstList = firstModel
    , secondList = secondModel
    }
    , Cmd.batch
        [ Cmd.map FirstTodoListMsg firstCmds
        , Cmd.map SecondTodoListMsg secondCmds]
    )


type Msg
    = FirstTodoListMsg TodoList.Msg
    | SecondTodoListMsg TodoList.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FirstTodoListMsg subMsg ->
      let
        (updated, cmds) =
          TodoList.update subMsg model.firstList
      in
      ( { model | firstList = updated }
      , Cmd.map FirstTodoListMsg cmds )

    SecondTodoListMsg subMsg ->
      let
        (updated, cmds) =
          TodoList.update subMsg model.secondList
      in
      ( { model | secondList = updated }
      , Cmd.map SecondTodoListMsg cmds )


view : Model -> Html Msg
view model =
  div
    []
    [ Html.map FirstTodoListMsg <| TodoList.view model.firstList
    , Html.map SecondTodoListMsg <| TodoList.view model.secondList
    ]

---- PROGRAM ----

main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

