module Todo where
{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

This clean division of concerns is a core part of Elm. You can read more about
this in the Pong tutorial: http://elm-lang.org/blog/making-pong

This program is not particularly large, so definitely see the following
for notes on structuring more complex GUIs with Elm:
https://github.com/evancz/elm-architecture-tutorial/
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import EF.Task
import EF.Project
import EF.Model
import EF.ViewModel exposing (ViewModel)

---- MODEL ----

-- The full application state of our todo app.
type alias Model =
    { tasks : EF.Task.Collection
    , projects : EF.Project.Collection
    , nested   : Model2
    }

type alias Model2 =
  { model : EF.Model.Model
  , viewModel : ViewModel  }

emptyModel2 : Model2
emptyModel2 =
  let model' = EF.Model.empty
  in
  { model = model'
  , viewModel = EF.ViewModel.empty model'
  }

emptyModel : Model
emptyModel =
  { tasks = EF.Task.emptyTaskCollection
  , projects = EF.Project.emptyProjectCollection
  , nested = emptyModel2
  }

---- UPDATE ----

-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following for more info on this pattern and
-- some alternatives: https://github.com/evancz/elm-architecture-tutorial/
type Action
    = NoOp
    | TaskUpdate EF.Task.Update
    | ProjectUpdate EF.Project.Update

-- How we update our Model on a given Action?
update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model
      TaskUpdate taskAction -> { model | tasks = EF.Task.update taskAction model.tasks }
      ProjectUpdate projectAction -> { model | projects = EF.Project.update projectAction model.projects }

---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    div
      [ class "todomvc-wrapper"
      , style [ ("visibility", "hidden") ]
      ]
      [ EF.Task.view (Signal.forwardTo address TaskUpdate) model.tasks
      , EF.Project.view (Signal.forwardTo address ProjectUpdate) model.projects
      , infoFooter
      ]


infoFooter : Html
infoFooter =
    footer [ id "info" ]
      [ p [] [ text "Double-click to edit a todo" ]
      , p []
          [ text "Written by "
          , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
          ]
      , p []
          [ text "Part of "
          , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
          ]
      ]


---- INPUTS ----

-- wire the entire application together
main : Signal Html
main =
  Signal.map (view actions.address) model


-- manage the model of our application over time
model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


initialModel : Model
initialModel =
  Maybe.withDefault emptyModel getStorage

-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


port focus : Signal String
port focus =
    let needsFocus act =
            case act of
              TaskUpdate (EF.Task.EditingTask id bool) -> bool
              _ -> False

        toSelector act =
            case act of
              TaskUpdate (EF.Task.EditingTask id _) -> "#todo-" ++ toString id
              _ -> ""
    in
        actions.signal
          |> Signal.filter needsFocus (TaskUpdate (EF.Task.EditingTask 0 True))
          |> Signal.map toSelector

-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model
