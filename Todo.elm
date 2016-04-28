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
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Window
import EF.Task

---- MODEL ----

-- The full application state of our todo app.
type alias Model =
    { tasks: EF.Task.Collection
    }


emptyModel : Model
emptyModel =
    { tasks = EF.Task.emptyTaskCollection
    }


---- UPDATE ----

-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following for more info on this pattern and
-- some alternatives: https://github.com/evancz/elm-architecture-tutorial/
type Action
    = NoOp
    | TaskUpdate EF.Task.Update

-- How we update our Model on a given Action?
update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model
      TaskUpdate task_act -> { model | tasks = task_update task_act model.tasks }


task_update : EF.Task.Update -> EF.Task.Collection -> EF.Task.Collection
task_update task_act model =
    case task_act of
        EF.Task.Add ->
          { model |
              uid = model.uid + 1,
              newTask = "",
              tasks =
              if String.isEmpty model.newTask
              then model.tasks
              else model.tasks ++ [EF.Task.newTask model.newTask model.uid]
          }
        EF.Task.UpdateNewTask str ->
            { model | newTask = str }

        EF.Task.EditingTask id isEditing ->
            let updateTask t = if t.id == id then { t | editing = isEditing } else t
              in
                { model | tasks = List.map updateTask model.tasks }

        EF.Task.UpdateTask id task ->
            let updateTask t = if t.id == id then { t | description = task } else t
            in
                { model | tasks = List.map updateTask model.tasks }

        EF.Task.Delete id ->
            { model | tasks = List.filter (\t -> t.id /= id) model.tasks }

        EF.Task.DeleteComplete ->
            { model | tasks = List.filter (not << .completed) model.tasks }

        EF.Task.Check id isCompleted ->
            let updateTask t = if t.id == id then { t | completed = isCompleted } else t
            in
                { model | tasks = List.map updateTask model.tasks }

        EF.Task.CheckAll isCompleted ->
            let updateTask t = { t | completed = isCompleted }
            in
                { model | tasks = List.map updateTask model.tasks }

        EF.Task.ChangeVisibility visibility ->
            { model | visibility = visibility }

---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    div
      [ class "todomvc-wrapper"
      , style [ ("visibility", "hidden") ]
      ]
      [
       task_view (Signal.forwardTo address TaskUpdate) model.tasks
      , infoFooter
      ]


task_view : Address EF.Task.Update -> EF.Task.Collection -> Html
task_view address model =
  section
    [ id "todoapp" ]
    [ lazy2 EF.Task.newTaskEntry address model.newTask
    , lazy3 task_list address model.visibility model.tasks
    , lazy3 task_controls address model.visibility model.tasks
    ]


task_list : Address EF.Task.Update -> String -> List EF.Task.Task -> Html
task_list address visibility tasks =
    let isVisible todo =
            case visibility of
              "Completed" -> todo.completed
              "Active" -> not todo.completed
              _ -> True

        allCompleted = List.all .completed tasks

        cssVisibility = if List.isEmpty tasks then "hidden" else "visible"
    in
    section
      [ id "main"
      , style [ ("visibility", cssVisibility) ]
      ]
      [ input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          , onClick address ((EF.Task.CheckAll (not allCompleted)))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (List.map (task_item address) (List.filter isVisible tasks))
      ]


task_item : Address EF.Task.Update -> EF.Task.Task -> Html
task_item address todo =
    li
      [ classList [ ("completed", todo.completed), ("editing", todo.editing) ] ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked todo.completed
              , onClick address (EF.Task.Check todo.id (not todo.completed))
              ]
              []
          , label
              [ onDoubleClick address ((EF.Task.EditingTask todo.id True)) ]
              [ text todo.description ]
          , button
              [ class "destroy"
              , onClick address ((EF.Task.Delete todo.id))
              ]
              []
          ]
      , input
          [ class "edit"
          , value todo.description
          , name "title"
          , id ("todo-" ++ toString todo.id)
          , on "input" targetValue (Signal.message address << (EF.Task.UpdateTask todo.id))
          , onBlur address ((EF.Task.EditingTask todo.id False))
          , EF.Task.onEnter address ((EF.Task.EditingTask todo.id False))
          ]
          []
      ]


task_controls : Address EF.Task.Update -> String -> List EF.Task.Task -> Html
task_controls address visibility tasks =
    let tasksCompleted = List.length (List.filter .completed tasks)
        tasksLeft = List.length tasks - tasksCompleted
        item_ = if tasksLeft == 1 then " item" else " items"
    in
    footer
      [ id "footer"
      , hidden (List.isEmpty tasks)
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (toString tasksLeft) ]
          , text (item_ ++ " left")
          ]
      , ul
          [ id "filters" ]
          [ task_visibilitySwap address "#/" "All" visibility
          , text " "
          , task_visibilitySwap address "#/active" "Active" visibility
          , text " "
          , task_visibilitySwap address "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          , onClick address (EF.Task.DeleteComplete)
          ]
          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
      ]


task_visibilitySwap : Address EF.Task.Update -> String -> String -> String -> Html
task_visibilitySwap address uri visibility actualVisibility =
    li
      [ onClick address ((EF.Task.ChangeVisibility visibility)) ]
      [ a [ href uri, classList [("selected", visibility == actualVisibility)] ] [ text visibility ] ]


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
