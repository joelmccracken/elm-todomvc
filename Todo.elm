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
    { tasks : List Task
    , newTask : String
    , uid : Int
    , visibility : String
    }


type alias Task =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


newTask : String -> Int -> Task
newTask desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


emptyModel : Model
emptyModel =
    { tasks = []
    , visibility = "All"
    , newTask = ""
    , uid = 0
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
      TaskUpdate tu -> task_update tu model


task_update : EF.Task.Update -> Model -> Model
task_update tu model =
    case tu of
        EF.Task.Add ->
          { model |
              uid = model.uid + 1,
              newTask = "",
              tasks =
              if String.isEmpty model.newTask
              then model.tasks
              else model.tasks ++ [newTask model.newTask model.uid]
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
       task_view (Signal.forwardTo address TaskUpdate) model
      , infoFooter
      ]


task_view : Address EF.Task.Update -> Model -> Html
task_view address model =
  section
    [ id "todoapp" ]
    [ lazy2 task_newTaskEntry address model.newTask
    , lazy3 task_list address model.visibility model.tasks
    , lazy3 task_controls address model.visibility model.tasks
    ]

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


task_newTaskEntry : Address EF.Task.Update -> String -> Html
task_newTaskEntry address task =
    header
      [ id "header" ]
      [ h1 [] [ text "todos" ]
      , input
          [ id "new-todo"
          , placeholder "What needs to be done?"
          , autofocus True
          , value task
          , name "newTodo"
          , on "input" targetValue (Signal.message address << (EF.Task.UpdateNewTask))
          , onEnter address (EF.Task.Add)
          ]
          []
      ]


task_list : Address EF.Task.Update -> String -> List Task -> Html
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


task_item : Address EF.Task.Update -> Task -> Html
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
          , onEnter address ((EF.Task.EditingTask todo.id False))
          ]
          []
      ]


task_controls : Address EF.Task.Update -> String -> List Task -> Html
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
