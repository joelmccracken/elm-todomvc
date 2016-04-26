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
    | TaskAction Task_Update

type Task_Update
    = UpdateNewTask String
    | EditingTask Int Bool
    | UpdateTask Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


-- How we update our Model on a given Action?
update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model
      TaskAction tu -> task_update tu model


task_update : Task_Update -> Model -> Model
task_update tu model =
    case tu of
        Add ->
          { model |
              uid = model.uid + 1,
              newTask = "",
              tasks =
              if String.isEmpty model.newTask
              then model.tasks
              else model.tasks ++ [newTask model.newTask model.uid]
          }
        UpdateNewTask str ->
            { model | newTask = str }

        EditingTask id isEditing ->
            let updateTask t = if t.id == id then { t | editing = isEditing } else t
              in
                { model | tasks = List.map updateTask model.tasks }

        UpdateTask id task ->
            let updateTask t = if t.id == id then { t | description = task } else t
            in
                { model | tasks = List.map updateTask model.tasks }

        Delete id ->
            { model | tasks = List.filter (\t -> t.id /= id) model.tasks }

        DeleteComplete ->
            { model | tasks = List.filter (not << .completed) model.tasks }

        Check id isCompleted ->
            let updateTask t = if t.id == id then { t | completed = isCompleted } else t
            in
                { model | tasks = List.map updateTask model.tasks }

        CheckAll isCompleted ->
            let updateTask t = { t | completed = isCompleted }
            in
                { model | tasks = List.map updateTask model.tasks }

        ChangeVisibility visibility ->
            { model | visibility = visibility }

---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    div
      [ class "todomvc-wrapper"
      , style [ ("visibility", "hidden") ]
      ]
      [
       task_view (Signal.forwardTo address TaskAction) model
      , infoFooter
      ]


task_view : Address Task_Update -> Model -> Html
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


task_newTaskEntry : Address Task_Update -> String -> Html
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
          , on "input" targetValue (Signal.message address << (UpdateNewTask))
          , onEnter address (Add)
          ]
          []
      ]


task_list : Address Task_Update -> String -> List Task -> Html
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
          , onClick address ((CheckAll (not allCompleted)))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (List.map (task_item address) (List.filter isVisible tasks))
      ]


task_item : Address Task_Update -> Task -> Html
task_item address todo =
    li
      [ classList [ ("completed", todo.completed), ("editing", todo.editing) ] ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked todo.completed
              , onClick address (Check todo.id (not todo.completed))
              ]
              []
          , label
              [ onDoubleClick address ((EditingTask todo.id True)) ]
              [ text todo.description ]
          , button
              [ class "destroy"
              , onClick address ((Delete todo.id))
              ]
              []
          ]
      , input
          [ class "edit"
          , value todo.description
          , name "title"
          , id ("todo-" ++ toString todo.id)
          , on "input" targetValue (Signal.message address << (UpdateTask todo.id))
          , onBlur address ((EditingTask todo.id False))
          , onEnter address ((EditingTask todo.id False))
          ]
          []
      ]


task_controls : Address Task_Update -> String -> List Task -> Html
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
          , onClick address (DeleteComplete)
          ]
          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
      ]


task_visibilitySwap : Address Task_Update -> String -> String -> String -> Html
task_visibilitySwap address uri visibility actualVisibility =
    li
      [ onClick address ((ChangeVisibility visibility)) ]
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
              TaskAction (EditingTask id bool) -> bool
              _ -> False

        toSelector act =
            case act of
              TaskAction (EditingTask id _) -> "#todo-" ++ toString id
              _ -> ""
    in
        actions.signal
          |> Signal.filter needsFocus (TaskAction (EditingTask 0 True))
          |> Signal.map toSelector


-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model
