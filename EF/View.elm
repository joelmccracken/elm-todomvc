module EF.View where

import Signal exposing (Signal, Address)
import Html exposing (Html, Attribute, text, header, h1, input, a, li, button, ul, strong, span, footer, label, div, section, select, option)
import Html.Attributes exposing (id, placeholder, autofocus, value, name, href, classList, hidden, class, type', checked, style, for)
import Html.Events exposing (on, targetValue, keyCode, onClick, onDoubleClick, onBlur)
import Json.Decode
import Html.Lazy exposing (lazy, lazy2, lazy3)

import EF.Model as Model
import EF.ViewModel exposing (ViewModel)
import EF.Update as Update


view : Address Update.TaskAction -> ViewModel -> Html
view address model =
  section
    [ id "todoapp" ]
    [ lazy2 newTaskEntry address model
--    , lazy3 list address model.visibility model.tasks
--    , lazy3 controls address model.visibility model.tasks
    ]


newTaskEntry : Address Update.TaskAction -> ViewModel -> Html
newTaskEntry address viewModel =
    header
      [ id "header" ]
      [ h1 [] [ text "Tasks" ]
      , input
          [ id "new-task"
          , placeholder "What needs to be done?"
          , autofocus True
          , value viewModel.newTask
          , name "newTask"
          , on "input" targetValue (Signal.message address << Update.UpdateNewTask)
          , onEnter address Update.CreateNewTask
          ]
          []
      , select
          [ onChange address (\x-> Update.UpdateNewTaskProject (Just x))]
          [ option [value "1"] [text "whatever"]
          , option [value "2"] [text "wherever"]
          ]
      ]

onChange : Address a -> (Int -> a) -> Attribute
onChange address value =
  on "change" Json.Decode.int
     (\id-> Signal.message address (value id))



controls : Address Update.TaskAction -> String -> ViewModel -> Html
controls address visibility viewModel =
    let tasks = VM.tasks viewModel
        tasksCompleted = List.length (List.filter (VM.isCompleted viewModel) tasks)
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
          [ visibilitySwap address "#/" "All" visibility
          , text " "
          , visibilitySwap address "#/active" "Active" visibility
          , text " "
          , visibilitySwap address "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          , onClick address (Update.DeleteTaskComplete)
          ]
          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
      ]


visibilitySwap : Address Update.TaskAction -> String -> String -> String -> Html
visibilitySwap address uri visibility actualVisibility =
    li
      [ onClick address (Update.ChangeVisibilityTask visibility) ]
      [ a [ href uri, classList [("selected", visibility == actualVisibility)] ] [ text visibility ] ]


item : Address Update.TaskAction -> Model.Task -> Html
item address todo =
    li
      [ classList [ ("completed", todo.completed), ("editing", todo.editing) ] ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked todo.completed
              , onClick address (Update.CheckTask todo.id (not todo.completed))
              ]
              []
          , label
              [ onDoubleClick address (Update.EditingTask todo.id True) ]
              [ text todo.description ]
          , button
              [ class "destroy"
              , onClick address (Update.DeleteTask todo.id)
              ]
              []
          ]
      , input
          [ class "edit"
          , value todo.description
          , name "title"
          , id ("todo-" ++ toString todo.id)
          , on "input" targetValue (Signal.message address << (Update.UpdateTask todo.id))
          , onBlur address (Update.EditingTask todo.id False)
          , onEnter address (Update.EditingTask todo.id False)
          ]
          []
      ]


list : Address Update.TaskAction -> String -> ViewModel -> Html
list address visibility vmodel =
    let isVisible todo =
            case visibility of
              "Completed" -> todo.completed
              "Active" -> not todo.completed
              _ -> True
        tasks = VM.tasks vmodel
        allCompleted = List.all VM.isCompleted VM.tasks(model)

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
          , onClick address ((Update.CheckAllTask (not allCompleted)))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (List.map (item address) (List.filter isVisible vmodel))
      ]


-- VIEW UTILITY --

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.Decode.customDecoder keyCode is13)
      (\_-> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
