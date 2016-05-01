module EF.Task where

import Signal exposing (Signal, Address)
import Html exposing (Html, Attribute, text, header, h1, input, a, li, button, ul, strong, span, footer, label, div, section, select, option)
import Html.Attributes exposing (id, placeholder, autofocus, value, name, href, classList, hidden, class, type', checked, style, for)
import Html.Events exposing (on, targetValue, keyCode, onClick, onDoubleClick, onBlur)
import Json.Decode
import String
import Html.Lazy exposing (lazy, lazy2, lazy3)


onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.Decode.customDecoder keyCode is13)
      (\_-> Signal.message address value)


onChange : Address a -> a -> Attribute
onChange address value =
    on "change" Json.Decode.int
         (\-> Signal.message address (value ))


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


type alias Task =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


type alias Collection =
   { tasks: List Task
   , newTask: String
   , uid : Int
   , visibility : String
   }


type Update
    = UpdateNewTask String
    | EditingTask Int Bool
    | UpdateTask Int String
    | Add
    | UpdateProject
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


newTask : String -> Int -> Task
newTask desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


emptyTaskCollection : Collection
emptyTaskCollection =
  { tasks = []
  , newTask = ""
  , uid = 0
  , visibility = ""
  }


newTaskEntry : Address Update -> String -> Html
newTaskEntry address task =
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
      , select
          [ onChange address UpdateProject]
          [ option [value "1"] [text "whatever"]
          , option [value "2"] [text "wherever"]
          ]
      ]

update : Update -> Collection -> Collection
update action model =
    case action of
        Add ->
          { model |
              uid = model.uid + 1,
              newTask = "",
              tasks =
              if String.isEmpty model.newTask
              then model.tasks
              else model.tasks ++ [newTask model.newTask model.uid]
          }
        UpdateProject -> model
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


controls : Address Update -> String -> List Task -> Html
controls address visibility tasks =
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
          , onClick address (DeleteComplete)
          ]
          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
      ]


visibilitySwap : Address Update -> String -> String -> String -> Html
visibilitySwap address uri visibility actualVisibility =
    li
      [ onClick address ((ChangeVisibility visibility)) ]
      [ a [ href uri, classList [("selected", visibility == actualVisibility)] ] [ text visibility ] ]


item : Address Update -> Task -> Html
item address todo =
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


list : Address Update -> String -> List Task -> Html
list address visibility tasks =
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
          (List.map (item address) (List.filter isVisible tasks))
      ]


view : Address Update -> Collection -> Html
view address model =
  section
    [ id "todoapp" ]
    [ lazy2 newTaskEntry address model.newTask
    , lazy3 list address model.visibility model.tasks
    , lazy3 controls address model.visibility model.tasks
    ]
