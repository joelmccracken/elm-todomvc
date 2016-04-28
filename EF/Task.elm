module EF.Task where

import Signal exposing (Signal, Address)
import Html exposing (Html, Attribute, text, header, h1, input, a, li, button, ul, strong, span, footer)
import Html.Attributes exposing (id, placeholder, autofocus, value, name, href, classList, hidden, class)
import Html.Events exposing (on, targetValue, keyCode, onClick)
import Json.Decode
import String


onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.Decode.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


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
