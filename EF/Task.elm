module EF.Task where

import Signal exposing (Signal, Address)
import Html exposing (Html, Attribute, text, header, h1, input)
import Html.Attributes exposing (id, placeholder, autofocus, value, name)
import Html.Events exposing (on, targetValue, keyCode)
import Json.Decode


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
