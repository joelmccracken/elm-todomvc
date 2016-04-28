module EF.Task where

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
