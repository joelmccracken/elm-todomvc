module EF.Task where

-- type alias Task =
--     { description : String
--     , completed : Bool
--     , editing : Bool
--     , id : Int
--     }

type alias TaskCollection =
  { tasks : List Int
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
