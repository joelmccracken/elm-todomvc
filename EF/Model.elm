module EF.Model where

type alias Model =
  { tasks : List Task
  , nextTaskID : Int
  , projects : List Project
  , nextProjectID : Int
  }


type alias Task =
  { description : String
  , completed   : Bool
  , id          : Int
  , projectID   : Maybe Int
  }


type alias Project =
  { description : String
  , completed   : Bool
  , id          : Int
  }


createNewTask : Model -> String -> Model
createNewTask model taskText =
  let newTasks = model.tasks ++ [newTask taskText model.nextTaskID]
  in
  { model |
      nextTaskID = model.nextTaskID + 1,
      tasks      = newTasks
  }


newTask : String -> Int -> Task
newTask desc id =
  { description = desc
  , id = id
  , completed = False
  , projectID = Nothing
  }


empty : Model
empty =
  { tasks    = []
  , nextTaskID = 0

  , projects = []
  , nextProjectID = 0
  }
