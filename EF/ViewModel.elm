module EF.ViewModel where

import EF.Model as M

import List
import Dict

type alias Task =
  { task       : M.Task
  , editing    : Bool
  }


type alias Model =
  { newTask        : String
  , newTaskProject : Maybe Int
  , taskEditing    : Dict.Dict Int Bool
  , taskVisibility : String
  , model          : M.Model
  }


empty : M.Model -> Model
empty model =
  { newTask = ""
  , newTaskProject = Nothing
  , taskEditing    = Dict.empty
  , taskVisibility = ""
  , model = model
  }


isCompleted : Model -> Task -> Bool
isCompleted model task =
  let foundTask = task |> findModelTask model
  in case foundTask of
       Just model -> model.completed
       Nothing    -> False

findModelTask : Model -> Task -> Maybe M.Task
findModelTask model task =
  model.model.tasks
    |> List.filter (\t-> t.id == task.task.id)
    |> List.head


extendModelTask : Model -> M.Task -> Task
extendModelTask vmodel task =
  let editing = case (Dict.get task.id vmodel.taskEditing) of
                  Just v  -> v
                  Nothing -> False
  in
  { task = task
  , editing = editing
  }


mapTasks : Model -> List M.Task -> List Task
mapTasks model = List.map (extendModelTask model)


tasks : Model -> List Task
tasks model = (mapTasks model) model.model.tasks


completed : Model -> List Task
completed model =
  let filtered = List.filter .completed model.model.tasks
  in (mapTasks model) filtered

isCompleted : Model -> Task -> Bool
isCompleted model =
  let
      isEditing = (Dict.get task.id vmodel.taskEditing)
      completion = case isEditing of
                         Just v  -> v
                         Nothing -> False
  let filtered = List.filter .completed model.model.tasks
  in (mapTasks model) filtered
