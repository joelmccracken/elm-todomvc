module EF.ViewModel where

import EF.Model as M

import List
import Dict

type alias ViewTask =
  { task       : M.Task
  , editing    : Bool
  }


type alias ViewModel =
  { newTask        : String
  , newTaskProject : Maybe Int
  , taskEditing    : Dict.Dict Int Bool
  , taskVisibility : String
  , model          : M.Model
  }


empty : M.Model -> ViewModel
empty model =
  { newTask = ""
  , newTaskProject = Nothing
  , taskEditing    = Dict.empty
  , taskVisibility = ""
  , model = model
  }


-- isCompleted : Model -> Task -> Bool
-- isCompleted model task =
--   let foundTask = task |> findModelTask model
--   in case foundTask of
--        Just model -> model.completed
--        Nothing    -> False

-- findModelTask : Model -> ViewTask -> Maybe M.Task
-- findModelTask model task =
--   model.model.tasks
--     |> List.filter (\t-> t.id == task.task.id)
--     |> List.head


isEditing : ViewModel -> M.Task -> Bool
isEditing vmodel vtask =
  case (Dict.get vtask.id vmodel.taskEditing) of
    Just v  -> v
    Nothing -> False


setEditing : ViewModel -> Int -> Bool -> ViewModel
setEditing vmodel id isEditing =
  { vmodel |
      taskEditing = Dict.insert id isEditing vmodel.taskEditing
  }


extendModelTask : ViewModel -> M.Task -> ViewTask
extendModelTask vmodel task =
  let editing = isEditing vmodel task
  in
  { task = task
  , editing = editing
  }


extendModelTasks : ViewModel -> List M.Task -> List ViewTask
extendModelTasks vmodel = List.map (extendModelTask vmodel)


tasks : ViewModel -> List ViewTask
tasks model = (extendModelTasks model) model.model.tasks


-- completed : Model -> List Task
-- completed model =
--   let completed = List.filter .completed model.model.tasks
--   in (mapTasks model) completed


-- isCompleted : Model -> Task -> Bool
-- isCompleted vmodel task =
--   let isEditing = (Dict.get task vmodel.taskEditing)
--       completion = case isEditing of
--                          Just v  -> v
--                          Nothing -> False
--       filtered = List.filter .completed vmodel.model.tasks
--   in (mapTasks vmodel) filtered
