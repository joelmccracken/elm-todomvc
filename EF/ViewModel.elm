module EF.ViewModel where

import EF.Model as M

import List

alist_find : a -> AssocList a b -> Maybe b
alist_find key alist =
   let matching      = List.filter (\ (k, v)-> k == key) alist
       firstMatching = List.head matching
   in Maybe.map (\ (a,b)->b) firstMatching


alist_add : a -> b -> AssocList a b -> AssocList a b
alist_add a b alist = (a, b) :: alist

alist_update : a -> b -> AssocList a b -> AssocList a b
alist_update key value alist =
  let updateIfKey = \ (k, v)-> if k == key then (k, value) else (k, v)
  in List.map updateIfKey alist


alist_addOrUpdate : a -> b -> AssocList a b -> AssocList a b
alist_addOrUpdate k v alist =
  case alist_find k alist of
    Just _ ->  alist_update k v alist
    Nothing -> alist_add k v alist

type alias AssocList a b = List (a, b)

type alias ViewTask =
  { task       : M.Task
  , editing    : Bool
  }


type alias ViewModel =
  { newTask        : String
  , newTaskProject : Maybe Int
  , taskEditing    : AssocList Int Bool
  , taskVisibility : String
  , model          : M.Model
  }





empty : M.Model -> ViewModel
empty model =
  { newTask = ""
  , newTaskProject = Nothing
  , taskEditing    = []
  , taskVisibility = ""
  , model = model
  }


isEditing : ViewModel -> M.Task -> Bool
isEditing vmodel vtask =
  case alist_find vtask.id vmodel.taskEditing of
    Just v  -> v
    Nothing -> False


setEditing : ViewModel -> Int -> Bool -> ViewModel
setEditing vmodel id isEditing =
  { vmodel |
      taskEditing = alist_addOrUpdate id isEditing vmodel.taskEditing
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


-- isCompleted : ViewModel -> ViewTask -> Bool
-- isCompleted vmodel task =
--   let isEditing = (Dict.get task vmodel.taskEditing)
--       completion = case isEditing of
--                          Just v  -> v
--                          Nothing -> False
--       filtered = List.filter .completed vmodel.model.tasks
--   in (mapTasks vmodel) filtered


isCompleted : ViewModel -> ViewTask -> Bool
isCompleted model task =
  let foundTask = task |> findModelTask model
  in case foundTask of
       Just model -> model.completed
       Nothing    -> False

findModelTask : ViewModel -> ViewTask -> Maybe M.Task
findModelTask model task =
  model.model.tasks
    |> List.filter (\t-> t.id == task.task.id)
    |> List.head
