module EF.Update where

import String

import EF.Model as Model
import EF.ViewModel

type TaskAction
    = UpdateNewTask String
    | UpdateNewTaskProject (Maybe Int)
    | CreateNewTask
    | EditingTask Int Bool
    | UpdateTask Int String
    | UpdateTaskProject Int Int
    | DeleteTask Int
    | DeleteTaskComplete
    | CheckTask Int Bool
    | CheckAllTask Bool
    | ChangeVisibilityTask String


type ProjectAction
    = UpdateNewProject String
    | Add
    | EditingProject Int Bool
    | UpdateProject Int String
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String

taskUpdate : TaskAction -> Model.Model -> EF.ViewModel.Model -> (Model.Model, EF.ViewModel.Model)
taskUpdate action model viewModel =
    case action of
        CreateNewTask ->
          if String.isEmpty viewModel.newTask
          then (model, viewModel)
          else (Model.createNewTask model viewModel.newTask, {viewModel | newTask = "" })

        UpdateNewTaskProject str -> (model, viewModel)

        UpdateTaskProject taskID projectID -> (model, viewModel)

        UpdateNewTask str ->
          (model, { viewModel | newTask = str })

        EditingTask id isEditing ->
          let
            updateTask t = if t.id == id then { t | editing = isEditing } else t
          in
            (model, { viewModel | tasks = List.map updateTask viewModel.tasks })

        UpdateTask id newDescription ->
          let updateTask t = if t.id == id
                             then { t | description = newDescription }
                             else t
          in
            ({ model | tasks = List.map updateTask model.tasks }, viewModel)

        DeleteTask id ->
          let newModel = { model | tasks = List.filter (\t -> t.id /= id) model.tasks }
          in (newModel, viewModel)

        DeleteTaskComplete ->
          let newModel = { model | tasks = List.filter (not << .completed) model.tasks }
          in (newModel, viewModel)

        CheckTask id isCompleted ->
          let updateTask t = if t.id == id then { t | completed = isCompleted } else t
          in
            ( { model | tasks = List.map updateTask model.tasks }
            , viewModel
            )

        CheckAllTask isCompleted ->
          let updateTask t = { t | completed = isCompleted }
          in
            ({ model | tasks = List.map updateTask model.tasks }, viewModel)

        ChangeVisibilityTask visibility ->
          (model, { viewModel | taskVisibility = visibility })
