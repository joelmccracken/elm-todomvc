module EF.Project where

import Signal exposing (Signal, Address)
import Html exposing (Html, Attribute, text, header, h1, input, a, li, button, ul, strong, span, footer, label, div, section)
import Html.Attributes exposing (id, placeholder, autofocus, value, name, href, classList, hidden, class, type', checked, style, for)
import Html.Events exposing (on, targetValue, keyCode, onClick, onDoubleClick, onBlur)
import Json.Decode
import String
import Html.Lazy exposing (lazy, lazy2, lazy3)

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.Decode.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


type alias Project =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


type alias Collection =
   { projects: List Project
   , newProject: String
   , uid : Int
   , visibility : String
   }


type Update
    = UpdateNewProject String
    | EditingProject Int Bool
    | UpdateProject Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


newProject : String -> Int -> Project
newProject desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


emptyProjectCollection : Collection
emptyProjectCollection =
  { projects = []
  , newProject = ""
  , uid = 0
  , visibility = ""
  }


newProjectEntry : Address Update -> String -> Html
newProjectEntry address project =
    header
      [ id "header" ]
      [ h1 [] [ text "todos" ]
      , input
          [ id "new-todo"
          , placeholder "What needs to be done?"
          , autofocus True
          , value project
          , name "newTodo"
          , on "input" targetValue (Signal.message address << (UpdateNewProject))
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
              newProject = "",
              projects =
              if String.isEmpty model.newProject
              then model.projects
              else model.projects ++ [newProject model.newProject model.uid]
          }
        UpdateNewProject str ->
            { model | newProject = str }

        EditingProject id isEditing ->
            let updateProject t = if t.id == id then { t | editing = isEditing } else t
              in
                { model | projects = List.map updateProject model.projects }

        UpdateProject id project ->
            let updateProject t = if t.id == id then { t | description = project } else t
            in
                { model | projects = List.map updateProject model.projects }

        Delete id ->
            { model | projects = List.filter (\t -> t.id /= id) model.projects }

        DeleteComplete ->
            { model | projects = List.filter (not << .completed) model.projects }

        Check id isCompleted ->
            let updateProject t = if t.id == id then { t | completed = isCompleted } else t
            in
                { model | projects = List.map updateProject model.projects }

        CheckAll isCompleted ->
            let updateProject t = { t | completed = isCompleted }
            in
                { model | projects = List.map updateProject model.projects }

        ChangeVisibility visibility ->
            { model | visibility = visibility }


controls : Address Update -> String -> List Project -> Html
controls address visibility projects =
    let projectsCompleted = List.length (List.filter .completed projects)
        projectsLeft = List.length projects - projectsCompleted
        item_ = if projectsLeft == 1 then " item" else " items"
    in
    footer
      [ id "footer"
      , hidden (List.isEmpty projects)
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (toString projectsLeft) ]
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
          , hidden (projectsCompleted == 0)
          , onClick address (DeleteComplete)
          ]
          [ text ("Clear completed (" ++ toString projectsCompleted ++ ")") ]
      ]


visibilitySwap : Address Update -> String -> String -> String -> Html
visibilitySwap address uri visibility actualVisibility =
    li
      [ onClick address ((ChangeVisibility visibility)) ]
      [ a [ href uri, classList [("selected", visibility == actualVisibility)] ] [ text visibility ] ]


item : Address Update -> Project -> Html
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
              [ onDoubleClick address ((EditingProject todo.id True)) ]
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
          , on "input" targetValue (Signal.message address << (UpdateProject todo.id))
          , onBlur address ((EditingProject todo.id False))
          , onEnter address ((EditingProject todo.id False))
          ]
          []
      ]


list : Address Update -> String -> List Project -> Html
list address visibility projects =
    let isVisible todo =
            case visibility of
              "Completed" -> todo.completed
              "Active" -> not todo.completed
              _ -> True

        allCompleted = List.all .completed projects

        cssVisibility = if List.isEmpty projects then "hidden" else "visible"
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
          (List.map (item address) (List.filter isVisible projects))
      ]


view : Address Update -> Collection -> Html
view address model =
  section
    [ id "todoapp" ]
    [ lazy2 newProjectEntry address model.newProject
    , lazy3 list address model.visibility model.projects
    , lazy3 controls address model.visibility model.projects
    ]
