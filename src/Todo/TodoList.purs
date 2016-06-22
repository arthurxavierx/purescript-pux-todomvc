module Todo.TodoList where

import Prelude

import Data.Array (null, filter, length, mapMaybe, (:))
import Data.Foldable (all)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Pux.Html (Attribute, button, li, Html, text, strong, span, footer, ul, label, input, section, h1, header)
import Pux.Html.Attributes (style, className, htmlFor, checked, type_, autoFocus, value, placeholder)
import Pux.Html.Events (onInput, onClick, onKeyDown, onChange)
import Pux.Router (link)
import Todo.Todo as Todo
import Todo.Filter (Filter(..), predicate)

data Action
  = Nop
  | Insert String
  | Update Int Todo.Action
  | CheckAll Boolean
  | ClearCompleted
  | ChangeFilter Filter
  | EditField String

type Model =
  { field :: String
  , tasks :: Array Todo.Model
  , uid :: Int
  , filter :: Filter }

init :: Model
init = { field: "", tasks: [], uid: 0, filter: All }

update :: Action -> Model -> Model
update action model =
  case action of
    Nop ->
      model

    Insert description ->
      model {
        field = ""
      , tasks = (Todo.init description model.uid) : model.tasks
      , uid = model.uid+1 }

    Update id taskAction -> do
      model { tasks = mapMaybe (updateTask taskAction id) model.tasks }

    CheckAll check ->
      model { tasks = map (_ { completed = check }) model.tasks }

    ClearCompleted ->
      model { tasks = filter (not _.completed) model.tasks }

    ChangeFilter filter ->
      model { filter = filter }

    EditField field ->
      model { field = field }
  where
    updateTask :: Todo.Action -> Int -> Todo.Model -> Maybe Todo.Model
    updateTask taskAction id task =
      if task.id /= id
      then Just task
      else Todo.update taskAction task

view :: Model -> Html Action
view model =
  section [className "todoapp"]
    [ headerView model.field
    , listView model.tasks model.filter
    , footerView model.tasks model.filter
    ]

headerView :: String -> Html Action
headerView field =
  header [className "header"]
    [ h1 [] [text "todos"]
    , input
      [ className "new-todo"
      , placeholder "What needs to be done?"
      , value field
      , autoFocus true
      , onInput (\e -> EditField e.target.value)
      , onKeyDown keyDown
      ] []
    ]
  where
    keyDown event
      | event.keyCode == 13 = Insert event.target.value
      | otherwise = Nop

listView :: Array Todo.Model -> Filter -> Html Action
listView tasks taskFilter =
  section
    [ className "main"
    , style [Tuple "display" (if null tasks then "none" else "inherit")]
    ]
    -- toggle all checkbox
    [ input
      [ className "toggle-all"
      , type_ "checkbox"
      , checked allCompleted
      , onChange (const $ CheckAll (not allCompleted))
      ] []
    , label [htmlFor "toggle-all"] [text "Mark all as complete"]
    -- filtered tasks
    , ul [className "todo-list"] (map (\t -> map (Update t.id) $ Todo.view t) filteredTasks)
    ]
  where
    allCompleted = all _.completed tasks
    filteredTasks = filter (predicate taskFilter) tasks

footerView :: Array Todo.Model -> Filter -> Html Action
footerView tasks taskFilter =
  footer
    [ className "footer"
    , style [Tuple "display" (if null tasks then "none" else "inherit")]
    ]
    -- completed tasks
    [ span [className "todo-count"]
      [ strong [] [text $ show completedTasks]
      , text (if completedTasks == 1 then " item left" else " item left")
      ]
    -- tasks filter
    , ul [className "filters"]
      [ li [] [ link "/" (isSelected All) [text "All"] ]
      , li [] [ link "/active" (isSelected Active) [text "Active"] ]
      , li [] [ link "/completed" (isSelected Completed) [text "Completed"] ]
      ]
    -- clear completed button
    , button
      [ className "clear-completed"
      , style [Tuple "display" (if completedTasks == 0 then "none" else "inherit")]
      , onClick (const $ ClearCompleted)
      ]
      [text "Clear completed"]
    ]
  where
    completedTasks = length $ filter (predicate Completed) tasks
    isSelected :: ∀ a. Filter -> Array (Attribute a)
    isSelected f = if taskFilter == f then [className "selected"] else []
