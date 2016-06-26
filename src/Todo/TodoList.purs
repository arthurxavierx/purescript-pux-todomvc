module Todo.TodoList where

import Prelude

import Todo.Todo as Todo
import DOM (DOM)
import Data.Array (concatMap, mapMaybe, null, filter, length, (:))
import Data.Foldable (all)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Pux (mapEffects, EffModel, noEffects)
import Pux.Html (Attribute, button, li, Html, text, strong, span, footer, ul, label, input, section, h1, header)
import Pux.Html.Attributes (style, className, htmlFor, checked, type_, autoFocus, value, placeholder)
import Pux.Html.Events (onInput, onClick, onKeyDown, onChange)
import Pux.Router (link)
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

update :: Action -> Model -> EffModel Model Action (dom :: DOM)
update action model =
  case action of
    Nop ->
      noEffects $ model

    Insert description ->
      noEffects $
        model {
          field = ""
        , tasks = (Todo.init description model.uid) : model.tasks
        , uid = model.uid+1 }

    Update id taskAction ->
      let updatedTasks = map (updateTask taskAction id) model.tasks
      in
        { state: model { tasks = mapMaybe _.state updatedTasks }
        , effects: concatMap (_.effects <<< mapEffects (Update id)) updatedTasks }

    CheckAll check ->
      noEffects $ model { tasks = map (_ { completed = check }) model.tasks }

    ClearCompleted ->
      noEffects $ model { tasks = filter (not _.completed) model.tasks }

    ChangeFilter filter ->
      noEffects $ model { filter = filter }

    EditField field ->
      noEffects $ model { field = field }
  where
    updateTask :: Todo.Action -> Int -> Todo.Model -> EffModel (Maybe Todo.Model) Todo.Action (dom :: DOM)
    updateTask taskAction id task =
      if task.id /= id
      then noEffects $ Just task
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
