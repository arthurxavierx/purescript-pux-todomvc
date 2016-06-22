module Todo.Todo where

import Prelude hiding (div)

import Control.MonadPlus (guard)
import Data.Maybe (isNothing, isJust, fromMaybe, Maybe(..))
import Data.String (null, trim)
import Pux.Html (Html, input, button, text, label, div, li)
import Pux.Html.Attributes (id_, name, checked, type_, value, className)
import Pux.Html.Events (onKeyDown, onDoubleClick, onClick, onBlur, onChange)

data Action
  = Nop
  | Focus
  | Cancel
  | Commit
  | Edit String
  | Check Boolean
  | Delete

type Model =
  { id :: Int
  , description :: String
  , completed :: Boolean
  , edits :: Maybe String
  }

init :: String -> Int -> Model
init description id = { id: id, description: description, completed: false, edits: Nothing }

update :: Action -> Model -> Maybe Model
update action task =
  case action of
    Nop ->
      Just task

    Focus ->
      Just task{ edits = Just task.description }

    Cancel ->
      Just task{ edits = Nothing }

    Commit ->
      if isNothing task.edits
      then Just task
      else do
        description <- task.edits
        guard $ not $ null (trim description)
        return task{ description = description, edits = Nothing }

    Edit edit ->
      Just task{ edits = Just edit }

    Check check ->
      Just task{ completed = check }

    Delete ->
      Nothing

view :: Model -> Html Action
view task =
  li [ className classes ]
    -- todo view
    [ div [className "view"]
      -- toggle checkbox
      [ input
        [ className "toggle"
        , type_ "checkbox"
        , checked task.completed
        , onClick (const $ Check (not task.completed))
        ] []
      -- description
      , label
        [ onDoubleClick (const $ Focus) ]
        [ text description ]
      -- destroy button
      , button [ className "destroy", onClick (const $ Delete) ] []
      ]
    -- input field
    , input
      [ className "edit"
      , value description
      , name "title"
      , id_ ("todo-" ++ show task.id)
      , onChange (\event -> Edit event.target.value)
      , onBlur (const Commit)
      , onKeyDown keyDown
      ] []
    ]
  where
    classes = (if task.completed then "completed " else "") ++ (if isJust task.edits then "editing" else "")
    description = fromMaybe task.description task.edits
    keyDown event
      | event.keyCode == 13 = Commit
      | event.keyCode == 27 = Cancel
      | otherwise = Nop
