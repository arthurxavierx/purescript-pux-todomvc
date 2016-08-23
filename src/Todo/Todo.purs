module Todo.Todo where

import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Maybe (isNothing, isJust, fromMaybe, Maybe(..))
import Data.String (null, trim)
import Pux (EffModel, noEffects)
import Pux.Html (Html, input, button, text, label, div, li)
import Pux.Html.Attributes (id_, name, checked, type_, value, className)
import Pux.Html.Events (onKeyDown, onDoubleClick, onClick, onBlur, onChange)
import Prelude hiding (div)

foreign import focusTodo :: ∀ e. Int -> Eff (dom :: DOM | e) Unit

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

update :: Action -> Model -> EffModel (Maybe Model) Action (dom :: DOM)
update action todo =
  case action of
    Nop ->
      noEffects $ Just todo

    Focus ->
      { state: Just todo { edits = Just todo.description }
      , effects: [(liftEff $ focusTodo todo.id) *> pure Nop] }

    Cancel ->
      noEffects $ Just todo { edits = Nothing }

    Commit ->
      if isNothing todo.edits
      then noEffects $ Just todo
      else noEffects $ do
        description <- todo.edits
        guard $ not $ null (trim description)
        pure $ todo { description = description, edits = Nothing }

    Edit edit ->
      noEffects $ Just todo { edits = Just edit }

    Check check ->
      noEffects $ Just todo { completed = check }

    Delete ->
      noEffects $ Nothing

view :: Model -> Html Action
view todo =
  li [ className classes ]
    -- todo view
    [ div [className "view"]
      -- toggle checkbox
      [ input
        [ className "toggle"
        , type_ "checkbox"
        , checked todo.completed
        , onClick (const $ Check (not todo.completed))
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
      , id_ ("todo-" <> show todo.id)
      , onChange (\event -> Edit event.target.value)
      , onBlur (const Commit)
      , onKeyDown keyDown
      ] []
    ]
  where
    classes = (if todo.completed then "completed " else "") <> (if isJust todo.edits then "editing" else "")
    description = fromMaybe todo.description todo.edits
    keyDown event
      | event.keyCode == 13 = Commit
      | event.keyCode == 27 = Cancel
      | otherwise = Nop
