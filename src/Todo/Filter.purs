module Todo.Filter where

import Prelude

data Filter = All | Active | Completed

derive instance eqFilter :: Eq Filter

instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

predicate :: ∀ a. Filter -> ({ completed :: Boolean | a } -> Boolean)
predicate All = const true
predicate Active = not <<< _.completed
predicate Completed = _.completed
