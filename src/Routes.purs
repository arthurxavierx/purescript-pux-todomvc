module App.Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Prelude (($))
import Pux.Router (end, lit, router)
import Todo.Filter

match :: String -> Filter
match url = fromMaybe All $ router url $
  Active <$ (lit "active") <* end
  <|>
  Completed <$ (lit "completed") <* end
