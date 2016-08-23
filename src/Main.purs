module Main where

import Prelude

import App.Routes (match)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Pux (App, CoreEffects, Config)
import Pux (start, renderToDOM) as Pux
import Pux.Devtool (start, Action) as Pux.Devtool
import Pux.Router (sampleUrl)
import Todo.TodoList (Action(ChangeFilter), Model, init, update, view)

initialState :: Model
initialState = init

config :: ∀ eff. Model -> Eff (dom :: DOM | eff) (Config Model Action (dom :: DOM))
config state = do
  urlSignal <- sampleUrl
  let routeSignal = map (ChangeFilter <<< match) urlSignal
  pure
    { initialState: state
    , update: update
    , view: view
    , inputs: [routeSignal]
    }

main :: Model -> Eff (CoreEffects (dom :: DOM)) (App Model Action)
main state = do
  app <- Pux.start =<< config state
  Pux.renderToDOM "#app" app.html
  pure app

debug :: Model -> Eff (CoreEffects (dom :: DOM)) (App Model (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  Pux.renderToDOM "#app" app.html
  pure app
