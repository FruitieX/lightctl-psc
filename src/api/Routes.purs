module Routes where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Handlers (getStateHandler, registerLuminaireHandler, setLightHandler)
import Node.Express.App (App, get, post, put)
import State (AppState)

registerRoutes :: forall e. AppState -> App (ref :: REF, console :: CONSOLE | e)
registerRoutes state = do
  get  "/luminaires"                     (getStateHandler          state)
  put  "/luminaires/:id"                 (registerLuminaireHandler state)
  post "/luminaires/:id/lights/:lightId" (setLightHandler          state)
