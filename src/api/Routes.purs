module Routes where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Handlers (getStateHandler, registerLuminaireHandler, setLightHandler)
import Luminaire (Luminaires)
import Node.Express.App (App, get, post, put)

registerRoutes :: forall e. Luminaires -> App (ref :: REF, now :: NOW, console :: CONSOLE | e)
registerRoutes state = do
  get  "/luminaires"                     (getStateHandler          state)
  put  "/luminaires/:id"                 (registerLuminaireHandler state)
  post "/luminaires/:id/lights/:lightId" (setLightHandler          state)
