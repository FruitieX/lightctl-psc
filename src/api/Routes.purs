module Routes where

import Prelude

import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Handlers (getStateHandler, registerLuminaireHandler, setLightHandler)
import Luminaire (Luminaires, LuminaireChangeBus)
import Node.Express.App (App, get, post, put)

registerRoutes
  :: forall e
   . Luminaires
  -> LuminaireChangeBus
  -> App (avar :: AVAR, ref :: REF, now :: NOW, console :: CONSOLE | e)
registerRoutes state bus = do
  get  "/luminaires"                     (getStateHandler          state)
  put  "/luminaires/:id"                 (registerLuminaireHandler state bus)
  post "/luminaires/:id/lights/:lightId" (setLightHandler          state bus)
