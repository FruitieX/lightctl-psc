module Handlers where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF, readRef)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Request (getBodyParam, getRouteParam)
import Node.Express.Response (sendJson)
import State (AppState, registerLuminaire)

getStateHandler :: forall e. AppState -> Handler (ref :: REF | e)
getStateHandler state = do
  curState <- liftEff $ readRef state
  sendJson curState

registerLuminaireHandler :: forall e. AppState -> Handler (ref :: REF | e)
registerLuminaireHandler state = do
  idParam <- getRouteParam "id"
  gatewayParam <- getBodyParam "gateway"

  case [idParam, gatewayParam] of
    [Just id, Just gatewayId] -> do
      liftEff $ registerLuminaire (wrap id) { gateway: (wrap gatewayId), lights: [] } state
      sendJson { status: "Luminaire registered" }
    _ -> nextThrow $ error "Luminaire and gateway IDs are required"
