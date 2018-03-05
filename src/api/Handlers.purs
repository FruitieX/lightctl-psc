module Handlers where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, readRef)
import Data.Either (Either(Right, Left))
import Data.Foreign (MultipleErrors)
import Data.Map (empty, showTree)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Number (fromString)
import Data.Time.Duration (Milliseconds(..))
import Light (LightColor)
import Luminaire (Luminaires, registerLuminaire, setLight)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Request (getBodyParam, getRouteParam)
import Node.Express.Response (sendJson)
import Simple.JSON (readJSON)

getStateHandler :: forall e. Luminaires -> Handler (ref :: REF | e)
getStateHandler state = do
  curState <- liftEff $ readRef state
  sendJson (showTree curState)

registerLuminaireHandler :: forall e. Luminaires -> Handler (ref :: REF | e)
registerLuminaireHandler state = do
  idParam <- getRouteParam "id"
  gatewayParam <- getBodyParam "gateway"

  case [idParam, gatewayParam] of
    [Just id, Just gatewayId] -> do
      liftEff $ registerLuminaire (wrap id) (wrap { gateway: (wrap gatewayId), lights: empty }) state
      sendJson { status: "Luminaire registered" }
    _ -> nextThrow $ error "Luminaire and gateway IDs are required"

setLightHandler
  :: forall e
   . Luminaires
  -> Handler (ref :: REF, now :: NOW | e)
setLightHandler state = do
  idParam <- getRouteParam "id"
  lightIdParam <- getRouteParam "lightId"
  colorParam <- getBodyParam "color"
  transitionTimeParam <- getBodyParam "transitionTime"

  case [idParam, lightIdParam, colorParam, transitionTimeParam] of
    [Just id, Just lightId, Just colorString, Just transitionTimeString] -> do

      let color' :: Either MultipleErrors LightColor
          color' = readJSON colorString

      case color' of
        Left errors -> nextThrow $ error (show errors)
        Right color -> do
          let transitionTime = fromString transitionTimeString

          success <- liftEff $ setLight (wrap id) (wrap lightId) color (Milliseconds <$> transitionTime) state
          sendJson { status: "Luminaire registered" }
    _ -> nextThrow $ error "Missing parameters"
