module Handlers where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, readRef)
import Control.Monad.Except (runExcept)
import Data.Tuple.Nested ((/\))
import Data.Either (hush)
import Data.Map (empty, showTree)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Number (fromString)
import Data.Time.Duration (Milliseconds(..))
import Luminaire (Luminaires, registerLuminaire, setLight)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Request (getBodyParam, getRouteParam)
import Node.Express.Response (sendJson)
import Simple.JSON (read)

getStateHandler :: forall e. Luminaires -> Handler (ref :: REF | e)
getStateHandler state = do
  curState <- liftEff $ readRef state
  sendJson (showTree curState)

registerLuminaireHandler :: forall e. Luminaires -> Handler (ref :: REF | e)
registerLuminaireHandler state = do
  idParam <- getRouteParam "id"
  gatewayParam <- getBodyParam "gateway"
  lightsParam <- getBodyParam "lights"

  case [idParam, gatewayParam, lightsParam] of
    [Just id, Just gatewayId, Just lightsParam] -> do

      --let lights :: Either MultipleErrors Lights
          --lights = readJSON lightsParam

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
  colorParam <- (=<<) (\x -> hush $ read' x) <$> getBodyParam "color"
  transitionTimeParam <- getBodyParam "transitionTime"

  case idParam /\ lightIdParam /\ transitionTimeParam /\ colorParam of
    Just id /\ Just lightId /\ Just transitionTimeString /\ Just color' -> do

      let transitionTime = fromString transitionTimeString

      success <- liftEff $ setLight (wrap id) (wrap lightId) color' (Milliseconds <$> transitionTime) state
      sendJson { status: "Luminaire registered" }
    _ -> nextThrow $ error "Missing parameters"

  where
    read' = runExcept <<< read
