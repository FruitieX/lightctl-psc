module Handlers where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, readRef)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Foreign (Foreign)
import Data.Map (empty, insert, showTree)
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Data.Number (fromString)
import Data.StrMap (StrMap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Light (Light(..), LightColor, LightId(..), Lights)
import Luminaire (GatewayId(..), Luminaire(..), LuminaireId(..), Luminaires, registerLuminaire, setLight)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Request (getBodyParam, getRouteParam)
import Node.Express.Response (sendJson)
import Simple.JSON (read)
import Utils (getBody)

getStateHandler :: forall e. Luminaires -> Handler (ref :: REF | e)
getStateHandler state = do
  curState <- liftEff $ readRef state
  sendJson (showTree curState)

--whateverJson :: String ->
type IntermediateLight =
  { color :: LightColor
  , prevColor :: LightColor
  , transitionStart :: Number
  , transitionTime :: Number
  }
type IntermediateLightStrMap = StrMap IntermediateLight
type IntermediateBody =
  { gateway :: String
  , lights :: IntermediateLightStrMap
  }

-- WTF instant creates a Maybe Instant
toLight
  :: IntermediateLightStrMap
  -> Lights
toLight =
  foldlWithIndex (\k m v -> do
    let transitionStart = Milliseconds v.transitionStart
    let transitionTime = Milliseconds v.transitionTime
    insert (LightId k) (Light v { transitionStart = transitionStart, transitionTime = transitionTime }) m
  ) empty

registerLuminaireHandler
  :: forall e
   . Luminaires
  -> Handler (ref :: REF | e)
registerLuminaireHandler state = do
  idParam <- getRouteParam "id"
  gatewayParam :: Maybe String <- getBodyParam "gateway"
  --body :: Maybe String <- hush <$> runExcept <<< readString <$> getBody'
  body :: Foreign <- getBody
  let (json :: Maybe IntermediateBody) = hush <$> runExcept $ read body

  let lights = toLight <$> _.lights <$> json

  case idParam /\ lights /\ gatewayParam of
    Just id /\ Just lights' /\ Just gateway -> do
      liftEff $ registerLuminaire (LuminaireId id) (Luminaire { gateway: GatewayId gateway, lights: lights' }) state
      sendJson { status: "Luminaire registered" }
    _ -> do
      nextThrow $ error "Missing parameters"

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
