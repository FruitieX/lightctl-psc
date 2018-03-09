module Luminaire where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus (BusRW, write)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Map (Map, delete, empty, insert, lookup, member)
import Data.Maybe (Maybe, isJust)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse)
import Global.Unsafe (unsafeStringify)
import Light (Light, LightColor, LightId, Lights, nextState)

-- Type definitions
newtype LuminaireId = LuminaireId String
instance showLuminaireId :: Show LuminaireId where
  show (LuminaireId id) = show id

derive instance eqLuminaireId :: Eq LuminaireId
derive instance ordLuminaireId :: Ord LuminaireId
derive instance newtypeLuminaireId :: Newtype LuminaireId _

newtype GatewayId = GatewayId String
derive instance newtypeGatewayId :: Newtype GatewayId _
instance showGatewayId :: Show GatewayId where
  show (GatewayId id) = show id

type LuminaireChangeBus = BusRW LuminaireChange
type LuminaireChange =
  { id :: LuminaireId
  , lights :: Lights
  }

--- Model type definitions
newtype Luminaire = Luminaire
  { gateway :: GatewayId
  , lights :: Lights
  }
derive instance newtypeLuminaire :: Newtype Luminaire _

instance showLuminaire :: Show Luminaire where
  show (Luminaire state) = "Gateway ID:" <> show state.gateway <> ", lights: " <> show state.lights

type Luminaires = Ref (Map LuminaireId Luminaire)

initState :: forall e. Eff (ref :: REF | e) Luminaires
initState = newRef empty

registerLuminaire
  :: forall e
   . LuminaireId
  -> Luminaire
  -> Luminaires
  -> Eff (ref :: REF | e) Unit
registerLuminaire id luminaire luminaires = do
  modifyRef luminaires
    (\l -> insert id luminaire l)

deregisterLuminaire
  :: forall e
   . LuminaireId
  -> Luminaire
  -> Luminaires
  -> Eff (ref :: REF | e) Boolean
deregisterLuminaire id luminaire luminaires = do
  let isMember = member id <$> readRef luminaires
  modifyRef luminaires
    (\l -> delete id l)
  isMember

getLuminaire
  :: forall e
   . LuminaireId
  -> Luminaires
  -> Eff (ref :: REF | e) (Maybe Luminaire)
getLuminaire id luminaires = do
  lookup id <$> readRef luminaires

getLuminaireLight
  :: forall e
   . LuminaireId
  -> LightId
  -> Luminaires
  -> Eff (ref :: REF | e) (Maybe Light)
getLuminaireLight id lid luminaires = do
  luminaire <- getLuminaire id luminaires
  let lights = _.lights <<< unwrap <$> luminaire
  let light = lights >>= lookup lid
  pure light

setLight
  :: forall e
   . LuminaireId
  -> LightId
  -> LightColor
  -> (Maybe Milliseconds)
  -> Luminaires
  -> LuminaireChangeBus
  -> Aff (avar :: AVAR, ref :: REF, now :: NOW | e) Boolean
setLight id lid color transitionTime luminaires bus = do
  light <- liftEff $ getLuminaireLight id lid luminaires
  luminaire <- liftEff $ getLuminaire id luminaires

  nextLight <- liftEff $ traverse (nextState color transitionTime) light

  let curLights = _.lights <<< unwrap <$> luminaire
  let (nextLights :: Maybe (Map LightId Light)) = insert lid <$> nextLight <*> curLights
  let nextLuminaire = nextLuminaire' <$> nextLights <*> luminaire

  _ <- traverse write' $ { id: id, lights: _ } <$> nextLights

  isJust <$> traverse (updateLuminaire luminaires) nextLuminaire

  where
    nextLuminaire' lights = over Luminaire (_ { lights = lights })
    updateLuminaire luminaires nextLuminaire =
      liftEff $ modifyRef luminaires
        (\l -> insert id nextLuminaire l)
    write' = (\change -> write change bus)
