module Luminaire (
  LuminaireId(..)
, GatewayId(..)
, LuminaireState
) where

import Data.Map (Map)
import Data.Newtype (class Newtype)
import Light (LightId, LightState)
import Prelude (class Eq, class Ord, class Show, show)

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

--- Model type definitions
type LuminaireState =
  { gateway :: GatewayId
  , lights :: Map LightId LightState
  }
