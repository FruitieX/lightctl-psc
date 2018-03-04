module Luminaire (
  LuminaireId(..)
, GatewayId(..)
, LuminaireState
) where

import Data.Newtype (class Newtype)
import Light (LightState)
import Prelude (class Eq, class Ord)

-- Type definitions
newtype LuminaireId = LuminaireId String
derive instance eqLuminaireId :: Eq LuminaireId
derive instance ordLuminaireId :: Ord LuminaireId
derive instance newtypeLuminaireId :: Newtype LuminaireId _

newtype GatewayId = GatewayId String
derive instance newtypeGatewayId :: Newtype GatewayId _

--- Model type definitions
type LuminaireState =
  { gateway :: GatewayId
  , lights :: Array LightState
  }
