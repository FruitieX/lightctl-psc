module Light (
  LightState
, LightId
, LightColor
) where

import Data.DateTime.Instant (Instant)
import Data.Time.Duration (Milliseconds)

-- Type definitions
newtype Hue = Hue Number
newtype Saturation = Saturation Number
newtype Value = Value Number

data LightColor = LightColor
  { hue :: Hue
  , saturation :: Saturation
  , value :: Value
  }

newtype LightId = LightId String

--- Model type definitions
type LightState =
  { color :: LightColor
  , prevColor :: LightColor
  , transitionStart :: Instant
  , transitionTime :: Milliseconds
  }
