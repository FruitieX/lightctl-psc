module Light where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (unInstant)
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Simple.JSON (class ReadForeign)

-- Type definitions
newtype Hue = Hue Number
derive newtype instance rfHue :: ReadForeign Hue
instance showHue :: Show Hue where
  show (Hue hue) = show hue

newtype Saturation = Saturation Number
derive newtype instance rfSaturation :: ReadForeign Saturation
instance showSaturation :: Show Saturation where
  show (Saturation saturation) = show saturation

newtype Value = Value Number
derive newtype instance rfValue :: ReadForeign Value
instance showValue :: Show Value where
  show (Value value) = show value

newtype LightColor = LightColor
  { hue :: Hue
  , saturation :: Saturation
  , value :: Value
  }
derive newtype instance rfLightColor :: ReadForeign LightColor
instance showLightColor :: Show LightColor where
  show (LightColor color) =
    "HSV:" <> show color.hue <> show color.saturation <> show color.value

newtype LightId = LightId String
instance showLightId :: Show LightId where
  show (LightId id) = show id

derive instance eqLightId :: Eq LightId
derive instance ordLightId :: Ord LightId
derive instance newtypeLightId :: Newtype LightId _

--- Model type definitions
newtype Light = Light
  { color :: LightColor
  , prevColor :: LightColor
  --, transitionStart :: Instant
  , transitionStart :: Milliseconds
  , transitionTime :: Milliseconds
  }
derive instance newtypeLight :: Newtype Light _

type Lights = Map LightId Light

instance showLight :: Show Light where
  show (Light state) =
    "Color:" <> show state.color <> ", Prev color: " <> show state.prevColor

nextState
  :: forall e
   . LightColor
  -> (Maybe Milliseconds)
  -> Light
  -> Eff (now :: NOW | e) Light
nextState nextColor transitionTime light = do
  transitionStart <- unInstant <$> now

  pure $ wrap
    { color: nextColor
    , prevColor: _.color (unwrap light)
    , transitionStart: transitionStart
    , transitionTime: fromMaybe (Milliseconds 500.0) transitionTime
    }
