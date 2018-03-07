module Utils where

import Prelude
import Data.Foreign (Foreign)
import Node.Express.Handler (HandlerM(..))
import Node.Express.Types (Request)

foreign import _getBody :: Request -> Foreign

getBody
  :: forall e
   . HandlerM e Foreign
getBody = HandlerM \req _ _ -> pure $ _getBody req
