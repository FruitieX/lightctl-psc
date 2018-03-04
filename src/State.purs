module State (
  AppState
, initState
, registerLuminaire
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef)
import Data.Map (Map, empty, insert)
import Luminaire (LuminaireId, LuminaireState)

-- Global state data
type AppState = Ref (Map LuminaireId LuminaireState)

initState :: forall e. Eff (ref :: REF | e) AppState
initState = newRef empty

-- TODO: check if LuminaireId is already a member
registerLuminaire :: forall e. LuminaireId -> LuminaireState -> AppState -> Eff (ref :: REF | e) Unit
registerLuminaire id luminaire state = do
  --curState <- liftEff $ readRef state
  modifyRef state (\luminaires -> insert id luminaire luminaires)
