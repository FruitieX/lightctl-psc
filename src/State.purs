module State (
  AppState
, initState
, registerLuminaire
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Light (LightId, LightState)
import Luminaire (LuminaireId, LuminaireState)

-- Global state data
type AppState = Ref (Map LuminaireId LuminaireState)

initState :: forall e. Eff (ref :: REF | e) AppState
initState = newRef empty

-- TODO: check if LuminaireId is already a member
registerLuminaire :: forall e. LuminaireId -> LuminaireState -> AppState -> Eff (ref :: REF | e) Unit
registerLuminaire id luminaire appState = do
  --curState <- liftEff $ readRef state
  modifyRef appState (\luminaires -> insert id luminaire luminaires)

--setLight :: forall e. LuminaireId -> LightId -> LightState -> AppState -> Eff (ref :: REF | e) Boolean
--setLight luminaireId lightId lightState appState = do
  --curLuminaireStateMaybe <- lookup luminaireId (liftEff $ readRef appState)

  --case curLuminaireStateMaybe of
    --Just curLuminaireState ->
      --modifyRef appState (\luminaires -> insert luminaireId )
      --pure true
    --_ -> false
