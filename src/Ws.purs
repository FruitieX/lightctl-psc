module Ws where

import Prelude

import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Aff.Bus (read)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF)
import Data.Map (empty)
import Data.Newtype (wrap)
import Global.Unsafe (unsafeStringify)
import Luminaire (GatewayId(GatewayId), LuminaireId(LuminaireId), Luminaires, LuminaireChangeBus, registerLuminaire)
import Node.HTTP (Request, Server)
import WebSocket.Ws (WS, WebSocketConnection, WebSocketMessage(WebSocketMessage), createWebSocketServerWithServer, onConnection, onError, onMessage, onServerError, sendMessage)

handleMessage
  :: forall e
   . Luminaires
  -> WebSocketConnection
  -> WebSocketMessage
  -> Eff (ref :: REF, ws :: WS, console :: CONSOLE | e) Unit
handleMessage state ws msg = do
  registerLuminaire (LuminaireId "Test") (wrap { gateway: (GatewayId "ws"), lights: empty }) state
  log (show msg)

handleError :: forall e. Error -> Eff (ws :: WS, console :: CONSOLE | e) Unit
handleError err = do
  log $ show err

handleBusMessages
  :: forall e
   . Luminaires
  -> LuminaireChangeBus
  -> WebSocketConnection
  -> Aff (avar :: AVAR, console :: CONSOLE, ws :: WS | e) Unit
handleBusMessages state bus ws = do
  res <- attempt (read bus)
  liftEff $ sendMessage ws $ WebSocketMessage "update"
  liftEff $ log $ unsafeStringify res

handleConnection
  :: forall e
   . Luminaires
  -> LuminaireChangeBus
  -> WebSocketConnection
  -> Request
  -> Eff (avar :: AVAR, ref :: REF, ws :: WS, console :: CONSOLE | e) Unit
handleConnection state bus ws req = do
  log "Connected!"
  onMessage ws $ handleMessage state ws
  onError ws handleError
  sendMessage ws $ WebSocketMessage "Hello, world!"
  launchAff_ $ handleBusMessages state bus ws

initWs
  :: forall e
   . Server
  -> Luminaires
  -> LuminaireChangeBus
  -> Aff (avar :: AVAR, ref :: REF, ws :: WS, console :: CONSOLE | e) Unit
initWs server state bus = do
  wss <- liftEff $ createWebSocketServerWithServer server {}
  liftEff $ onConnection wss (handleConnection state bus)
  liftEff $ onServerError wss handleError
