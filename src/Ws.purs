module Ws where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF)
import Data.Map (empty)
import Data.Newtype (wrap)
import Luminaire (GatewayId(GatewayId), LuminaireId(LuminaireId), Luminaires, registerLuminaire)
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

handleConnection
  :: forall e
   . Luminaires
  -> WebSocketConnection
  -> Request
  -> Eff (ref :: REF, ws :: WS, console :: CONSOLE | e) Unit
handleConnection state ws req = do
  log "Connected!"
  onMessage ws $ handleMessage state ws
  onError ws handleError
  sendMessage ws $ WebSocketMessage "Hello, world!"

initWs
  :: forall e
   . Server
  -> Luminaires
  -> Eff (ref :: REF, ws :: WS, console :: CONSOLE | e) Unit
initWs server state = do
  wss <- createWebSocketServerWithServer server {}
  onConnection wss (handleConnection state)
  onServerError wss handleError
