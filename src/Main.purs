module Main where

import Prelude hiding (apply)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF, readRef)
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Node.Express.App (App, get, listenHttp, put, useExternal)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Request (getBodyParam, getRouteParam)
import Node.Express.Response (sendJson)
import Node.Express.Types (EXPRESS, Request, Response, ExpressM)
import Node.Process (PROCESS, lookupEnv)
import Routes (registerRoutes)
import State (AppState, initState, registerLuminaire)
import WebSocket.Ws (WS)
import Ws (initWs)

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

appSetup :: forall e. AppState -> App (ref :: REF, console :: CONSOLE | e)
appSetup state = do
  liftEff $ log "Setting up"
  useExternal jsonBodyParser
  registerRoutes state

main
  :: forall e
   . Eff (ref :: REF, express :: EXPRESS, console :: CONSOLE, process :: PROCESS, ws :: WS | e)
     Unit
main = do
  state <- initState
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  server <- listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port
  initWs server state
  pure unit
