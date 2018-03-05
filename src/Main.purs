module Main where

import Prelude hiding (apply)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Luminaire (Luminaires, initState)
import Node.Express.App (App, listenHttp, useExternal)
import Node.Express.Types (EXPRESS, Request, Response, ExpressM)
import Node.Process (PROCESS, lookupEnv)
import Routes (registerRoutes)
import WebSocket.Ws (WS)
import Ws (initWs)

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

appSetup :: forall e. Luminaires -> App (ref :: REF, now :: NOW, console :: CONSOLE | e)
appSetup state = do
  liftEff $ log "Setting up"
  useExternal jsonBodyParser
  registerRoutes state

main
  :: forall e
   . Eff (ref :: REF, now :: NOW, express :: EXPRESS, console :: CONSOLE, process :: PROCESS, ws :: WS | e)
     Unit
main = do
  state <- initState
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  server <- listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port
  initWs server state
  pure unit
