module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Bus (make)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Luminaire (LuminaireChangeBus, Luminaires, initState)
import Node.Express.App (App, listenHttp, useExternal)
import Node.Express.Types (EXPRESS, Request, Response, ExpressM)
import Node.Process (PROCESS, lookupEnv)
import Routes (registerRoutes)
import WebSocket.Ws (WS)
import Ws (initWs)

parseInt
  :: String
  -> Int
parseInt str = fromMaybe 0 $ fromString str

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

expressSetup
  :: forall e
   . Luminaires
  -> LuminaireChangeBus
  -> App (avar :: AVAR, ref :: REF, now :: NOW, console :: CONSOLE | e)
expressSetup state bus = do
  liftEff $ log "Setting up"
  useExternal jsonBodyParser
  registerRoutes state bus

appSetup
  :: forall e
   . Aff (ref :: REF, avar :: AVAR, now :: NOW, express :: EXPRESS, console :: CONSOLE, process :: PROCESS, ws :: WS | e) Unit
appSetup = do
  bus <- make
  state <- liftEff $ initState
  port <- liftEff $ (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  server <- liftEff $ listenHttp (expressSetup state bus) port \_ ->
    log $ "Listening on " <> show port
  initWs server state bus

main
  :: forall e
   . Eff (ref :: REF, avar :: AVAR, now :: NOW, express :: EXPRESS, console :: CONSOLE, process :: PROCESS, ws :: WS | e)
     Unit
main = do
  launchAff_ appSetup
  pure unit
