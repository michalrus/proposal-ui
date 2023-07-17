{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module PrometheusUI (spawnPrometheusUI) where

import           Brick (EventM, Widget, BrickEvent(VtyEvent), str)
import           Types (Name, Dialog(Dialog,dRender,dHandleEvent), DialogReply(DialogReplyContinue), AppState, CustomEvent)
import qualified Graphics.Vty as V

import           Control.Monad.IO.Class                         (liftIO)
import           System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import           System.Metrics.Prometheus.Concurrent.RegistryT (runRegistryT, registerCounter, RegistryT(RegistryT, unRegistryT))
import           System.Metrics.Prometheus.Metric.Counter       (inc, Counter)
import           System.Metrics.Prometheus.MetricId (fromList)

import           Control.Concurrent.Async (Async, async, cancel)
import           Control.Monad.Reader (ask, runReaderT)

data PrometheusState = PrometheusState
  { psCounter :: Counter
  , psState :: Async ()
  , psCallback :: EventM Name DialogReply
  , msg :: String
  }

mkPrometheusUI :: PrometheusState -> Dialog
mkPrometheusUI state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

spawnPrometheusUI :: EventM Name DialogReply -> EventM Name Dialog
spawnPrometheusUI callback = do
  (counter, server) <- liftIO $ startServer
  let
    state = PrometheusState counter server callback ""
  pure $ mkPrometheusUI state

startServer :: IO (Counter, Async ())
startServer = do
  runRegistryT $ do
    connectCounter <- registerCounter "brick_events" (fromList [("key","value")])
    registry <- RegistryT ask
    server <- liftIO $ async $ runReaderT (unRegistryT $ serveMetricsT 8080 []) registry
    pure (connectCounter, server)

renderUI :: PrometheusState -> AppState -> [ Widget Name ]
renderUI PrometheusState{msg} _ = [ str msg ]

handleEvents :: PrometheusState -> AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
handleEvents pstate@PrometheusState{psCallback,psState,psCounter} _ event = do
  liftIO $ inc psCounter
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      liftIO $ cancel psState
      psCallback
    e -> do
      pure $ DialogReplyContinue $ mkPrometheusUI $ pstate { msg = show e }
