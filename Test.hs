{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Brick (Widget, BrickEvent, EventM, Next, App(App, appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap), on, continue, showFirstCursor, customMain, halt, getVtyHandle, suspendAndResume)
import qualified Brick.AttrMap as A
import           Brick.BChan (BChan, newBChan)
import qualified Brick.Widgets.List as L
--import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import Brick.Forms

import Dialog1 (spawnDialog1)
import Types (Dialog(dRender, dHandleEvent), AppState(AppState, asDialogStack, asLastMsg), Name, CustomEvent, DialogReply(DialogReplyHalt, DialogReplyContinue, DialogReplyLiftIO))

data CurrentWindow = MainWindow deriving Eq

defaultState :: BChan () -> Dialog -> AppState
defaultState replyChan dlg = AppState replyChan "" dlg

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,            V.white `on` V.blue)
  , (L.listSelectedAttr,    V.blue `on` V.white)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  ]

drawUI :: AppState -> [ Widget Name ]
drawUI state = (dRender . asDialogStack) state state

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next (AppState))
handleEvent state@AppState{asDialogStack} event = do
  let
    thing :: IO Dialog -> IO AppState
    thing ioact = do
      dlg <- ioact
      pure $ state { asDialogStack = dlg }
    go :: DialogReply -> EventM Name (Next AppState)
    go reply = do
      case reply of
        DialogReplyHalt s -> halt s
        DialogReplyContinue dlg -> continue $ state { asDialogStack = dlg }
        DialogReplyLiftIO ioact -> suspendAndResume (thing ioact)
  newDlg <- (dHandleEvent asDialogStack) state event
  go newDlg

startup :: AppState -> EventM Name AppState
startup astate = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.BracketedPaste) $ liftIO $ V.setMode output V.BracketedPaste True
  pure astate

main :: IO ()
main = do
  eventChan <- newBChan 10
  replyChan <- newBChan 10
  let
    app = App
      { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent
      , appStartEvent = startup
      , appAttrMap = const $ theMap
      }
    go :: IO AppState
    go = do
      let
        mkVty = V.mkVty V.defaultConfig
      vty <- mkVty
      finalState <- customMain vty mkVty (Just eventChan) app (defaultState replyChan spawnDialog1)
      pure finalState
  finalState <- go
  print $ asLastMsg finalState
  pure ()
