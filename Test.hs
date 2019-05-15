{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Brick (Widget, BrickEvent, EventM, Next, App(App, appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap), on, continue, showFirstCursor, customMain, halt)
import qualified Brick.AttrMap as A
import           Brick.BChan (BChan, newBChan)
import qualified Brick.Widgets.List as L
--import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import Brick.Forms

import Dialog1 (spawnDialog1)
import Types (Dialog(dRender, dHandleEvent), AppState(AppState, asDialogStack, asLastMsg), Name, CustomEvent)

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
  newDlg <- (dHandleEvent asDialogStack) state event
  case newDlg of
    Left s -> halt s
    Right dlg -> continue $ state { asDialogStack = dlg }

main :: IO ()
main = do
  eventChan <- newBChan 10
  replyChan <- newBChan 10
  let
    app = App
      { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent
      , appStartEvent = \x -> pure x
      , appAttrMap = const $ theMap
      }
    go = do
      finalState <- customMain (V.mkVty V.defaultConfig) (Just eventChan) app (defaultState replyChan spawnDialog1)
      pure finalState
  finalState <- go
  print $ asLastMsg finalState
  pure ()
