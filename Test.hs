{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Brick
import qualified Brick.AttrMap as A
import           Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import qualified Data.Vector as V

data AppState = AppState
  { asReplyChan :: BChan ()
  , asLastMsg :: String
  , asCurrentWindow :: CurrentWindow
  , asDialogStack :: [ Dialog ]
  }

data Dialog = forall s a. (Show a) => Dialog
  { dRender :: AppState -> s -> [ Widget Name ]
  , dHandleEvent :: AppState -> s -> BrickEvent Name CustomEvent -> EventM Name (NextDlg s)
  , dState :: s
  , dFinishCallback :: a -> IO ()
  }

data NextDlg s = UpdateCurrentState s | PushNew Dialog | CloseCurrent

data ClusterState = Offline | Remote Cluster | LocalDemo deriving Show
data Cluster = Mainnet | Staging | Testnet deriving Show

data CurrentWindow = MainWindow deriving Eq

data MainMenuChoices = StartDemo | ReleasePropose | DummyPropose | StopDemo deriving Show

data Name = MainMenu | None deriving (Eq, Ord, Show)
data CustomEvent = CustomEvent  deriving Show

defaultState :: BChan () -> Dialog -> AppState
defaultState replyChan dlg = AppState replyChan "" MainWindow [ dlg ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,            V.white `on` V.blue)
  , (L.listSelectedAttr,    V.blue `on` V.white)
  ]

drawUI :: AppState -> [ Widget Name ]
drawUI state = go (asDialogStack state)
  where
    go [] = [ emptyWidget ]
    go ( Dialog{dRender,dState} : _ ) = dRender state dState
  --case (asCurrentWindow state) of
  --MainWindow -> mainUi state

mainUi :: AppState -> Dialog1State -> [ Widget Name ]
mainUi mainState dlgState = [ vBox [ lastMessage mainState, state, renderMainMenu ] ]
  where
    renderRow :: Bool -> MainMenuChoices -> Widget Name
    renderRow _ name = str $ show name
    renderMainMenu = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Main Menu") $ padLeftRight 1 $ L.renderList renderRow True (dlg1MainList dlgState)
    state :: Widget Name
    state = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Global state") $ padLeftRight 1 $ strWrap $ show (dlg1ClusterState dlgState)

lastMessage :: AppState -> Widget Name
lastMessage AppState{asLastMsg} = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "last debug msg") $ padAll 1 $ strWrap asLastMsg

data Dialog1State = Dialog1State
  { dlg1MainList :: L.List Name MainMenuChoices
  , dlg1ClusterState :: ClusterState
  }

dlg1Render :: AppState -> Dialog1State -> [ Widget Name ]
dlg1Render = mainUi

updateDialogState :: AppState -> BrickEvent Name CustomEvent -> Dialog -> EventM Name (NextDlg Dialog)
updateDialogState state event Dialog{dHandleEvent,dRender,dState,dFinishCallback} = do
  foo <- dHandleEvent state dState event
  case foo of
    UpdateCurrentState newState -> do
      pure $ UpdateCurrentState $ Dialog
        { dRender = dRender
        , dHandleEvent = dHandleEvent
        , dState = newState
        , dFinishCallback = dFinishCallback
        }
    PushNew newDlg -> pure $ PushNew newDlg
    CloseCurrent -> pure CloseCurrent

dlg1HandleEvent :: AppState -> Dialog1State -> BrickEvent Name CustomEvent -> EventM Name (NextDlg Dialog1State)
dlg1HandleEvent _ dlgState event = do
  let
    openThing :: EventM Name (NextDlg Dialog1State)
    openThing = do
      case L.listSelectedElement (dlg1MainList dlgState) of
        Just (_, StartDemo) -> do
          -- TODO, actually start the demo cluster
          pure $ UpdateCurrentState $ dlgState
            { dlg1ClusterState = LocalDemo
            , dlg1MainList = L.list MainMenu (V.fromList [ ReleasePropose, DummyPropose, StopDemo ]) 1
            }
        Just (_, StopDemo) -> do
          -- TODO, actually stop the demo cluster
          pure $ UpdateCurrentState $ dlgState
            { dlg1ClusterState = Offline
            , dlg1MainList = L.list MainMenu (V.fromList [ StartDemo ]) 1
            }
        _ -> do
          pure $ UpdateCurrentState dlgState
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> pure $ CloseCurrent
    VtyEvent (V.EvKey V.KEnter []) -> do
      openThing
    VtyEvent evt -> do
      newlist <- L.handleListEventVi L.handleListEvent evt (dlg1MainList dlgState)
      pure $ UpdateCurrentState $ dlgState { dlg1MainList = newlist }
    _ -> do
      pure $ UpdateCurrentState dlgState

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent state event = do
  let
    go [] = halt state
    go (dlg : rest) = do
      foo <- updateDialogState state event dlg
      case foo of
        UpdateCurrentState newDlg -> do
          continue $ state { asDialogStack = newDlg : rest }
        PushNew dlg2 -> do
          continue $ state { asDialogStack = dlg2 : (dlg : rest) }
        CloseCurrent -> do
          let
            newstack = rest
          case newstack of
            [] -> halt $ state { asDialogStack = rest }
            x -> continue $ state { asDialogStack = x }
  go (asDialogStack state)

data Foo = Foo deriving Show

main :: IO ()
main = do
  putStrLn "hello world"
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
      let
        mainMenu = L.list MainMenu (V.fromList [ StartDemo ]) 1
        callback :: Foo -> IO ()
        callback = print
        dlg = Dialog dlg1Render dlg1HandleEvent (Dialog1State mainMenu Offline) callback
      finalState <- customMain (V.mkVty V.defaultConfig) (Just eventChan) app (defaultState replyChan dlg)
      pure finalState
  finalState <- go
  print $ asLastMsg finalState
  pure ()
