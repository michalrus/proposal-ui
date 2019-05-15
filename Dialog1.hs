{-# LANGUAGE NamedFieldPuns    #-}

module Dialog1 (spawnDialog1) where

import qualified Brick.Widgets.List as L

import           Types (Name(MainMenu), AppState(AppState, asLastMsg), Dialog(Dialog, dRender, dHandleEvent), CustomEvent, DialogReply(DialogReplyContinue, DialogReplyHalt))
import           Brick (Widget, BrickEvent(VtyEvent), EventM, vBox, str, padLeftRight, withBorderStyle, strWrap, padAll)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V
import qualified Data.Vector as V

import FileChooser (spawnFileChooser)
import ProposalUI (spawnProposalUI)

data Dialog1State = Dialog1State
  { dlg1MainList :: L.List Name MainMenuChoices
  , dlg1ClusterState :: ClusterState
  , dlg1Path :: String
  }

data MainMenuChoices = StartDemo | ReleasePropose | DummyPropose | StopDemo | TestFileChoice | ProposalUI deriving Show
data ClusterState = Offline | Remote Cluster | LocalDemo deriving Show
data Cluster = Mainnet | Staging | Testnet deriving Show

dlg1Render :: Dialog1State -> AppState -> [ Widget Name ]
dlg1Render = mainUi

mainUi :: Dialog1State -> AppState -> [ Widget Name ]
mainUi dlgState mainState = [ vBox [ lastMessage mainState, state, renderMainMenu ] ]
  where
    renderRow :: Bool -> MainMenuChoices -> Widget Name
    renderRow _ name = str $ show name
    renderMainMenu = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Main Menu") $ padLeftRight 1 $ L.renderList renderRow True (dlg1MainList dlgState)
    state :: Widget Name
    state = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Global state") $ padLeftRight 1 $ strWrap $ show (dlg1ClusterState dlgState)

lastMessage :: AppState -> Widget Name
lastMessage AppState{asLastMsg} = (withBorderStyle BS.unicodeBold . B.borderWithLabel (str "last debug msg") . padAll 1) (strWrap asLastMsg)

mkDlg1 :: Dialog1State -> Dialog
mkDlg1 state = Dialog { dRender = dlg1Render state, dHandleEvent = dlg1HandleEvent state }

dlg1HandleEvent :: Dialog1State -> AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
dlg1HandleEvent dState as event = do
  let
    openThing :: EventM Name Dialog
    openThing = do
      case L.listSelectedElement (dlg1MainList dState) of
        Just (_, StartDemo) -> do
          -- TODO, actually start the demo cluster
          let
            newState = dState
              { dlg1ClusterState = LocalDemo
              , dlg1MainList = L.list MainMenu (V.fromList [ ReleasePropose, DummyPropose, StopDemo, TestFileChoice ]) 1
              }
          pure $ Dialog
            { dRender = dlg1Render newState
            , dHandleEvent = dlg1HandleEvent newState
            }
        Just (_, StopDemo) -> do
          -- TODO, actually stop the demo cluster
          pure $ mkDlg1 $ dState
            { dlg1ClusterState = Offline
            , dlg1MainList = L.list MainMenu (V.fromList [ StartDemo, TestFileChoice ]) 1
            }
        Just (_, TestFileChoice) -> do
          spawnFileChooser $ \mPath -> do
            pure $ case mPath of
              Just path -> DialogReplyContinue $ mkDlg1 $ dState { dlg1Path = path }
              Nothing -> DialogReplyContinue $ mkDlg1 dState
        Just (_, ProposalUI) -> do
          spawnProposalUI $ do
            pure $ DialogReplyContinue $ mkDlg1 dState
        _ -> do
          pure $ mkDlg1 dState
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> pure $ DialogReplyHalt as
    VtyEvent (V.EvKey V.KEnter []) -> do
      DialogReplyContinue <$> openThing
    VtyEvent evt -> do
      newlist <- L.handleListEventVi L.handleListEvent evt (dlg1MainList dState)
      pure $ DialogReplyContinue $ mkDlg1 dState { dlg1MainList = newlist }
    _ -> do
      pure $ DialogReplyContinue $ mkDlg1 dState

spawnDialog1 :: Dialog
spawnDialog1 = mkDlg1 state
  where
    state = Dialog1State mainMenu Offline ""
    mainMenu = L.list MainMenu (V.fromList [ StartDemo, TestFileChoice, ProposalUI ]) 1
