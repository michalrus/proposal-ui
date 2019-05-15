{-# LANGUAGE OverloadedStrings #-}

module ProposalUI (spawnProposalUI) where

import Brick
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V

import Types
import PromptString

data ProposalUIState = ProposalUIState
  { psDaedalusRev :: Maybe String
  , psCallback :: EventM Name (Either AppState Dialog)
  , psMenuState :: L.List Name MenuChoices
  }

data MenuChoices = SetDaedalusRev deriving Show

mkProposalUI :: ProposalUIState -> Dialog
mkProposalUI state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

generateNewMenu :: ProposalUIState -> L.List Name MenuChoices
generateNewMenu pstate = L.list Menu1 (V.fromList [ SetDaedalusRev ]) 1

renderUI :: ProposalUIState -> AppState -> [ Widget Name ]
renderUI pstate astate = [ root ]
  where
    root :: Widget Name
    root = vBox [ B.borderWithLabel (str "Current State") status, menu ]
    status :: Widget Name
    status = vBox [ daedalusRev ]
    daedalusRev :: Widget Name
    daedalusRev = case (psDaedalusRev pstate) of
      Nothing -> str "No Daedalus Revision set"
      Just rev -> str $ "Daeadalus Revision: " <> rev
    menu :: Widget Name
    menu = B.borderWithLabel (str "Menu") $ padLeftRight 1 $ L.renderList renderRow True (psMenuState pstate)
    renderRow :: Bool -> MenuChoices -> Widget Name
    renderRow _ SetDaedalusRev = str "Set Daedalus Revision"

handleEvents :: ProposalUIState -> AppState -> BrickEvent Name CustomEvent -> EventM Name (Either AppState Dialog)
handleEvents pstate astate event = do
  let
    isValidRevision :: String -> Bool
    isValidRevision = all isValidChar
    isValidChar x = ( (x >= 'a') && (x <= 'f') ) || ( (x >= '0') && (x <= '9') )
    openThing :: EventM Name (Either AppState Dialog)
    openThing = do
      case L.listSelectedElement (psMenuState pstate) of
        Just (index, item) -> case item of
          SetDaedalusRev -> spawnPromptString "Daedalus Revision?" isValidRevision $ \rev -> do
            let
              state1 = pstate { psDaedalusRev = Just rev }
              state2 = pstate { psMenuState = generateNewMenu state1 }
            pure $ Right $ mkProposalUI state2
        Nothing -> pure $ Right $ mkProposalUI pstate -- nothing selected, do nothing
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      psCallback pstate
    VtyEvent (V.EvKey V.KEnter []) -> do
      openThing
    _ -> do
      pure $ Right $ mkProposalUI pstate

spawnProposalUI :: EventM Name (Either AppState Dialog) -> EventM Name Dialog
spawnProposalUI callback = do
  let
    state' = ProposalUIState Nothing callback undefined
    menu = generateNewMenu state'
    state = state' { psMenuState = menu }
  pure $ mkProposalUI state
