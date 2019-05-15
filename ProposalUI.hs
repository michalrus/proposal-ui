{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ProposalUI (spawnProposalUI) where

import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Managed
import qualified System.Process             as P
import Turtle
import Control.Concurrent
import Data.Time.Clock
import Data.Time.Format

import Brick
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V

import Types
import PromptString

import Iohk.Types
import UpdateLogic
import InstallerVersions
import Github (Rev)
import Utils

data ProposalUIState = ProposalUIState
  { psDaedalusRev :: Maybe String
  , psCallback :: EventM Name DialogReply
  , psMenuState :: L.List Name MenuChoices
  , psInstallers :: Maybe InstallersResults
  , psOutputDir :: Turtle.FilePath
  }

data MenuChoices = SetDaedalusRev | FindInstallers | SignInstallers deriving Show

mkProposalUI :: ProposalUIState -> Dialog
mkProposalUI state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

generateNewMenu :: ProposalUIState -> L.List Name MenuChoices
generateNewMenu ProposalUIState{psDaedalusRev,psInstallers} = L.list Menu1 (V.fromList thelist) 1
  where
    thelist = [ SetDaedalusRev ] <> maybeFindInstallers <> maybeSign
    maybeFindInstallers = if (isJust psDaedalusRev) then [ FindInstallers ] else []
    maybeSign = if (isJust psInstallers) then [ SignInstallers ] else []

renderUI :: ProposalUIState -> AppState -> [ Widget Name ]
renderUI ProposalUIState{psMenuState,psInstallers,psDaedalusRev} _astate = [ root ]
  where
    root :: Widget Name
    root = vBox [ B.borderWithLabel (str "Current State") status, menu ]
    status :: Widget Name
    status = vBox $ [ daedalusRev ] <> (mkInstallers psInstallers)
    daedalusRev :: Widget Name
    daedalusRev = case psDaedalusRev of
      Nothing -> str "No Daedalus Revision set"
      Just rev -> str $ "Daeadalus Revision: " <> rev
    mkInstallers :: Maybe InstallersResults -> [ Widget Name ]
    mkInstallers (Just InstallersResults{ciResults,globalResult}) = mkGlobalResult globalResult <> (map mkCiResult ciResults)
    mkInstallers Nothing = []
    mkCiResult :: CIResult -> Widget Name
    mkCiResult CIResult{ciResultLocalPath,ciResultUrl,ciResultDownloadUrl,ciResultBuildNumber,ciResultArch,ciResultSHA1Sum} = B.border $ vBox
      [ str $ "Arch: " <> show ciResultArch
      , str $ "Local Path: " <> show ciResultLocalPath
      , txt $ "URL: " <> ciResultUrl
      , txt $ "Download URL: " <> ciResultDownloadUrl
      , str $ "Build#: " <> show ciResultBuildNumber
      , str $ "SHA1: " <> show ciResultSHA1Sum
      ]
    mkGlobalResult GlobalResults{grCardanoCommit,grDaedalusCommit,grApplicationVersion,grCardanoVersion,grDaedalusVersion} =
      [ txt $ "Cardano rev: " <> grCardanoCommit
      , txt $ "Daedalus rev: " <> grDaedalusCommit
      , str $ "ApplicationVersion: " <> (show grApplicationVersion)
      , txt $ "Cardano Version: " <> grCardanoVersion
      , txt $ "Daedalus Version: " <> grDaedalusVersion ]
    menu :: Widget Name
    menu = B.borderWithLabel (str "Menu") $ padLeftRight 1 $ L.renderList renderRow True psMenuState
    renderRow :: Bool -> MenuChoices -> Widget Name
    renderRow _ SetDaedalusRev = str "Set Daedalus Revision"
    renderRow _ FindInstallers = str "Find Installers"
    renderRow _ SignInstallers = str "Sign installers with GPG"

configurationKeys :: Environment -> Arch -> T.Text
configurationKeys Production Win64   = "mainnet_wallet_win64"
configurationKeys Production Mac64   = "mainnet_wallet_macos64"
configurationKeys Production Linux64 = "mainnet_wallet_linux64"
configurationKeys Staging    Win64   = "mainnet_dryrun_wallet_win64"
configurationKeys Staging    Mac64   = "mainnet_dryrun_wallet_macos64"
configurationKeys Staging    Linux64 = "mainnet_dryrun_wallet_linux64"
configurationKeys Testnet    Win64   = "testnet_wallet_win64"
configurationKeys Testnet    Mac64   = "testnet_wallet_macos64"
configurationKeys Testnet    Linux64 = "testnet_wallet_linux64"
configurationKeys env' _ = error $ "Application versions not used in '" <> show env' <> "' environment"

-- | Step 2a. (Optional) Sign installers with GPG. This will leave
-- .asc files next to the installers which will be picked up in the
-- upload S3 step.
updateProposalSignInstallers :: InstallersResults -> Maybe T.Text -> IO ()
updateProposalSignInstallers params userId = do
  mapM_ signInstaller (map ciResultLocalPath . ciResults $ params)
  where
    -- using system instead of procs so that tty is available to gpg
    signInstaller f = system (P.proc "gpg2" $ map T.unpack $ gpgArgs f) empty
    gpgArgs f = userArg ++ ["--detach-sig", "--armor", "--sign", tt f]
    userArg = case userId of
                Just u  -> ["--local-user", u]
                Nothing -> []

-- | Checks if an installer from a CI result matches the environment
-- that iohk-ops is running under.
installerForEnv :: Environment -> CIResult -> Bool
installerForEnv env = matchNet . installerNetwork . ciResultLocalPath
  where matchNet n = case env of
          Production  -> n == Just InstallerMainnet
          Staging     -> n == Just InstallerStaging
          Testnet     -> n == Just InstallerTestnet
          Development -> True
          _           -> False

findInstallers :: Turtle.FilePath -> Environment -> Rev -> IO InstallersResults
findInstallers destDir env rev = do
  let
    bkNum = Nothing
    avNum = Nothing
    instP = installerPredicates (installerForEnv env) (selectBuildNumberPredicate bkNum avNum)
  with (getInstallersResults (configurationKeys env) instP rev (Just destDir)) pure

handleEvents :: ProposalUIState -> AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
handleEvents pstate@ProposalUIState{psMenuState,psDaedalusRev,psInstallers,psOutputDir} _astate event = do
  let
    isValidRevision :: String -> Bool
    isValidRevision = all isValidChar
    isValidChar char = ( (char >= 'a') && (char <= 'f') ) || ( (char >= '0') && (char <= '9') )
    openThing :: EventM Name DialogReply
    openThing = do
      case L.listSelectedElement psMenuState of
        Just (_index, item) -> case item of
          SetDaedalusRev -> spawnPromptString "Daedalus Revision?" isValidRevision $ \rev -> do
            let
              state1 = pstate { psDaedalusRev = Just rev, psInstallers = Nothing }
              state2 = state1 { psMenuState = generateNewMenu state1 }
            pure $ DialogReplyContinue $ mkProposalUI state2
          FindInstallers -> do
            let
              act :: IO Dialog
              act = do
                res <- findInstallers psOutputDir Production (T.pack $ fromJust psDaedalusRev)
                let
                  state1 = pstate { psInstallers = Just res }
                  state2 = state1 { psMenuState = generateNewMenu state1 }
                pure $ mkProposalUI state2
            pure $ DialogReplyLiftIO act
          SignInstallers -> do
            pure $ DialogReplyLiftIO $ do
              updateProposalSignInstallers (fromJust psInstallers) (Just "michael.bishop@iohk.io")
              threadDelay 1000000
              -- TODO, flag as signed, and give a popup saying success/fail
              pure $ mkProposalUI pstate
        Nothing -> pure $ DialogReplyContinue $ mkProposalUI pstate -- nothing selected, do nothing
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      psCallback pstate
    VtyEvent (V.EvKey V.KEnter []) -> do
      openThing
    VtyEvent evt -> do
      newlist <- L.handleListEventVi L.handleListEvent evt psMenuState
      pure $ DialogReplyContinue $ mkProposalUI pstate { psMenuState = newlist }
    _ -> do
      pure $ DialogReplyContinue $ mkProposalUI pstate

spawnProposalUI :: EventM Name DialogReply -> EventM Name Dialog
spawnProposalUI callback = do
  now <- liftIO $ getCurrentTime
  let
    yearMonthDay = formatTime defaultTimeLocale (iso8601DateFormat Nothing) now
    destDir = "proposal-cluster-" <> yearMonthDay
    state' :: ProposalUIState
    state' = ProposalUIState (Just "639ff61007c98257278e34d2aa2809e851417d01") callback undefined Nothing (fromString destDir)
    menu = generateNewMenu state'
    state = state' { psMenuState = menu }
  pure $ mkProposalUI state
