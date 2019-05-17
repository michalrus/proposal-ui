{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module ProposalUI (spawnProposalUI) where

import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Managed
import qualified System.Process             as P
import           Turtle (FilePath, Shell, system, empty,single,die,format,(%),w,printf,s,fp, (<.>), testfile, readTextFile, join, (</>), encodeString, fromString)
import Data.Time.Clock
import Data.Time.Format
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Aeson                 hiding (Options, encodeFile)
import qualified Data.HashMap.Strict        as HM

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

import ProposalUI.Types
import RunCardano

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
    mkInstallers :: Maybe InstallerData -> [ Widget Name ]
    mkInstallers (Just InstallerData{idResults = InstallersResults{ciResults,globalResult},idSHA256,idBlake}) =
         mkGlobalResult globalResult
      <> (map mkCiResult ciResults)
      <> [ ((str . show) idSHA256), ((str . show) idBlake) ]
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
    renderRow _ S3Upload = str "Upload Installers to S3"

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

findInstallers :: BucketInfo -> Turtle.FilePath -> Environment -> Rev -> IO InstallerData
findInstallers bucket destDir env rev = do
  let
    bkNum = Nothing
    avNum = Nothing
    instP = installerPredicates (installerForEnv env) (selectBuildNumberPredicate bkNum avNum)
    opts :: CommandOptions
    opts = commandOptions "/tmp/work/path" "/cardano/src" "mainnet-dryrun-full" (biBucket bucket)
    thing2 :: Managed InstallersResults
    thing2 = getInstallersResults (configurationKeys env) instP rev (Just destDir)
  installerResults <- with thing2 pure
  let
    thing :: Shell (InstallerHashes, InstallerHashes)
    thing = do
      sha256 <- getHashes sha256sum installerResults
      hashes <- getHashes (cardanoHashInstaller opts) installerResults
      pure (sha256, hashes)
  (sha256', hashes') <- single thing
  pure $ InstallerData installerResults sha256' hashes'

-- | Apply a hashing command to all the installer files.
getHashes :: MonadManaged m => (Turtle.FilePath -> m T.Text) -> InstallersResults -> m InstallerHashes
getHashes getHash res = forResults res resultHash
  where
    resultHash arch r = getHash (ciResultLocalPath r) >>= check arch
    check arch "" = die $ format ("Hash for "%w%" installer is empty") arch
    check _ h     = pure h

-- | Step 3. Hash installers and upload to S3
updateProposalUploadS3 :: BucketInfo -> InstallerData -> Shell (ArchMap DownloadVersionInfo)
updateProposalUploadS3 bucket InstallerData{idResults,idSHA256,idBlake} = do
  printf ("*** Uploading installers to S3 bucket "%s%"\n") (biBucket bucket)
  urls <- uploadInstallers bucket idResults idBlake
  printf ("*** Uploading signatures to same S3 bucket.\n")
  signatures <- uploadSignatures bucket idResults
  let
    dvis = makeDownloadVersionInfo idResults urls idBlake idSHA256 signatures
  pure dvis

createVersionJSON :: ArchMap DownloadVersionInfo -> Turtle.FilePath -> IO ()
createVersionJSON dvis dir = do
  let
    cfgReleaseNotes = undefined
  printf ("*** Writing "%fp%"\n") (versionFile dir)
  liftIO $ writeVersionJSON (versionFile dir) dvis cfgReleaseNotes

uploadInstallers :: BucketInfo -> InstallersResults -> InstallerHashes -> Shell (ArchMap T.Text)
uploadInstallers bucket res hashes = runAWS' $ forResults res upload
  where
    upload arch ci = do
      let hash = lookupArch arch hashes
      printf ("***   "%s%"  "%fp%"\n") hash (ciResultLocalPath ci)
      uploadHashedInstaller bucket (ciResultLocalPath ci) (globalResult res) hash

-- | Perform an action on the CI result of each arch.
forResults :: MonadIO io => InstallersResults -> (Arch -> CIResult -> io b) -> io (ArchMap b)
forResults rs action = needCIResult rs >>= archMapEach action

uploadResultSignature :: BucketInfo -> CIResult -> IO (Maybe T.Text)
uploadResultSignature bucket res = liftIO $ maybeReadFile sigFile >>= \case
  Just sig -> do
    runAWS' $ uploadSignature bucket sigFile
    pure $ Just sig
  Nothing -> do
    printf ("***   Signature file "%fp%" does not exist.\n") sigFile
    pure Nothing
  where
    sigFile = ciResultLocalPath res <.> "asc"
    maybeReadFile f = testfile f >>= \case
      True -> Just <$> readTextFile f
      False -> pure Nothing

-- | Partition CI results by arch.
groupResults :: InstallersResults -> ArchMap [CIResult]
groupResults rs = filt <$> idArchMap
  where filt arch = filter ((== arch) . ciResultArch) (ciResults rs)

-- | Get a single CI result for each arch and crash if not found.
needCIResult :: MonadIO io => InstallersResults -> io (ArchMap CIResult)
needCIResult = archMapEach need . groupResults
  where
    need arch [] = die $ format ("The CI result for "%w%" is required but was not found.") arch
    need _arch (r:_) = pure r

-- | Slurp in previously created signatures.
uploadSignatures :: BucketInfo -> InstallersResults -> Shell (ArchMap (Maybe T.Text))
uploadSignatures bucket irs = fmap join . archMapFromList <$> mapM uploadSig (ciResults irs)
  where
    uploadSig res = do
      sig <- liftIO $ uploadResultSignature bucket res
      pure (ciResultArch res, sig)

-- | Path to the version info json in the work directory.
versionFile :: Turtle.FilePath -> Turtle.FilePath
versionFile dir = dir </> "daedalus-latest-version.json"

writeVersionJSON :: Turtle.FilePath -> ArchMap DownloadVersionInfo -> Maybe T.Text -> IO ()
writeVersionJSON out dvis releaseNotes = L8.writeFile (encodeString out) (encode v)
  where
    v = downloadVersionInfoObject dvis releaseNotes

-- | Adds two json objects together.
mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object (a <> b)
mergeObjects _ b                   = b

-- | Splat version info to an aeson object.
downloadVersionInfoObject :: ArchMap DownloadVersionInfo  -> Maybe T.Text -> Value
downloadVersionInfoObject dvis releaseNotes = mergeObjects legacy newFormat
  where
    legacy :: Value
    legacy = (Object . HM.fromList . concat . map (uncurry toObject) . archMapToList) dvis
    newFormat :: Value
    newFormat = toJSON (DownloadVersionJson dvis releaseNotes)
    toObject :: Arch -> DownloadVersionInfo -> [ (T.Text, Value) ]
    toObject arch DownloadVersionInfo{dviVersion,dviURL,dviHash,dviSignature,dviSHA256} = attrs
      where
        attrs :: [ (T.Text, Value) ]
        attrs = [ (keyPrefix arch <> k, String v) | (k, v) <-
                    [ (""         , dviVersion)
                    , ("URL"      , dviURL)
                    , ("Hash"     , dviHash)
                    , ("SHA256"   , dviSHA256)
                    , ("Signature", fromMaybe "" dviSignature)
                    ] ]
    keyPrefix Mac64   = "macos"
    keyPrefix Win64   = "win64"
    keyPrefix Linux64 = "linux"

makeDownloadVersionInfo :: InstallersResults
                        -> ArchMap T.Text         -- ^ Download URLS
                        -> InstallerHashes      -- ^ Blake2b hashes
                        -> InstallerHashes      -- ^ SHA256 hashes
                        -> ArchMap (Maybe T.Text) -- ^ GPG Signatures
                        -> ArchMap DownloadVersionInfo
makeDownloadVersionInfo InstallersResults{globalResult} urls hashes sha256 sigs = archMap dvi
  where
    dvi a = DownloadVersionInfo
      { dviVersion = grDaedalusVersion globalResult
      , dviURL = lookupArch a urls
      , dviHash = lookupArch a hashes
      , dviSHA256 = lookupArch a sha256
      , dviSignature = lookupArch a sigs
      }

handleEvents :: ProposalUIState -> AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
handleEvents pstate@ProposalUIState{psMenuState,psDaedalusRev,psInstallers,psOutputDir} _astate event = do
  let
    bucket = BucketInfo "bucket-name" "bucket-name.s3.amazonaws.com"
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
                res <- findInstallers bucket psOutputDir Production (T.pack $ fromJust psDaedalusRev)
                let
                  state1 = pstate { psInstallers = Just res }
                  state2 = state1 { psMenuState = generateNewMenu state1 }
                pure $ mkProposalUI state2
            pure $ DialogReplyLiftIO act
          SignInstallers -> do
            pure $ DialogReplyLiftIO $ do
              updateProposalSignInstallers (idResults $ fromJust psInstallers) (Just "michael.bishop@iohk.io")
              -- TODO, flag as signed, and give a popup saying success/fail
              pure $ mkProposalUI pstate
          S3Upload -> do
            pure $ DialogReplyLiftIO $ do
              dvis <- single $ updateProposalUploadS3 bucket (fromJust psInstallers)
              let
                state1 = pstate { psDownloadVersionInfo = Just dvis }
                state2 = state1 { psMenuState = generateNewMenu state1 }
              pure $ mkProposalUI state2
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
    state' = ProposalUIState (Just "639ff61007c98257278e34d2aa2809e851417d01") callback undefined Nothing (fromString destDir) Nothing
    menu = generateNewMenu state'
    state = state' { psMenuState = menu }
  pure $ mkProposalUI state
