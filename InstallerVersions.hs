{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns     #-}

module InstallerVersions
  ( GlobalResults(..)
  , findVersionInfo
  , InstallerNetwork(..)
  , installerNetwork
  ) where

import           Control.Lens               ((^?!))
import           Data.Aeson.Lens            (key, _String)
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.HashMap.Strict        as HM
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Yaml                  as Y
import qualified Filesystem.Path.CurrentOS  as FP
import           Turtle                     (FilePath, Managed, printf,d, liftIO, (%),s,format, (</>), filename)

import           Cardano                    (ConfigurationYaml,
                                             applicationVersion, update, ConfigurationRoot)
import           Github                     (Rev)
import           Nix                        (nixBuildExpr, nixEvalExpr)
import           Utils                      (tt)
import Arch (ApplicationVersionKey, Arch(Win64, Mac64))

data GlobalResults = GlobalResults {
      grCardanoCommit      :: Text
    , grDaedalusCommit     :: Text
    , grApplicationVersion :: Int
    , grCardanoVersion     :: Text
    , grDaedalusVersion    :: Text
  } deriving (Show)


findVersionInfo :: ApplicationVersionKey -> Rev -> Managed GlobalResults
findVersionInfo keys grDaedalusCommit = do
  grApplicationVersion <- grabAppVersion grDaedalusCommit keys
  printf ("applicationVersion: "%d%"\n") grApplicationVersion
  grCardanoVersion <- liftIO $ fetchCardanoVersionFromDaedalus grDaedalusCommit
  printf ("Cardano version: "%s%"\n") grCardanoVersion
  grDaedalusVersion <- liftIO $ fetchDaedalusVersion grDaedalusCommit
  printf ("Daedalus version: "%s%"\n") grDaedalusVersion
  grCardanoCommit <- liftIO $ fetchCardanoCommitFromDaedalus grDaedalusCommit
  printf ("Cardano commit: "%s%"\n") grCardanoCommit
  pure GlobalResults{grDaedalusCommit,grCardanoCommit,grDaedalusVersion,grCardanoVersion,grApplicationVersion}

-- | Gets package.json version from Daedalus sources.
fetchDaedalusVersion :: Text -> IO Text
fetchDaedalusVersion rev = getVersion <$> fetchDaedalusJSON "package.json" rev
  where
    getVersion v = v ^?! key "version" . _String

-- | Gets the git rev from cardano-sl-src.json in Daedalus
fetchCardanoCommitFromDaedalus :: Rev -> IO Rev
fetchCardanoCommitFromDaedalus rev = getRev <$> fetchDaedalusJSON "cardano-sl-src.json" rev
  where
    getRev v = v ^?! key "rev" . _String

fetchDaedalusJSON :: Turtle.FilePath -> Text -> IO S8.ByteString
fetchDaedalusJSON json rev = do
  res <- nixEvalExpr (fetchDaedalusNixExpr rev)
  loadFile json (FP.fromText $ (res ^?! _String))
  where
    loadFile fpath storePath = S8.readFile (FP.encodeString $ storePath </> fpath)

-- | Gets version string from daedalus-bridge attribute of Daedalus default.nix
fetchCardanoVersionFromDaedalus :: Text -> IO Text
fetchCardanoVersionFromDaedalus rev = getString <$> nixEvalExpr expr
  where
    getString val = val ^?! _String
    expr = format ("(import "%s%" {}).daedalus-bridge.version") (fetchDaedalusNixExpr rev)

-- | Returns the store path of daedalus-bridge.
fetchDaedalusBridge :: Rev -> Managed Turtle.FilePath
fetchDaedalusBridge rev = nixBuildExpr expr
  where expr = format ("(import "%s%" {}).daedalus-bridge") (fetchDaedalusNixExpr rev)

-- | A nix expression to import a specific revision of Deadalus from git.
fetchDaedalusNixExpr :: Rev -> Text
fetchDaedalusNixExpr = format ("(builtins.fetchTarball "%s%s%".tar.gz)") url
  where url = "https://github.com/input-output-hk/daedalus/archive/" :: Text

-- | Gets version information from the config files in the
-- daedalus-bridge derivation.
grabAppVersion :: Rev     -- ^ git commit id to check out
               -> ApplicationVersionKey -- ^ yaml keys to find
               -> Managed Int     -- ^ an integer version
grabAppVersion rev key' = do
  bridge <- fetchDaedalusBridge rev
  liftIO $ do
    (Right fullConfiguration) <- Y.decodeFileEither (FP.encodeString $ bridge </> "config/configuration.yaml")
    appVersionFromConfig key' fullConfiguration

appVersionFromConfig :: ApplicationVersionKey -> ConfigurationYaml -> IO Int
appVersionFromConfig key' cfg = case (ver Win64, ver Mac64) of
  -- TODO, doesnt check that the linux version matches the rest
  (Nothing, _)            -> fail "configuration-key missing"
  (_, Nothing)            -> fail "configuration-key missing"
  (win, mac) | win /= mac -> fail "applicationVersions dont match"
  (Just val, Just _)      -> pure (applicationVersion $ update val)
  where
    ver :: Arch -> Maybe Cardano.ConfigurationRoot
    ver a = HM.lookup (key' a) cfg

----------------------------------------------------------------------------

-- | Cardano cluster which the installer will connect to.
data InstallerNetwork = InstallerMainnet | InstallerStaging | InstallerTestnet deriving (Eq)

instance Show InstallerNetwork where
  show InstallerMainnet = "Mainnet"
  show InstallerStaging = "Staging"
  show InstallerTestnet = "Testnet"

-- | Determine which cardano network an installer is for based on its
-- filename. The inverse of this function is in
-- daedalus/installers/Types.hs.
installerNetwork :: Turtle.FilePath -> Maybe InstallerNetwork
installerNetwork fpath | "mainnet" `T.isInfixOf` name = Just InstallerMainnet
                       | "staging" `T.isInfixOf` name = Just InstallerStaging
                       | "testnet" `T.isInfixOf` name = Just InstallerTestnet
                       | otherwise = Nothing
  where name = tt (filename fpath)
