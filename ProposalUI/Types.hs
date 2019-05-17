{-# LANGUAGE OverloadedStrings #-}

module ProposalUI.Types where

import Data.Text
import           Data.Aeson                 hiding (Options, encodeFile)
import Turtle

import Brick
import qualified Brick.Widgets.List as L

import Arch
import Types
import UpdateLogic

-- | Intermediate data type for the daedalus download json file.
data DownloadVersionInfo = DownloadVersionInfo
  { dviVersion   :: Text
  , dviURL       :: Text
  , dviHash      :: Text
  , dviSHA256    :: Text
  , dviSignature :: Maybe Text
  } deriving Show

instance ToJSON DownloadVersionInfo where
  toJSON dvi = object
    [ "version"   .= dviVersion dvi
    , "URL"       .= dviURL dvi
    , "hash"      .= dviHash dvi
    , "SHA256"    .= dviSHA256 dvi
    , "signature" .= dviSignature dvi
    ]

data DownloadVersionJson = DownloadVersionJson
  { dvjDvis         :: ArchMap DownloadVersionInfo
  , dvjReleaseNotes :: Maybe Text
  } deriving Show

instance ToJSON DownloadVersionJson where
  toJSON (DownloadVersionJson dvis releaseNotes) = object [ "platforms" .= dvis, "release_notes" .= releaseNotes ]

type InstallerHashes = ArchMap Text

data MenuChoices = SetDaedalusRev | FindInstallers | SignInstallers | S3Upload deriving Show

data ProposalUIState = ProposalUIState
  { psDaedalusRev :: Maybe String
  , psCallback :: EventM Name DialogReply
  , psMenuState :: L.List Name MenuChoices
  , psInstallers :: Maybe InstallerData
  , psOutputDir :: Turtle.FilePath
  , psDownloadVersionInfo :: Maybe (ArchMap DownloadVersionInfo)
  }

data InstallerData = InstallerData
  { idResults :: InstallersResults
  , idSHA256 :: InstallerHashes
  , idBlake :: InstallerHashes
  }
