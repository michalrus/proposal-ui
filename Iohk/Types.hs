{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}

module Iohk.Types
  ( ApplicationVersion(..)
  , Environment(..)
  ) where

import qualified Data.Map.Strict       as Map
import           Data.String
import           Data.Text
import           Data.Yaml             (FromJSON (..), ToJSON (..))
import           GHC.Generics          hiding (from, to)
import qualified GHC.Generics          as G
import           Prelude               hiding (FilePath)
import qualified Turtle                as Turtle

import           Arch

-- * Elementary types
--
newtype ApplicationVersion = ApplicationVersion { getApplicationVersion :: Text } deriving (FromJSON, IsString, Show, Eq, Generic, ToJSON)


data Environment
  = Any               -- ^ Wildcard or unspecified, depending on context.
  | Benchmark
  | Production
  | Staging
  | Testnet
  | Development
  | DevOps
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Environment

