{-# LANGUAGE DataKinds, DeriveGeneric, DeriveDataTypeable, FlexibleInstances, GADTs, KindSignatures, OverloadedStrings, RecordWildCards, StandaloneDeriving, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-orphans -Wno-missing-signatures #-}

module Nix
  ( nixBuildExpr
  , nixEvalExpr
  ) where

import           Prelude                   hiding (FilePath)

import           Control.Monad.Catch              (Exception, throwM, MonadThrow)
import qualified Data.Aeson                    as AE
import           Data.Aeson                       (eitherDecodeStrict, Value)
import qualified Data.ByteString.Char8         as S8
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import           Data.Text.Encoding.Error         (lenientDecode)
import           Data.Yaml                        (FromJSON(..), ToJSON(..))
import           Filesystem.Path                  (FilePath)
import qualified Filesystem.Path.CurrentOS     as FP
import           Turtle                    hiding (env, err, fold, prefix, procs, e, f, o, x)
import qualified Turtle.Bytes                  as B
import Control.Monad.Managed (MonadManaged)
import qualified System.Process             as P


instance FromJSON FilePath where parseJSON = AE.withText "filepath" $ \v -> pure $ fromText v
instance ToJSON   FilePath where toJSON    = AE.String . format fp


-- | Evaluate a nix expression, returning its value as JSON.
nixEvalExpr :: Text -> IO Value
nixEvalExpr expr = eval >>= parseNixOutput
  where eval = procNix "nix-instantiate" [ "--json", "--read-write-mode" , "--eval" , "--expr", expr ]

-- | Build a nix expression, returning the store path.
nixBuildExpr :: MonadManaged m => Text -> m FilePath
nixBuildExpr expr = do
  dir <- (T.pack . encodeString) <$> mktempdir "/tmp" "nixbuild-"
  -- using system instead of procs so that tty is available to nix
  _ <- liftIO $ system (P.proc "nix" [ "build", "-o", T.unpack $ dir <> "/result", T.unpack $ "(" <> expr <> ")" ]) empty
  pure $ FP.fromText $ dir <> "/result"

data NixError = NixError { nixErrorStatus :: Int, nixErrorMessage :: Text } deriving Show
instance Exception NixError

parseNixOutput :: (MonadThrow m, FromJSON a) => S8.ByteString -> m a
parseNixOutput json = case eitherDecodeStrict json of
  Right val -> pure val
  Left e -> throwM $ NixError 0 ("Could not parse nix output: " <> T.pack e)

procNix :: Text -> [Text] -> IO S8.ByteString
procNix cmd args = log >> B.procStrictWithErr cmd args empty >>= handle
  where
    log = T.putStrLn $ T.intercalate " " (cmd:args)
    handle (ExitSuccess, out, _) = pure out
    handle (ExitFailure status, _, err) = throwM $ NixError status (T.decodeUtf8With lenientDecode err)
