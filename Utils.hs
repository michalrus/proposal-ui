{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Utils
  ( fetchJson'
  , tt
  , fetchJson
  , fetchCachedUrl
  , fetchCachedUrlWithSHA1) where

import           Prelude                   hiding (FilePath)
import           Data.Aeson                (FromJSON, decode)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import qualified Data.Text.Lazy.Encoding   as LT
import           GHC.Stack                 (HasCallStack)
import           Network.HTTP.Client       (httpLbs, parseRequest,
                                            requestHeaders, responseBody)
import           Network.HTTP.Client.TLS   (newTlsManager)
import           Network.HTTP.Types.Header (RequestHeaders)
import           System.Exit               (ExitCode (ExitFailure, ExitSuccess))
import           Data.Text                 (Text)
import           Turtle                    (FilePath, (%), s, fp, format, proc)

fetchCachedUrl :: HasCallStack => T.Text -> FilePath -> FilePath -> IO ()
fetchCachedUrl url name outPath = fetchCachedUrl' url name outPath Nothing

fetchCachedUrlWithSHA1 :: HasCallStack => T.Text -> T.Text -> FilePath -> FilePath -> IO ()
fetchCachedUrlWithSHA1 sha1 url name outPath = fetchCachedUrl' url name outPath (Just sha1)

fetchCachedUrl' :: HasCallStack => T.Text -> FilePath -> FilePath -> Maybe T.Text -> IO ()
fetchCachedUrl' url name outPath sha1 = proc "nix-build" args mempty >>= handleExit
  where
    args = [ "-E", format ("with import <nixpkgs> {}; let file = "%s%"; in runCommand \""%fp%"\" {} \"ln -sv ${file} $out\"") fetchExpr name, "-o", format fp outPath ]
    fetchExpr = case sha1 of
      Just hash -> format ("pkgs.fetchurl { url = \""%s%"\"; sha1 = \""%s%"\"; }") url hash
      Nothing   -> "builtins.fetchurl \"" <> url <> "\""
    handleExit ExitSuccess     = return ()
    handleExit (ExitFailure _) = error "error downloading file"

fetchJson :: HasCallStack => FromJSON a => T.Text -> IO a
fetchJson = fetchJson' mempty

fetchJson' :: HasCallStack => FromJSON a => RequestHeaders -> T.Text -> IO a
fetchJson' extraHeaders url = do
  reply <- fetchUrl extraHeaders url
  let
    maybeObj :: FromJSON a => Maybe a
    maybeObj = decode reply
  case maybeObj of
    Just v  -> return v
    Nothing -> error $ "unable to parse json: " <> LT.unpack (LT.decodeUtf8 reply) <> " from: " <> T.unpack url

fetchUrl :: RequestHeaders -> T.Text -> IO LBS.ByteString
fetchUrl extraHeaders url = do
  man <- newTlsManager
  reqUrl <- (parseRequest . T.unpack) url
  let req' = reqUrl { requestHeaders = [ ("User-Agent", "https://github.com/input-output-hk/iohk-ops") ] <> extraHeaders }
  resp <- httpLbs req' man
  return $ responseBody resp

tt :: FilePath -> Text
tt = format fp
