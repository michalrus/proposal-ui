module Main where

import Temp
import Data.ByteString.Lazy as BSL
import Crypto.Hash
import qualified Data.ByteString.Base16 as B16
import Data.String
import Data.ByteString as BS

main :: IO ()
main = do
  fileData <- BS.readFile "test-data"
  let
    h1 = installerHash fileData
    Right hex = B16.decode $ fromString "4a573b1c196a074330b50171513d62a75e5f27ff154950bbbfcd7b0beec33e8e"
    Just h2 = digestFromByteString hex
  print h1
  if h1 == h2
    then
      print "good"
    else
      undefined
  pure ()
