{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, MultiParamTypeClasses, FlexibleInstances #-}

module Temp where

import qualified Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import           Crypto.Hash (Blake2b_256, Digest, HashAlgorithm)
import Control.DeepSeq
import Data.Dynamic
import qualified Data.ByteArray as ByteArray
import GHC.Generics
import qualified Codec.CBOR.Encoding as E
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Codec.CBOR.Write as CBOR.Write
import           Data.ByteString.Builder (Builder)


installerHash :: BS.ByteString -> Digest Blake2b_256
installerHash bytes = Hash.hashlazy $ CBOR.Write.toLazyByteString $ E.encodeBytes bytes
