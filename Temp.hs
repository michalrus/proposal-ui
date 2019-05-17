module Temp (installerHash) where

import Data.ByteString as BS
import           Crypto.Hash (Blake2b_256, Digest)
import qualified Codec.CBOR.Encoding as E
import qualified Crypto.Hash as Hash
import qualified Codec.CBOR.Write as CBOR.Write


installerHash :: BS.ByteString -> Digest Blake2b_256
installerHash bytes = Hash.hashlazy $ CBOR.Write.toLazyByteString $ E.encodeBytes bytes
