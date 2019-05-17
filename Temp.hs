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

-- various things copied from cardano-sl, will be moved into a better location once it works

installerHash :: BSL.ByteString -> Hash Raw
installerHash = castHash . hash

type Hash = AbstractHash Blake2b_256

newtype Raw = Raw ByteString deriving (Bi, Eq, Ord, Show, Typeable, NFData)

-- | Hash wrapper with phantom type for more type-safety.
newtype AbstractHash algo a = AbstractHash (Digest algo) deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

class Typeable a => Bi a where
  encode :: a -> E.Encoding

instance Bi BS.ByteString where
  encode = E.encodeBytes

-- | Type class for unsafe cast between hashes.
-- You must ensure that types have identical Bi instances.
class CastHash a b where
  castHash :: AbstractHash algo a -> AbstractHash algo b
  castHash (AbstractHash x) = AbstractHash x

hash :: Bi a => a -> Hash a
hash = unsafeHash

instance CastHash a Raw

instance Bi BSL.ByteString where
  encode = encode . BSL.toStrict

unsafeHash :: Bi a => a -> Hash b
unsafeHash = unsafeAbstractHash

unsafeAbstractHash :: (HashAlgorithm algo, Bi a) => a -> AbstractHash algo b
unsafeAbstractHash = AbstractHash . Hash.hashlazy . serialize

serialize :: Bi a => a -> BSL.ByteString
serialize = serializeWith 1024 4096

serializeWith :: Bi a => Int -> Int -> a -> BSL.ByteString
serializeWith firstChunk nextChunk = Builder.toLazyByteStringWith strategy mempty . serializeBuilder
  where
    strategy = Builder.safeStrategy firstChunk nextChunk

serializeBuilder :: Bi a => a -> Builder
serializeBuilder = CBOR.Write.toBuilder . encode
