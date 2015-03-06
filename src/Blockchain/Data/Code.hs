
module Blockchain.Data.Code where

import Blockchain.Data.RLP

import qualified Data.ByteString as B

newtype Code = Code{codeBytes::B.ByteString} deriving (Show, Eq)

instance RLPSerializable Code where
    rlpEncode (Code bytes) = rlpEncode bytes
    rlpDecode = Code . rlpDecode

