{-# LANGUAGE DeriveGeneric #-}
     
module Blockchain.Data.Code where

import qualified Data.ByteString as B
import GHC.Generics 
    
import Blockchain.Data.RLP
       
newtype Code = Code{codeBytes::B.ByteString} deriving (Show, Eq, Read, Generic)

instance RLPSerializable Code where
    rlpEncode (Code bytes) = rlpEncode bytes
    rlpDecode = Code . rlpDecode

-- instance Format Code where
--    format Code {codeBytes=c} = B.unpack c
