{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Blockchain.Data.MiscJSON where
       
import Data.Aeson
import Data.Aeson.Types

import Data.Text.Encoding
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS
import Control.Applicative

import Blockchain.Database.MerklePatricia
import Blockchain.Data.Code
import Blockchain.Data.Transaction
import Crypto.Types.PubKey.ECC
import Blockchain.SHA
import Blockchain.Handshake

instance ToJSON Transaction
instance FromJSON Transaction

instance FromJSON Code
instance ToJSON Code

instance ToJSON SHAPtr 
instance FromJSON SHAPtr

instance ToJSON SHA
instance FromJSON SHA

instance FromJSON BS.ByteString where
    parseJSON (String t) = pure $ fst $ B16.decode $ encodeUtf8 $ t
    parseJSON v          = typeMismatch "ByteString" v

instance ToJSON BS.ByteString where
    toJSON  = String . decodeUtf8 .  B16.encode

instance FromJSON Point where
    parseJSON (String t) = pure $ bytesToPoint $ BS.unpack $ fst $ B16.decode $ encodeUtf8 $ t
    parseJSON v          = typeMismatch "Point" v

instance ToJSON Point where
    toJSON = String . decodeUtf8 . B16.encode . BS.pack . pointToBytes 
