{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

--TODO : Take this next line out
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.Data.MiscJSON where
       
import Data.Aeson
import Data.Aeson.Types

import Data.Text.Encoding
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import Control.Applicative

import Crypto.Types.PubKey.ECC
import Blockchain.SHA
import Blockchain.Handshake

instance ToJSON SHA
instance FromJSON SHA

instance FromJSON B.ByteString where
    parseJSON (String t) = pure $ fst $ B16.decode $ encodeUtf8 $ t
    parseJSON v          = typeMismatch "ByteString" v

instance ToJSON B.ByteString where
    toJSON  = String . decodeUtf8 .  B16.encode

instance FromJSON Point where
    parseJSON (String t) = pure $ bytesToPoint $ B.unpack $ fst $ B16.decode $ encodeUtf8 $ t
    parseJSON v          = typeMismatch "Point" v

instance ToJSON Point where
    toJSON = String . decodeUtf8 . B16.encode . B.pack . pointToBytes 
