{-# LANGUAGE OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.Data.GenesisInfo (
  GenesisInfo(..)
  ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Time
import Data.Word

import Blockchain.Data.Address
import Blockchain.ExtWord
import Blockchain.SHA
import Blockchain.Database.MerklePatricia

data GenesisInfo =
  GenesisInfo {
    genesisInfoParentHash::SHA,
    genesisInfoUnclesHash::SHA,
    genesisInfoCoinbase::Address,
    genesisInfoAccountInfo::[(Word160, Integer)],
    genesisInfoTransactionsRoot::SHAPtr,
    genesisInfoReceiptsRoot::SHAPtr,
    genesisInfoLogBloom::B.ByteString,
    genesisInfoDifficulty::Integer,
    genesisInfoNumber::Integer,
    genesisInfoGasLimit::Integer,
    genesisInfoGasUsed::Integer,
    genesisInfoTimestamp::UTCTime,
    genesisInfoExtraData::Integer,
    genesisInfoMixHash::SHA,
    genesisInfoNonce::Word64
} deriving (Show)

instance FromJSON SHA where
  parseJSON (String s) =
    case B16.decode $ BC.pack $ T.unpack s of
      (x, "") -> SHA <$> (return $ bytesToWord256 $ B.unpack x)
      _ -> error "bad format when calling FromJSON for SHA"
  parseJSON _ = undefined
   
instance ToJSON SHA where
  toJSON (SHA _) = undefined

instance FromJSON Word160 where
  parseJSON (String s) =
    case B16.decode $ BC.pack $ T.unpack s of
      (x, "") -> return $ bytesToWord160 $ B.unpack x
      _ -> error "bad format when calling FromJSON for Word160"
  parseJSON _ = undefined

instance ToJSON Word160 where
  toJSON = undefined

instance FromJSON GenesisInfo where
  parseJSON (Object o) =
    GenesisInfo <$>
    o .: "parentHash" <*>
    (return $ SHA 0xc0) <*> --UnclesHash"
    o .: "coinbase" <*>
    o .: "AccountInfo" <*>
    (return emptyTriePtr) <*> --TransactionsRoot
    (return emptyTriePtr) <*> --ReceiptsRoot
    (return $ B.replicate 256 0) <*> --LogBloom
    o .: "difficulty" <*>
    (return 0) <*> --number
    o .: "gasLimit" <*>
    (return 0) <*> --gasUsed
    o .: "timestamp" <*>
    o .: "extraData" <*>
    o .: "mixhash" <*>
    o .: "nonce"
  parseJSON _ = undefined
  
instance ToJSON GenesisInfo where
  toJSON (GenesisInfo ph _ cb ai _ _ _ d _ gl _ ts ed mh n) =
    object [
    "parentHash" .= ph,
    "coinbase" .= cb,
    "AccountInfo" .= ai,
    "difficulty" .= d,
    "gasLimit" .= gl,
    "timestamp" .= ts,
    "extraData" .= ed,
    "mixhash" .= mh,
    "nonce" .= n
    ]

