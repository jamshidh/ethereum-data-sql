{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Blockchain.Data.GenesisInfo (
  GenesisInfo(..)
  ) where

import qualified Data.ByteString as B
import Data.Time
import Data.Time.Clock.POSIX
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
}
