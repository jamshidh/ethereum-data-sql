{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Blockchain.Data.GenesisBlock (
                      initializeGenesisBlock,
                      initializeStateDB
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as B
import Data.Time
import Data.Time.Clock.POSIX

import Blockchain.Database.MerklePatricia

import Blockchain.Constants
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.CanonicalGenesis
import Blockchain.Data.GenesisInfo
import Blockchain.Data.StablenetGenesis
import Blockchain.Data.TestnetGenesis
import Blockchain.Data.DiffDB
import Blockchain.DB.BlockDB
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import Blockchain.DB.SQLDB
import Blockchain.ExtWord
import Blockchain.SHA

--import Debug.Trace

initializeBlankStateDB::HasStateDB m=>
                        m ()
initializeBlankStateDB = do
  db <- getStateDB
  liftIO $ runResourceT $
         initializeBlank db
  setStateDBStateRoot emptyTriePtr

initializeStateDB::(HasStateDB m, HasHashDB m)=>
                   [(Word160, Integer)]->m ()
initializeStateDB addressInfo = do
  initializeBlankStateDB
  
  forM_ addressInfo $ \(address, balance) ->
    putAddressState (Address address) blankAddressState{addressStateBalance=balance}




genesisInfoToGenesisBlock::(HasStateDB m, HasHashDB m)=>
                           GenesisInfo->m Block
genesisInfoToGenesisBlock gi = do
  initializeStateDB $ genesisInfoAccountInfo gi
  db <- getStateDB
  return $
    Block {
      blockBlockData =
         BlockData {
           blockDataParentHash = genesisInfoParentHash gi,
           blockDataUnclesHash = genesisInfoUnclesHash gi, 
           blockDataCoinbase = genesisInfoCoinbase gi,
           blockDataStateRoot = stateRoot db, 
           blockDataTransactionsRoot = genesisInfoTransactionsRoot gi, 
           blockDataReceiptsRoot = genesisInfoReceiptsRoot gi, 
           blockDataLogBloom = genesisInfoLogBloom gi, 
           blockDataDifficulty = genesisInfoDifficulty gi, 
           blockDataNumber = genesisInfoNumber gi, 
           blockDataGasLimit = genesisInfoGasLimit gi, 
           blockDataGasUsed = genesisInfoGasUsed gi, 
           blockDataTimestamp = genesisInfoTimestamp gi, 
           blockDataExtraData = genesisInfoExtraData gi, 
           blockDataMixHash = genesisInfoMixHash gi, 
           blockDataNonce = genesisInfoNonce gi
           },
      blockReceiptTransactions=[],
      blockBlockUncles=[]
      }
         


initializeGenesisBlock::(HasStateDB m, HasCodeDB m, HasSQLDB m, HasHashDB m)=>
                        Bool->m Block
initializeGenesisBlock altGenBlock = do
  genesisBlock <- genesisInfoToGenesisBlock canonicalGenesisInfo
  genBlkId <- putBlock genesisBlock
  genAddrStates <- getAllAddressStates
  let diffFromPair (addr', addrS) = CreateAddr addr' addrS
  commitSqlDiffs genBlkId 0 $ map diffFromPair genAddrStates

  return genesisBlock




