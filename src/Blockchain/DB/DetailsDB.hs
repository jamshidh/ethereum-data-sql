{-# LANGUAGE OverloadedStrings #-}

module Blockchain.DB.DetailsDB (
  HasDetailsDB(..),
  DetailsDB,
  detailsDBPut,
  detailsDBGet,
  getBestBlockHash,
  getGenesisBlockHash,
  getBestBlock
  ) where

import Control.Monad.Trans.Resource
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Functor
import Data.Maybe
import qualified Database.LevelDB as DB
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.BlockDB
import Blockchain.Data.GenesisBlock
import Blockchain.DB.BlockDB
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.SQLDB
import Blockchain.DB.StateDB
import Blockchain.SHA

type DetailsDB = DB.DB

class MonadResource m=>
      HasDetailsDB m where
  getDetailsDB::Monad m=>m DetailsDB

detailsDBPut::HasDetailsDB m=>
              B.ByteString->B.ByteString->m ()
detailsDBPut key val = do
  db <- getDetailsDB
  DB.put db def key val
    
detailsDBGet::HasDetailsDB m=>
              B.ByteString->m (Maybe B.ByteString)
detailsDBGet key = do
  db <- getDetailsDB
  DB.get db def key

getBestBlockHash::(HasDetailsDB m, HasCodeDB m, HasHashDB m, HasSQLDB m, HasStateDB m)=>
                  Bool->m SHA
getBestBlockHash altGenBlock = do
  maybeBestHash <- detailsDBGet "best"
  case maybeBestHash of
    Nothing -> do
      bhSHA <- getGenesisBlockHash altGenBlock
      detailsDBPut "best" $ BL.toStrict $ encode bhSHA
      return bhSHA
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getGenesisBlockHash::(HasDetailsDB m, HasCodeDB m, HasHashDB m, HasSQLDB m, HasStateDB m)=>
                     Bool->m SHA
getGenesisBlockHash altGenBlock = do
  maybeGenesisHash <- detailsDBGet "genesis"
  case maybeGenesisHash of
    Nothing -> do
      bhSHA <- blockHash <$> initializeGenesisBlock altGenBlock
      detailsDBPut "genesis" $ BL.toStrict $ encode bhSHA
      return bhSHA
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getBestBlock::(HasDetailsDB m, HasCodeDB m, HasHashDB m, HasSQLDB m, HasStateDB m)=>
              Bool->m Block
getBestBlock altGenBlock = do
  bestBlockHash <- getBestBlockHash altGenBlock
  bestBlock <- getBlockLite bestBlockHash
  return $ fromMaybe (error $ "Missing block in database: " ++ show (pretty bestBlockHash)) bestBlock

