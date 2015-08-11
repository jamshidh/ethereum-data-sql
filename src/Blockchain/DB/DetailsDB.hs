{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.DB.DetailsDB (
  HasDetailsDB(..),
  DetailsDB,
  detailsDBPut,
  detailsDBGet,
  getBestBlockHash,
  getGenesisBlockHash,
  getBestBlock,
  getBestProcessedBlock
  ) where

import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Default
import Data.Maybe
import qualified Database.Esqueleto as E
import qualified Database.LevelDB as DB
import qualified Database.Persist.Sql as SQL

import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.DB.HashDB
import Blockchain.DB.SQLDB
import Blockchain.Format
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

getBestBlockHash::(HasHashDB m, HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>
                  m SHA
getBestBlockHash = do
  db <- getSQLDB
  ret <-
    runResourceT $
    flip SQL.runSqlPool db $ do
      E.select $ E.from $ \a -> do
        E.limit 1
        E.orderBy [E.desc (a E.^. BlockDataRefTotalDifficulty)]
        return $ a E.^. BlockDataRefHash
  case ret of
    [x] -> return $ E.unValue x
    [] -> error "Ethereum DBs are blank, you need to set them up by running 'ethereum-setup'"
    _ -> error "getBestBlockHash can't handle a tie yet, yet that is what we have."

getBestProcessedBlockHash::(HasHashDB m, HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>
                  m SHA
getBestProcessedBlockHash = do
  db <- getSQLDB
  ret <-
    runResourceT $
    flip SQL.runSqlPool db $ do
      E.select $ E.from $ \(bd `E.InnerJoin` processed) -> do
        E.on (bd E.^. BlockDataRefBlockId E.==. processed E.^. ProcessedBlockId)
        E.limit 1
        E.orderBy [E.desc (bd E.^. BlockDataRefTotalDifficulty)]
        return $ bd E.^. BlockDataRefHash
  case ret of
    [x] -> return $ E.unValue x
    [] -> error "Ethereum DBs are blank, you need to set them up by running 'ethereum-setup'"
    _ -> error "getBestBlockHash can't handle a tie yet, yet that is what we have."

getGenesisBlockHash::(HasHashDB m, HasSQLDB m)=>
                     m SHA
getGenesisBlockHash = do
  db <- getSQLDB
  ret <-
    runResourceT $
    flip SQL.runSqlPool db $ do
      E.select $ E.from $ \a -> do
        E.where_ (a E.^. BlockDataRefNumber E.==. E.val 0)
        return $ a E.^. BlockDataRefHash
  case ret of
    [x] -> return $ E.unValue x
    [] -> error "Ethereum DBs are blank, you need to set them up by running 'ethereum-setup'"
    _ -> error "getGenesisBlockHash called, but there are multiple genesis blocks!  This is an error."

getBestBlock::(HasHashDB m, HasSQLDB m)=>
              m Block
getBestBlock = do
  bestBlockHash <- getBestBlockHash
  bestBlock <- getBlockLite bestBlockHash
  return $ fromMaybe (error $ "Missing block in database: " ++ format (bestBlockHash)) bestBlock

getBestProcessedBlock::(HasHashDB m, HasSQLDB m)=>
              m Block
getBestProcessedBlock = do
  bestBlockHash <- getBestProcessedBlockHash
  bestBlock <- getBlockLite bestBlockHash
  return $ fromMaybe (error $ "Missing block in database: " ++ format (bestBlockHash)) bestBlock

