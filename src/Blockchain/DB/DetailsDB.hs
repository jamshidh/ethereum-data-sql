{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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
import Data.List
import Data.Maybe
import qualified Database.Esqueleto as E
import qualified Database.LevelDB as DB
import qualified Database.Persist as P
import qualified Database.Persist.Sql as SQL
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
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

getBestBlockHash::(HasHashDB m, HasStateDB m, HasCodeDB m, HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>
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
    [] -> blockHash <$> initializeGenesisBlock
  
getGenesisBlockHash::(HasCodeDB m, HasHashDB m, HasSQLDB m, HasStateDB m)=>
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
    [] -> blockHash <$> initializeGenesisBlock

getBestBlock::(HasCodeDB m, HasHashDB m, HasSQLDB m, HasStateDB m)=>
              m Block
getBestBlock = do
  bestBlockHash <- getBestBlockHash
  bestBlock <- getBlockLite bestBlockHash
  return $ fromMaybe (error $ "Missing block in database: " ++ show (pretty bestBlockHash)) bestBlock

