{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Blockchain.DBM (
  DBs(..),
  DBM,
  setStateRoot,
  setStorageStateRoot,
  getStorageStateRoot,
  openDBs,
  DetailsDB,
  BlockDB
  ) where


import qualified Database.LevelDB as DB

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import System.Directory
import System.FilePath
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import           Control.Monad.Logger    (runStderrLoggingT)
import qualified Database.Persist            as P
import qualified Database.Persist.Postgresql as SQL
import           Database.Persist.TH

import Blockchain.Constants
import Blockchain.Database.MerklePatricia
import Blockchain.Data.Transaction
import Blockchain.Data.SignedTransaction
import Blockchain.Util
import Blockchain.Data.Address
import Blockchain.Data.Block
import Blockchain.Data.Sql
import Blockchain.Data.Code
import Blockchain.SHA

--import Debug.Trace

type BlockDB = DB.DB
type CodeDB = DB.DB
type DetailsDB = DB.DB
type StorageDB = MPDB
type SQLBlockDB = SQL.ConnectionPool
  

data DBs =
  DBs {
    blockDB::BlockDB,
    detailsDB::DetailsDB,
    stateDB::MPDB,
    codeDB::CodeDB,
    storageDB::StorageDB,
    sqlBlockDB::SQLBlockDB
    }

type DBM = StateT DBs IO

connStr = "host=localhost dbname=test user=kjameslubin password=test port=5432"

setStateRoot::SHAPtr->DBM ()
setStateRoot stateRoot' = do
  ctx <- get
  put ctx{stateDB=(stateDB ctx){stateRoot=stateRoot'}}

setStorageStateRoot::SHAPtr->DBM ()
setStorageStateRoot stateRoot' = do
  ctx <- get
  put ctx{storageDB=(storageDB ctx){stateRoot=stateRoot'}}

getStorageStateRoot::DBM SHAPtr
getStorageStateRoot = do
  ctx <- get
  return $ stateRoot $ storageDB ctx

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}

openDBs::String->ResourceT IO DBs
openDBs theType = do
  homeDir <- liftIO getHomeDirectory                     
  liftIO $ createDirectoryIfMissing False $ homeDir </> dbDir theType
  bdb <- DB.open (homeDir </> dbDir theType ++ blockDBPath) options
  ddb <- DB.open (homeDir </> dbDir theType ++ detailsDBPath) options
  sdb <- DB.open (homeDir </> dbDir theType ++ stateDBPath) options
  sqlBlock <-   runStderrLoggingT  $ SQL.createPostgresqlPool connStr 20
  SQL.runSqlPool (SQL.runMigration migrateAll) sqlBlock
  return $ DBs
      bdb
      ddb
      MPDB{ ldb=sdb, stateRoot=error "no stateRoot defined"}
      sdb
      MPDB{ ldb=sdb, stateRoot=SHAPtr B.empty} --error "no storage stateRoot defined"}
      sqlBlock
