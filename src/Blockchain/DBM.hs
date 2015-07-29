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
  DBsLite(..),
  DBMLite,
  --setStateRoot,
  getStateRoot,
  openDBs,
  openDBsLite
  ) where


import qualified Database.LevelDB as DB

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import System.Directory
import System.FilePath

import           Control.Monad.Logger    (runNoLoggingT)
import qualified Database.Persist.Postgresql as SQL

import Blockchain.Constants

import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.BlockDB
import Blockchain.DB.CodeDB
import Blockchain.DB.DetailsDB
import Blockchain.DB.HashDB
import Blockchain.DB.SQLDB
import Blockchain.DB.StateDB

--import Debug.Trace

data DBs =
  DBs {
    blockDB'::BlockDB,
    detailsDB'::DetailsDB,
    stateDB'::MP.MPDB,
    codeDB'::CodeDB,
    hashDB'::HashDB,
    sqlDB'::SQLDB
    }

data DBsLite =
  DBsLite {
     sqlDBLite :: SQLDB
     }

type DBMLite = StateT DBsLite (ResourceT IO)

connStr::SQL.ConnectionString
connStr = "host=localhost dbname=eth user=postgres password=api port=5432"

options::DB.Options
options = DB.defaultOptions {DB.createIfMissing=True, DB.cacheSize=1024}

openDBs::(MonadResource m, MonadBaseControl IO m)=>String->m DBs
openDBs theType = do
  homeDir <- liftIO getHomeDirectory                     
  liftIO $ createDirectoryIfMissing False $ homeDir </> dbDir theType
  bdb <- DB.open (homeDir </> dbDir theType ++ blockDBPath) options
  ddb <- DB.open (homeDir </> dbDir theType ++ detailsDBPath) options
  sdb <- DB.open (homeDir </> dbDir theType ++ stateDBPath) options
  sqldb <-   runNoLoggingT  $ SQL.createPostgresqlPool connStr 20
  SQL.runSqlPool (SQL.runMigration migrateAll) sqldb
  return $ DBs
      bdb
      ddb
      MP.MPDB{ MP.ldb=sdb, MP.stateRoot=error "no stateRoot defined"}
      sdb
      sdb
      sqldb

openDBsLite :: SQL.ConnectionString -> ResourceT IO DBsLite
openDBsLite connectionString = do
  sqldb <- runNoLoggingT  $ SQL.createPostgresqlPool connectionString 20
  SQL.runSqlPool (SQL.runMigration migrateAll) sqldb
  return $ DBsLite sqldb
