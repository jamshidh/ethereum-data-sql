{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.Setup (
  oneTimeSetup
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Database.LevelDB as DB
import qualified Database.Persist.Postgresql as SQL
import System.Directory
import System.FilePath


import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.Data.DataDefs
import Blockchain.Data.GenesisBlock
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import Blockchain.DB.SQLDB
import Blockchain.Constants

data SetupDBs =
  SetupDBs {
    stateDB::StateDB,
    hashDB::HashDB,
    codeDB::CodeDB,
    sqlDB::SQLDB
    }

type SetupDBM = StateT SetupDBs (ResourceT IO)
instance HasStateDB SetupDBM where
  getStateDB = do
    cxt <- get
    return $ stateDB cxt
  setStateDBStateRoot sr = do
    cxt <- get
    put cxt{stateDB=(stateDB cxt){MP.stateRoot=sr}}

{-instance HasStorageDB SetupDBM where
  getStorageDB = do
    cxt <- get
    return $ MPDB.ldb $ setupDBStateDB cxt --storage and states use the same database!-}

instance HasHashDB SetupDBM where
  getHashDB = fmap hashDB get

{-
instance HasBlockDB SetupDBM where
  getBlockDB = fmap setupDBBlockDB get-}

instance HasCodeDB SetupDBM where
  getCodeDB = fmap codeDB get

instance HasSQLDB SetupDBM where
  getSQLDB = fmap sqlDB get


connStr::SQL.ConnectionString
connStr = "host=localhost dbname=eth user=postgres password=api port=5432"

oneTimeSetup::IO ()
oneTimeSetup = do
  _ <-
    runResourceT $ do
      homeDir <- liftIO getHomeDirectory                     
      sdb <- DB.open (homeDir </> dbDir "h" ++ stateDBPath)
             DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
      let hdb = sdb
          cdb = sdb
      sqldb <-   runNoLoggingT  $ SQL.createPostgresqlPool connStr 20
      SQL.runSqlPool (SQL.runMigration migrateAll) sqldb

      flip runStateT (SetupDBs
                           MP.MPDB{MP.ldb=sdb}
                           hdb
                           cdb
                           sqldb) $ do
        initializeGenesisBlock
        addCode B.empty --blank code is the default for Accounts, but gets added nowhere else.

  return ()

