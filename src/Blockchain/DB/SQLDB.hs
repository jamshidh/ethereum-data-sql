{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Blockchain.DB.SQLDB (
  HasSQLDB(..),
  SQLDB,
  sqlQuery
  ) where

import Control.Monad.Trans.Resource
import qualified Database.Persist.Postgresql as SQL

type SQLDB = SQL.ConnectionPool

class MonadBaseControl IO m=>
      HasSQLDB m where
  getSQLDB::Monad m=>m SQLDB

sqlQuery::(HasSQLDB m, MonadResourceBase m)=>
          SQL.SqlPersistT (ResourceT m) a->m a
sqlQuery q = do
  db <- getSQLDB
  runResourceT $
               SQL.runSqlPool q db

       
