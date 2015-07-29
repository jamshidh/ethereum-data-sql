{-# LANGUAGE FlexibleContexts #-}

module Blockchain.DB.SQLDB (
  HasSQLDB(..),
  SQLDB
  ) where

import Control.Monad.Trans.Resource
import qualified Database.Persist.Postgresql as SQL

type SQLDB = SQL.ConnectionPool

class MonadBaseControl IO m=>
      HasSQLDB m where
  getSQLDB::Monad m=>m SQLDB

