
module Blockchain.Data.TransactionResult (
     putTransactionResult
    ) where

import Database.Persist hiding (get)
import Database.Persist.Types
import Database.Persist.TH
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E

import Control.Monad.State
import Control.Monad.Trans.Resource


import Blockchain.Data.DataDefs
import Blockchain.DBM

putTransactionResult::TransactionResult->DBM (Key TransactionResult)
putTransactionResult tr = do
  ctx <- get
  runResourceT $ SQL.runSqlPool (SQL.insert tr) $ sqlDB ctx 
