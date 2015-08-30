{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Blockchain.Data.ProcessedDB (
  Block(..),
  putProcessed
) where 

import Database.Persist hiding (get)
import qualified Database.Persist.Postgresql as SQL

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs

import Control.Monad.State
import Control.Monad.Trans.Resource

putProcessed :: (HasSQLDB m, MonadIO m)=>
               Processed->m (Key Processed)
putProcessed p = do
  db <- getSQLDB
  runResourceT $ flip SQL.runSqlPool db $ SQL.insert p

--instance Format Processed where
--  format Processed{processedBlockId=blockId} = CL.yellow $ format blockId
