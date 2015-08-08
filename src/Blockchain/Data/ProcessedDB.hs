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
import Database.Persist.Types
import Database.Persist.TH
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Persist.Sql as SQL
import qualified Database.Esqueleto as E

import qualified Crypto.Hash.SHA3 as C
import Data.Binary hiding (get,put)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Foreign.ForeignPtr.Unsafe
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Address
import qualified Blockchain.Colors as CL
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.BlockDB
import Blockchain.DB.SQLDB
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.DB.BlockDB
import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.RawTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.DataDefs
import Blockchain.Data.Code

import Control.Monad.State
import Control.Monad.Trans.Resource

putProcessed :: (HasSQLDB m, MonadIO m)=>
               Processed->m (Key Processed)
putProcessed p = do
  db <- getSQLDB
  runResourceT $ flip SQL.runSqlPool db $ SQL.insert p

--instance Format Processed where
--  format Processed{processedBlockId=blockId} = CL.yellow $ format blockId
