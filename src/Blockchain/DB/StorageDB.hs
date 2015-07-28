
module Blockchain.DB.StorageDB (
  HasStorageDB(..)
  ) where

import Control.Monad.Trans.Resource
import qualified Database.LevelDB as DB

class MonadResource m=>
      HasStorageDB m where
  getStorageDB::Monad m=>m DB.DB

