
module Blockchain.DB.StateDB (
  HasStateDB(..),
  getStateRoot
  ) where

import Control.Monad.Trans.Resource

import Blockchain.Database.MerklePatricia

class MonadResource m=>
      HasStateDB m where
  getStateDB::Monad m=>m MPDB
  setStateDBStateRoot::Monad m=>SHAPtr->m ()


getStateRoot::HasStateDB m=>m SHAPtr
getStateRoot = do
  db <- getStateDB
  return $ stateRoot db

