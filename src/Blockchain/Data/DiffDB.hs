{-# LANGUAGE FlexibleContexts #-}

module Blockchain.Data.DiffDB (
  sqlDiff,
  commitSqlDiffs,
  AddrDiffOp(..), addrConvert, addrDbDiff,
  StorageDiffOp(..), storageConvert, storageDbDiff
  ) where

import Database.Persist hiding (get)
import qualified Database.Persist.Postgresql as SQL hiding (get)

import Blockchain.Database.MerklePatricia.Internal
import qualified Blockchain.Database.MerklePatricia.Diff as Diff
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.SQLDB
import Blockchain.DB.StateDB
import Blockchain.ExtWord
import Blockchain.Util

import Control.Monad.State as ST hiding (state)
import Control.Monad.Trans.Resource
import qualified Data.NibbleString as N

import Debug.Trace

type SqlDbM m = SQL.SqlPersistT m

sqlDiff :: (HasSQLDB m, HasCodeDB m, HasStateDB m, HasHashDB m, MonadResource m, MonadBaseControl IO m)=>
           BlockDataRefId -> Integer -> SHAPtr -> SHAPtr -> m ()
sqlDiff blkDataId blkNum oldAddrs newAddrs = do
  db <- getStateDB
  diffAddrs <- addrDbDiff db oldAddrs newAddrs
  commitSqlDiffs blkDataId blkNum diffAddrs

commitSqlDiffs :: (HasStateDB m, HasHashDB m, HasCodeDB m, HasSQLDB m, MonadResource m, MonadBaseControl IO m)=>
                  BlockDataRefId -> Integer -> [AddrDiffOp] -> m ()
commitSqlDiffs blkDataId blkNum diffAddrs = do
  pool <- getSQLDB
  SQL.runSqlPool (mapM_ (commitAddr blkDataId blkNum) diffAddrs) pool

data AddrDiffOp =
  CreateAddr { addr :: Address, state :: AddressState } |
  DeleteAddr { addr :: Address } |
  UpdateAddr { addr :: Address, oldState :: AddressState, newState :: AddressState }
  deriving (Show)

addrDbDiff :: (HasStateDB m, HasHashDB m, MonadResource m)=>MPDB -> SHAPtr -> SHAPtr -> m [AddrDiffOp]
addrDbDiff db ptr1 ptr2 = do
  diffs <- Diff.dbDiff db ptr1 ptr2
  mapM addrConvert diffs

addrConvert :: (HasStateDB m, HasHashDB m, MonadResource m)=>Diff.DiffOp -> m AddrDiffOp

addrConvert (Diff.Create k v) = do
  Just k' <- getAddressFromHash (N.pack k)
  let v' = rlpDecode $ rlpDeserialize $ rlpDecode v
  return $ CreateAddr k' v'

addrConvert (Diff.Delete k) = do
  Just k' <- getAddressFromHash (N.pack k)
  return $ DeleteAddr k'

addrConvert (Diff.Update k v1 v2) = do
  Just k' <- getAddressFromHash (N.pack k)
  let v1' = rlpDecode $ rlpDeserialize $ rlpDecode v1
      v2' = rlpDecode $ rlpDeserialize $ rlpDecode v2
  return $ UpdateAddr k' v1' v2'

commitAddr :: (HasStateDB m, HasHashDB m, HasCodeDB m, MonadResource m, MonadBaseControl IO m)=>
              BlockDataRefId -> Integer -> AddrDiffOp -> SQL.SqlPersistT m ()

commitAddr blkDataId blkNum CreateAddr{ addr = addr', state = addrS } = do
  Just code <- lift $ getCode ch
  let aRef = AddressStateRef addr' n b cr code blkDataId blkNum -- Should use Lens here, no doubt
  addrID <- SQL.insert aRef
  db <- lift getStateDB
  addrStorageKVs <- --lift $ lift $ lift $
                    unsafeGetAllKeyVals db{stateRoot = addrStorageRoot}
  realStorageKVs <- lift $ mapM decodeKV addrStorageKVs
  mapM_ (SQL.insert . storageOfKV addrID) realStorageKVs
  where
    AddressState n b cr ch = addrS
    addrStorageRoot = cr
    decodeKV (k,v) = storageConvert $ Diff.Create (N.unpack k) v
    storageOfKV addrID (CreateStorage k v)= Storage addrID k v
    storageOfKV _ _ = undefined "Ryan is confident this won't happen"

commitAddr _ _ DeleteAddr{ addr = addr' } = do
  addrID <- getAddressStateSQL addr' "delete"
  SQL.deleteWhere [ StorageAddressStateRefId SQL.==. addrID ]
  SQL.delete addrID

commitAddr blkDataId blkNum UpdateAddr{ addr = addr', oldState = addrS1, newState = addrS2 } = do
  addrID <- getAddressStateSQL addr' "update"
  SQL.update addrID [ AddressStateRefNonce =. n, AddressStateRefBalance =. b,
                      AddressStateRefLatestBlockDataRefId =. blkDataId,
                      AddressStateRefLatestBlockDataRefNumber =. blkNum]
  db <- lift getStateDB
  storageDiff <- lift $ storageDbDiff db oldAddrStorage newAddrStorage
  mapM_ (commitStorage addrID) (trace ("Address " ++ formatAddressWithoutColor addr' ++ ": " ++ show storageDiff) storageDiff)
  where
    AddressState _ _ cr1 _ = addrS1
    oldAddrStorage = cr1
    AddressState n b cr2 _ = addrS2
    newAddrStorage = cr2

data StorageDiffOp =
  CreateStorage { key :: Word256, val :: Word256 } |
  DeleteStorage { key :: Word256 } |
  UpdateStorage { key :: Word256, oldVal :: Word256, newVal :: Word256 }
  deriving (Show)

storageDbDiff :: (HasHashDB m, MonadResource m)=>MPDB -> SHAPtr -> SHAPtr -> m [StorageDiffOp]
storageDbDiff db ptr1 ptr2 = do
  diffs <- Diff.dbDiff db ptr1 ptr2
  mapM storageConvert diffs

storageConvert :: (HasHashDB m, MonadResource m)=>Diff.DiffOp -> m StorageDiffOp

storageConvert (Diff.Create k v) = do
  Just k' <- getStorageKeyFromHash $ N.pack k
  let v' = fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode v
  return $ CreateStorage k' v'

storageConvert (Diff.Delete k) = do
  Just k' <- getStorageKeyFromHash $ N.pack k
  return $ DeleteStorage k'

storageConvert (Diff.Update k v1 v2) = do
  Just k' <- getStorageKeyFromHash $ N.pack k
  let v1' = fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode v1
      v2' = fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode v2
  return $ UpdateStorage k' v1' v2'

commitStorage :: (HasStateDB m, HasHashDB m, MonadResource m)=>SQL.Key AddressStateRef -> StorageDiffOp -> SqlDbM m ()

commitStorage addrID CreateStorage{ key = storageKey', val = storageVal } = do
  _ <- SQL.insert $ Storage addrID storageKey' storageVal
  return ()

commitStorage addrID DeleteStorage{ key = storageKey' } = do
  storageID <- getStorageKeySQL addrID storageKey' "delete"
  SQL.delete storageID

commitStorage addrID UpdateStorage{ key = storageKey', oldVal = _, newVal = storageVal} = do
  storageID <- getStorageKeySQL addrID storageKey' "update"
  SQL.update storageID [ StorageValue =. storageVal ]
  return ()

getAddressStateSQL :: (HasStateDB m, HasHashDB m, MonadResource m)=>Address -> String -> SqlDbM m (SQL.Key AddressStateRef)
getAddressStateSQL addr' s = do
  addrIDs <- SQL.selectKeysList
              [ AddressStateRefAddress SQL.==. addr' ] [ LimitTo 1 ]
  if null addrIDs
    then error $ s ++ ": Address not found in SQL db: " ++ formatAddressWithoutColor addr'
    else return $ head addrIDs

getStorageKeySQL :: (HasStateDB m, HasHashDB m, MonadResource m)=>(SQL.Key AddressStateRef) -> Word256 -> String -> SqlDbM m (SQL.Key Storage)
getStorageKeySQL addrID storageKey' s = do
  storageIDs <- SQL.selectKeysList
              [ StorageAddressStateRefId SQL.==. addrID, StorageKey SQL.==. storageKey' ]
              [ LimitTo 1 ]
  if null storageIDs
    then error $ s ++ ": Storage key not found in SQL db: " ++ showHex4 storageKey'
    else return $ head storageIDs
