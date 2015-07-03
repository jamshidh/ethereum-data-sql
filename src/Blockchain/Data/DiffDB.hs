module Blockchain.Data.DiffDB (
  sqlDiff,
  commitSqlDiffs,
  AddrDiffOp(..),
  StorageDiffOp(..)
  ) where

import Database.Persist hiding (get)
import Database.Persist.Class
import Database.Persist.Types
import Database.Persist.TH
import qualified Database.Persist.Postgresql as SQL hiding (get)

import Blockchain.Database.MerklePatricia.Internal
import qualified Blockchain.Database.MerklePatricia.Diff as Diff
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.DB.CodeDB
import Blockchain.DBM
import Blockchain.ExtWord
import Blockchain.Util

import Control.Monad.State as ST hiding (state)
import Control.Monad.Trans.Resource
import qualified Data.NibbleString as N

import Debug.Trace

type SqlDbM = SQL.SqlPersistT (ResourceT DBM)

sqlDiff :: BlockDataRefId -> Integer -> SHAPtr -> SHAPtr -> DBM ()
sqlDiff blkDataId blkNum oldAddrs newAddrs = do
  ctx <- ST.get
  kvs <- lift $ unsafeGetAllKeyVals $ stateDB ctx
  diffAddrs <- addrDbDiff (stateDB ctx) oldAddrs newAddrs
  commitSqlDiffs blkDataId blkNum diffAddrs

commitSqlDiffs :: BlockDataRefId -> Integer -> [AddrDiffOp] -> DBM ()
commitSqlDiffs blkDataId blkNum diffAddrs = do
  ctx <- ST.get
  runResourceT $ SQL.runSqlPool (mapM_ (commitAddr blkDataId blkNum) diffAddrs) (sqlDB ctx)

data AddrDiffOp =
  CreateAddr { addr :: Address, state :: AddressState } |
  DeleteAddr { addr :: Address } |
  UpdateAddr { addr :: Address, oldState :: AddressState, newState :: AddressState }
  deriving (Show)

addrDbDiff :: MPDB -> SHAPtr -> SHAPtr -> DBM [AddrDiffOp]
addrDbDiff db ptr1 ptr2 = do
  diffs <- lift $ Diff.dbDiff db ptr1 ptr2
  mapM addrConvert diffs

addrConvert :: Diff.DiffOp -> DBM AddrDiffOp

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

commitAddr :: BlockDataRefId -> Integer -> AddrDiffOp -> SqlDbM ()

commitAddr blkDataId blkNum CreateAddr{ addr = addr, state = addrS } = do
  Just code <- lift $ lift $ getCode ch
  let aRef = AddressStateRef addr n b cr code blkDataId blkNum -- Should use Lens here, no doubt
  addrID <- SQL.insert aRef
  ctx <- lift $ lift ST.get
  addrStorageKVs <- lift $ lift $ lift $
                    unsafeGetAllKeyVals (stateDB ctx){stateRoot = addrStorageRoot}
  realStorageKVs <- lift $ lift $ mapM decodeKV addrStorageKVs
  mapM_ (SQL.insert . storageOfKV addrID) realStorageKVs
  where
    AddressState n b cr ch = addrS
    addrStorageRoot = cr
    decodeKV (k,v) = storageConvert $ Diff.Create (N.unpack k) v
    storageOfKV addrID (CreateStorage k v)= Storage addrID k v

commitAddr _ _ DeleteAddr{ addr = addr } = do
  addrID <- getAddressStateSQL addr "delete"
  SQL.deleteWhere [ StorageAddressStateRefId SQL.==. addrID ]
  SQL.delete addrID

commitAddr blkDataId blkNum UpdateAddr{ addr = addr, oldState = addrS1, newState = addrS2 } = do
  addrID <- getAddressStateSQL addr "update"
  SQL.update addrID [ AddressStateRefNonce =. n, AddressStateRefBalance =. b,
                      AddressStateRefLatestBlockDataRefId =. blkDataId,
                      AddressStateRefLatestBlockDataRefNumber =. blkNum]
  ctx <- lift $ lift ST.get
  storageDiff <- lift $ lift $ storageDbDiff (stateDB ctx) oldAddrStorage newAddrStorage
  mapM_ (commitStorage addrID) (trace ("Address " ++ formatAddressWithoutColor addr ++ ": " ++ show storageDiff) storageDiff)
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

storageDbDiff :: MPDB -> SHAPtr -> SHAPtr -> DBM [StorageDiffOp]
storageDbDiff db ptr1 ptr2 = do
  diffs <- lift $ Diff.dbDiff db ptr1 ptr2
  mapM storageConvert diffs

storageConvert :: Diff.DiffOp -> DBM StorageDiffOp

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

commitStorage :: SQL.Key AddressStateRef -> StorageDiffOp -> SqlDbM ()

commitStorage addrID CreateStorage{ key = storageKey, val = storageVal } = do
  SQL.insert $ Storage addrID storageKey storageVal
  return ()

commitStorage addrID DeleteStorage{ key = storageKey } = do
  storageID <- getStorageKeySQL addrID storageKey "delete"
  SQL.delete storageID

commitStorage addrID UpdateStorage{ key = storageKey, oldVal = _, newVal = storageVal} = do
  storageID <- getStorageKeySQL addrID storageKey "update"
  SQL.update storageID [ StorageValue =. storageVal ]
  return ()

getAddressStateSQL :: Address -> String -> SqlDbM (SQL.Key AddressStateRef)
getAddressStateSQL addr s = do
  addrIDs <- SQL.selectKeysList
              [ AddressStateRefAddress SQL.==. addr ] [ LimitTo 1 ]
  if null addrIDs
    then error $ s ++ ": Address not found in SQL db: " ++ formatAddressWithoutColor addr
    else return $ head addrIDs

getStorageKeySQL :: (SQL.Key AddressStateRef) -> Word256 -> String -> SqlDbM (SQL.Key Storage)
getStorageKeySQL addrID storageKey s = do
  storageIDs <- SQL.selectKeysList
              [ StorageAddressStateRefId SQL.==. addrID, StorageKey SQL.==. storageKey ]
              [ LimitTo 1 ]
  if null storageIDs
    then error $ s ++ ": Storage key not found in SQL db: " ++ showHex4 storageKey
    else return $ head storageIDs
