module Blockchain.Data.DiffDB (sqlDiff) where

import Database.Persist hiding (get)
import Database.Persist.Class
import Database.Persist.Types
import Database.Persist.TH
import qualified Database.Persist.Postgresql as SQL hiding (get)

import Blockchain.Database.MerklePatricia.Internal
import Blockchain.Database.MerklePatricia.Diff as Diff
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.DB.CodeDB
import Blockchain.DBM
import Blockchain.ExtWord
import Blockchain.Util

import Control.Monad.State as ST
import Control.Monad.Trans.Resource
import qualified Data.NibbleString as N

type SqlDbM = SQL.SqlPersistT (ResourceT DBM)

sqlDiff :: SHAPtr -> SHAPtr -> DBM ()
sqlDiff oldAddrs newAddrs = do
  ctx <- ST.get
  diffAddrs <- lift $ dbDiff (stateDB ctx) oldAddrs newAddrs
  runResourceT $ SQL.runSqlPool (mapM_ commitAddr diffAddrs) (sqlDB ctx)

commitAddr :: DiffOp -> SqlDbM ()

commitAddr Create{ key = nl, val = addrRLP } = do
  Just addr <- lift $ lift $ getAddressFromHash (N.pack nl)
  Just code <- lift $ lift $ getCode ch
  let aRef = AddressStateRef addr n b cr code -- Should use Lens here, no doubt
  addrID <- SQL.insert aRef
  ctx <- lift $ lift ST.get
  addrStorageKVs <- lift $ lift $ lift $
                    unsafeGetAllKeyVals (stateDB ctx){stateRoot = addrStorageRoot}
  realStorageKVs <- lift $ lift $ mapM decodeKV addrStorageKVs
  mapM_ (SQL.insert . storageOfKV addrID) realStorageKVs
  where
    AddressState n b cr ch = rlpDecode $ rlpDeserialize $ rlpDecode addrRLP
    addrStorageRoot = cr
    decodeKV (k,v) = do
      Just realK <- getStorageKeyFromHash k
      return (realK, fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode v)
    storageOfKV addrID = uncurry (Storage addrID)

commitAddr Delete{ key = nl } = do
  Just addr <- lift $ lift $ getAddressFromHash (N.pack nl)
  addrID <- getAddressStateSQL addr "delete"
  SQL.deleteWhere [ StorageAddressStateRefId SQL.==. addrID ]
  SQL.delete addrID

commitAddr Diff.Update{ key = nl, oldVal = addrRLP1, newVal = addrRLP2 } = do
  Just addr <- lift $ lift $ getAddressFromHash (N.pack nl)
  addrID <- getAddressStateSQL addr "update"
  SQL.update addrID [ AddressStateRefNonce =. n, AddressStateRefBalance =. b ]
  ctx <- lift $ lift ST.get
  storageDiff <- lift $ lift $ lift $
                 dbDiff (stateDB ctx) oldAddrStorage newAddrStorage
  mapM_ (commitStorage addrID) storageDiff
  where
    AddressState _ _ cr1 _ = rlpDecode $ rlpDeserialize $ rlpDecode addrRLP1
    oldAddrStorage = cr1
    AddressState n b cr2 _ = rlpDecode $ rlpDeserialize $ rlpDecode addrRLP2
    newAddrStorage = cr2

commitStorage :: SQL.Key AddressStateRef -> DiffOp -> SqlDbM ()

commitStorage addrID Create{ key = nl, val = valRLP } = do
  Just storageKey <- lift $ lift $ getStorageKeyFromHash $ N.pack nl
  let storageVal = fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode valRLP
  SQL.insert $ Storage addrID storageKey storageVal
  return ()

commitStorage _ Delete{ key = nl } = do
  Just storageKey <- lift $ lift $ getStorageKeyFromHash $ N.pack nl
  storageID <- getStorageKeySQL storageKey "delete"
  SQL.delete storageID

commitStorage _ Diff.Update{ key = nl, oldVal = _, newVal = valRLP } = do
  Just storageKey <- lift $ lift $ getStorageKeyFromHash $ N.pack nl
  let storageVal = fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode valRLP
  storageID <- getStorageKeySQL storageKey "update"
  SQL.update storageID [ StorageValue =. storageVal ]
  return ()

getAddressStateSQL :: Address -> String -> SqlDbM (SQL.Key AddressStateRef)
getAddressStateSQL addr s = do
  addrIDs <- SQL.selectKeysList
              [ AddressStateRefAddress SQL.==. addr ] [ LimitTo 1 ]
  if null addrIDs
    then error $ s ++ ": Address not found in SQL db: " ++ formatAddressWithoutColor addr
    else return $ head addrIDs

getStorageKeySQL :: Word256 -> String -> SqlDbM (SQL.Key Storage)
getStorageKeySQL storageKey s = do
  storageIDs <- SQL.selectKeysList
              [ StorageKey SQL.==. storageKey ] [ LimitTo 1 ]
  if null storageIDs
    then error $ s ++ ": Storage key not found in SQL db: " ++ showHex4 storageKey
    else return $ head storageIDs
