
module Blockchain.ExtDBs (
  MP.SHAPtr(..),
  MP.emptyTriePtr,
  stateDBPut,
  stateDBGet,
  putKeyVal,
  getKeyVal,
  getAllKeyVals,
  keyExists,
  deleteKey,
  putStorageKeyVal',
  deleteStorageKey',
  getStorageKeyVal',
  getAllStorageKeyVals'
  ) where

import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Default
import qualified Database.LevelDB as DB

import qualified Data.NibbleString as N
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.RLP
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import Blockchain.DB.StorageDB
import qualified Blockchain.Database.MerklePatricia as MP
import qualified Blockchain.Database.MerklePatricia.Internal as MP
import Blockchain.ExtWord

  
stateDBPut::HasStateDB m=>B.ByteString->B.ByteString->m ()
stateDBPut key val = do
  db <- getStateDB
  DB.put (MP.ldb db) def key val
  setStateDBStateRoot $ MP.SHAPtr key

stateDBGet::HasStateDB m=>B.ByteString->m (Maybe B.ByteString)
stateDBGet key = do
  db <- getStateDB
  DB.get (MP.ldb db) def key

putKeyVal::HasStateDB m=>N.NibbleString->RLPObject->m ()
putKeyVal key val = do
  db <- getStateDB
  newStateDB <-
    liftIO $ runResourceT $ MP.putKeyVal db key val
  setStateDBStateRoot $ MP.stateRoot newStateDB

getAllKeyVals::HasStateDB m=>m [(N.NibbleString, RLPObject)]
getAllKeyVals = do
  db <- getStateDB
  MP.unsafeGetAllKeyVals db

getKeyVal::HasStateDB m=>N.NibbleString -> m (Maybe RLPObject)
getKeyVal key = do
  db <- getStateDB
  MP.getKeyVal db key

deleteKey::HasStateDB m=>N.NibbleString->m ()
deleteKey key = do
  db <- getStateDB
  newStateDB <-
    MP.deleteKey db key
  setStateDBStateRoot $ MP.stateRoot newStateDB

keyExists::HasStateDB m=>N.NibbleString->m Bool
keyExists key = do
  db <- getStateDB
  MP.keyExists db key

----


putStorageKeyVal'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                  Address->Word256->Word256->m ()
putStorageKeyVal' owner key val = do
  hashDBPut storageKeyNibbles
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MP.stateRoot $ MP.putKeyVal mpdb storageKeyNibbles (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}
  where storageKeyNibbles = N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key

deleteStorageKey'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m ()
deleteStorageKey' owner key = do
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MP.stateRoot $ MP.deleteKey mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}

getStorageKeyVal'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m Word256
getStorageKeyVal' owner key = do
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  maybeVal <- MP.getKeyVal mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  case maybeVal of
    Nothing -> return 0
    Just x -> return $ fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode x

getAllStorageKeyVals'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                       Address->m [(MP.Key, Word256)]
getAllStorageKeyVals' owner = do
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  kvs <- MP.unsafeGetAllKeyVals mpdb
  return $ map (fmap $ fromInteger . rlpDecode . rlpDeserialize . rlpDecode) kvs
