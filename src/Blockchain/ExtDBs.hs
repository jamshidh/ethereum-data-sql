
module Blockchain.ExtDBs (
  MP.SHAPtr(..),
  MP.emptyTriePtr,
  detailsDBPut,
  detailsDBGet,
  blockDBGet,
  blockDBPut,
  codeDBGet,
  codeDBPut,
  stateDBPut,
  stateDBGet,
  hashDBPut,
  hashDBGet,
  putKeyVal,
  getKeyVal,
  getAllKeyVals,
  keyExists,
  deleteKey
  ) where

import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Binary hiding (get, put)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import qualified Database.LevelDB as DB
import Network.Haskoin.Internals

import qualified Data.NibbleString as N
import Blockchain.Data.RLP
import Blockchain.DBM
import qualified Blockchain.Database.MerklePatricia as MP
import qualified Blockchain.Database.MerklePatricia.Internal as MPI
import Blockchain.SHA
import Blockchain.Util

import Blockchain.DBM

detailsDBPut::HasDetailsDB m=>B.ByteString->B.ByteString->m ()
detailsDBPut key val = do
  db <- getDetailsDB
  DB.put db def key val
    
detailsDBGet::HasDetailsDB m=>B.ByteString->m (Maybe B.ByteString)
detailsDBGet key = do
  db <- getDetailsDB
  DB.get db def key
    
blockDBPut::HasBlockDB m=>B.ByteString->B.ByteString->m ()
blockDBPut key val = do
  db <- getBlockDB
  DB.put db def key val
    
blockDBGet::HasBlockDB m=>B.ByteString->m (Maybe B.ByteString)
blockDBGet key = do
  db <- getBlockDB
  DB.get db def key


codeDBPut::HasCodeDB m=>B.ByteString->m ()
codeDBPut code = do
  db <- getCodeDB
  DB.put db def (BL.toStrict $ encode $ hash code) code
    

codeDBGet::HasCodeDB m=>B.ByteString->m (Maybe B.ByteString)
codeDBGet key = do
  db <- getCodeDB
  DB.get db def key

hashDBPut::HasHashDB m=>N.NibbleString->m ()
hashDBPut unsafeKey = do
  db <- getHashDB
  DB.put db def
    (nibbleString2ByteString $ MPI.keyToSafeKey unsafeKey)
    (nibbleString2ByteString unsafeKey)

hashDBGet::HasHashDB m=>N.NibbleString->m (Maybe N.NibbleString)
hashDBGet key = do
  db <- getHashDB
  liftM (fmap byteString2NibbleString) $
    DB.get db def (nibbleString2ByteString key)

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
  MPI.unsafeGetAllKeyVals db

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
