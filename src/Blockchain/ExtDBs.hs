
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
  putKeyVal,
  getKeyVals,
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
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.NibbleString as N
import Blockchain.Data.RLP
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.SHA
import Blockchain.Util

import Blockchain.DBM

--import Debug.Trace

detailsDBPut::B.ByteString->B.ByteString->DBM ()
detailsDBPut key val = do
  ctx <- get
  runResourceT $ 
    DB.put (detailsDB ctx) def key val
    
detailsDBGet::B.ByteString->DBM (Maybe B.ByteString)
detailsDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (detailsDB ctx) def key
    
blockDBPut::B.ByteString->B.ByteString->DBM ()
blockDBPut key val = do
  ctx <- get
  runResourceT $ 
    DB.put (blockDB ctx) def key val
    
blockDBGet::B.ByteString->DBM (Maybe B.ByteString)
blockDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (blockDB ctx) def key


codeDBPut::B.ByteString->DBM ()
codeDBPut code = do
  ctx <- get
  runResourceT $ 
    DB.put (codeDB ctx) def (BL.toStrict $ encode $ hash code) code
    

codeDBGet::B.ByteString->DBM (Maybe B.ByteString)
codeDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (codeDB ctx) def key
    
stateDBPut::B.ByteString->B.ByteString->DBM ()
stateDBPut key val = do
  ctx <- get
  runResourceT $ 
    DB.put (MP.ldb $ stateDB ctx) def key val
  put ctx{stateDB=(stateDB ctx){MP.stateRoot=MP.SHAPtr key}}

stateDBGet::B.ByteString->DBM (Maybe B.ByteString)
stateDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (MP.ldb $ stateDB ctx) def key
    

putKeyVal::N.NibbleString->RLPObject->DBM ()
putKeyVal key val = do
  ctx <- get
  newStateDB <-
    liftIO $ runResourceT $ MP.putKeyVal (stateDB ctx) key val
  put ctx{stateDB=newStateDB}

getKeyVals::N.NibbleString->DBM [(N.NibbleString, RLPObject)]
getKeyVals key = do
  ctx <- get
  liftIO $ runResourceT $ MP.getKeyVals (stateDB ctx) key

deleteKey::N.NibbleString->DBM ()
deleteKey key = do
  ctx <- get
  newStateDB <-
    liftIO $ runResourceT $ MP.deleteKey (stateDB ctx) key
  put ctx{stateDB=newStateDB}

keyExists::N.NibbleString->DBM Bool
keyExists key = do
  ctx <- get
  liftIO $ runResourceT $ MP.keyExists (stateDB ctx) key
