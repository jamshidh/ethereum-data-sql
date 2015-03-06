
module Blockchain.DBM (
  DBs(..),
  DBM,
  setStateRoot,
  setStorageStateRoot,
  getStorageStateRoot,
  openDBs,
  DetailsDB,
  BlockDB
  ) where


import qualified Database.LevelDB as DB

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import System.Directory
import System.FilePath
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Constants
import Blockchain.Database.MerklePatricia

--import Debug.Trace

type BlockDB = DB.DB
type CodeDB = DB.DB
type DetailsDB = DB.DB
type StorageDB = MPDB

data DBs =
  DBs {
    blockDB::BlockDB,
    detailsDB::DetailsDB,
    stateDB::MPDB,
    codeDB::CodeDB,
    storageDB::StorageDB
    }

type DBM = StateT DBs IO

setStateRoot::SHAPtr->DBM ()
setStateRoot stateRoot' = do
  ctx <- get
  put ctx{stateDB=(stateDB ctx){stateRoot=stateRoot'}}

setStorageStateRoot::SHAPtr->DBM ()
setStorageStateRoot stateRoot' = do
  ctx <- get
  put ctx{storageDB=(storageDB ctx){stateRoot=stateRoot'}}

getStorageStateRoot::DBM SHAPtr
getStorageStateRoot = do
  ctx <- get
  return $ stateRoot $ storageDB ctx

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}

openDBs::String->ResourceT IO DBs
openDBs theType = do
  homeDir <- liftIO getHomeDirectory                     
  liftIO $ createDirectoryIfMissing False $ homeDir </> dbDir theType
  bdb <- DB.open (homeDir </> dbDir theType ++ blockDBPath) options
  ddb <- DB.open (homeDir </> dbDir theType ++ detailsDBPath) options
  sdb <- DB.open (homeDir </> dbDir theType ++ stateDBPath) options
  return $ DBs
      bdb
      ddb
      MPDB{ ldb=sdb, stateRoot=error "no stateRoot defined"}
      sdb
      MPDB{ ldb=sdb, stateRoot=SHAPtr B.empty} --error "no storage stateRoot defined"}
