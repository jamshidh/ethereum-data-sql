
module Blockchain.DB.CodeDB (
               addCode,
               getCode
              ) where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.SHA
import Blockchain.Database.MerklePatricia

addCode::B.ByteString->DBM ()
addCode = codeDBPut

getCode::SHA->DBM (Maybe B.ByteString)
getCode theHash = 
  codeDBGet (BL.toStrict $ encode $ sha2SHAPtr theHash)
