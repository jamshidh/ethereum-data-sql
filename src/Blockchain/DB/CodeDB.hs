
module Blockchain.DB.CodeDB (
               addCode,
               getCode
              ) where

import Control.Monad.Trans.Resource
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.SHA
import Blockchain.Database.MerklePatricia

addCode::(HasCodeDB m, MonadResource m)=>B.ByteString->m ()
addCode = codeDBPut

getCode::(HasCodeDB m, MonadResource m)=>SHA->m (Maybe B.ByteString)
getCode theHash = 
  codeDBGet (BL.toStrict $ encode $ sha2SHAPtr theHash)
