{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

--TODO : Take this next line out
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.Data.AddressStateDB (
  AddressState(..),
  blankAddressState,
  getAddressState,
  getAllAddressStates,
  putAddressState,
  deleteAddressState,
  addressStateExists,
  getAddressFromHash,
  getStorageKeyFromHash
) where 

import Blockchain.Data.Address
import qualified Blockchain.Colors as CL

import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import qualified Blockchain.Database.MerklePatricia.Internal as MP

import Data.Binary
import qualified Data.ByteString.Lazy as BL

import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Control.Monad.State as ST
import Control.Monad.Trans.Resource
       
import qualified Data.NibbleString as N

blankAddressState:: AddressState
blankAddressState = AddressState { addressStateNonce=0, addressStateBalance=0, addressStateContractRoot=MP.emptyTriePtr, addressStateCodeHash=hash "" }


instance Format AddressState where
  format a = CL.blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ addressStateBalance a) ++ 
                 "\ncontractRoot: " ++ format (addressStateContractRoot a) ++
                 "\ncodeHash: " ++ format (addressStateCodeHash a))
  
instance RLPSerializable AddressState where
  rlpEncode a | addressStateBalance a < 0 = error $ "Error in cal to rlpEncode for AddressState: AddressState has negative balance: " ++ format a
  rlpEncode a = RLPArray [
    rlpEncode $ toInteger $ addressStateNonce a,
    rlpEncode $ toInteger $ addressStateBalance a,
    rlpEncode $ addressStateContractRoot a,
    rlpEncode $ addressStateCodeHash a
                ]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromInteger $ rlpDecode n,
      addressStateBalance=fromInteger $ rlpDecode b,
      addressStateContractRoot=rlpDecode cr,
      addressStateCodeHash=rlpDecode ch
      } 
  rlpDecode x = error $ "Missing case in rlpDecode for AddressState: " ++ show (pretty x)

getAddressState::(HasStateDB m, HasHashDB m)=>Address->m AddressState
getAddressState address = do
    db <- getStateDB
    states <- MP.getKeyVal db $ addressAsNibbleString address
    case states of
      Nothing -> do
        -- Querying an absent state counts as initializing it.
        putAddressState address b
        return b
        where b = blankAddressState
      Just s -> return $ (rlpDecode . rlpDeserialize . rlpDecode) s
        
getAllAddressStates::(HasHashDB m, HasStateDB m, MonadResource m)=>m [(Address, AddressState)]
getAllAddressStates = do
  sdb <- getStateDB
  mapM convert =<<  MP.unsafeGetAllKeyVals sdb
  where
    convert::(HasHashDB m, MonadResource m)=>(N.NibbleString, RLPObject)-> m (Address, AddressState)
    convert (k, v) = do
      Just k' <- getAddressFromHash k
      return (k', rlpDecode . rlpDeserialize . rlpDecode $ v)

getAddressFromHash::(HasHashDB m, MonadResource m)=>N.NibbleString -> m (Maybe Address)
getAddressFromHash =
  liftM (fmap addressFromNibbleString) . hashDBGet

getStorageKeyFromHash::(HasHashDB m, MonadResource m)=>N.NibbleString -> m (Maybe Word256)
getStorageKeyFromHash  =
  liftM (fmap (decode . BL.fromStrict . nibbleString2ByteString) ) . hashDBGet  

putAddressState::(HasStateDB m, HasHashDB m)=>Address->AddressState->m ()
putAddressState address newState = do
  hashDBPut addrNibbles
  db <- getStateDB
  
  db' <- MP.putKeyVal db addrNibbles $ rlpEncode $ rlpSerialize $ rlpEncode newState
  setStateDBStateRoot (MP.stateRoot db')
  where addrNibbles = addressAsNibbleString address

deleteAddressState::(HasStateDB m, MonadResource m)=>Address->m ()
deleteAddressState address = do
  db <- getStateDB
  db' <- MP.deleteKey db (addressAsNibbleString address)
  setStateDBStateRoot $ MP.stateRoot db'

addressStateExists::(HasStateDB m, MonadResource m)=>Address->m Bool
addressStateExists address = do
  db <- getStateDB
  MP.keyExists db (addressAsNibbleString address)
