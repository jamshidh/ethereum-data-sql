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


import Database.Persist hiding (get)
import Database.Persist.Class
import Database.Persist.Types
import Database.Persist.TH
import Database.Persist.Postgresql as SQL hiding (get)

import Blockchain.DBM
import Blockchain.Data.Address
import qualified Blockchain.Colors as CL

import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.BlockDB
import Blockchain.Data.Transaction
import Blockchain.Data.DataDefs

import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Time
import qualified Data.ByteString as B

import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Control.Monad.State as ST
import Control.Monad.Trans.Resource
       
import qualified Data.NibbleString as N

blankAddressState:: AddressState
blankAddressState = AddressState { addressStateNonce=0, addressStateBalance=0, addressStateContractRoot=emptyTriePtr, addressStateCodeHash=hash "" }


instance Format AddressState where
  format a = CL.blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ addressStateBalance a) ++ 
                 "\ncontractRoot: " ++ show (pretty $ addressStateContractRoot a) ++
                 "\ncodeHash: " ++ show (pretty $ addressStateCodeHash a))
  
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

getAddressState::Address->DBM AddressState
getAddressState address = do
    states <- getKeyVal $ addressAsNibbleString address
    case states of
      Nothing -> do
        -- Querying an absent state counts as initializing it.
        putAddressState address b
        return b
        where b = blankAddressState
      Just s -> return $ (rlpDecode . rlpDeserialize . rlpDecode) s
        
getAllAddressStates::DBM [(Address, AddressState)]
getAllAddressStates = do
    states <- getAllKeyVals
    return $ map convert $ states
    where
      convert::(N.NibbleString, RLPObject)->(Address, AddressState)
      convert (k, v) = (Address $ fromInteger $ byteString2Integer $ nibbleString2ByteString k, rlpDecode . rlpDeserialize . rlpDecode $ v)

getAddressFromHash :: N.NibbleString -> DBM (Maybe Address)
getAddressFromHash =
  liftM (fmap addressFromNibbleString) . hashDBGet

getStorageKeyFromHash :: N.NibbleString -> DBM (Maybe Word256)
getStorageKeyFromHash  =
  liftM (fmap (decode . BL.fromStrict . nibbleString2ByteString) ) . hashDBGet  

putAddressState::Address->AddressState->DBM ()
putAddressState address newState = do
  hashDBPut addrNibbles
  putKeyVal addrNibbles $ rlpEncode $ rlpSerialize $ rlpEncode newState
  where addrNibbles = addressAsNibbleString address

deleteAddressState::Address->DBM ()
deleteAddressState address = 
  deleteKey (addressAsNibbleString address)

addressStateExists::Address->DBM Bool
addressStateExists address = 
  keyExists (addressAsNibbleString address)
