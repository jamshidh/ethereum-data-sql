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
  addressStateExists
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

import qualified Control.Monad.State as ST
import Control.Monad.Trans.Resource
       
import qualified Data.NibbleString as N

{-
share [ mkPersist sqlSettings ]
    DD.entityDefs
-}


blankAddressState:: AddressState
blankAddressState = AddressState { addressStateNonce=0, addressStateBalance=0, addressStateContractRoot=emptyTriePtr, addressStateCodeHash=hash "" }


instance Format AddressState where
  format a = CL.blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ addressStateBalance a) ++ 
                 "\ncontractRoot: " ++ show (pretty $ addressStateContractRoot a) ++
                 "\ncodeHash: " ++ show (pretty $ addressStateCodeHash a))
  
instance RLPSerializable AddressState where
  --rlpEncode a | balance a < 0 = rlpEncode a{balance = - balance a}
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

addressAsNibbleString::Address->N.NibbleString
addressAsNibbleString (Address s) = N.EvenNibbleString $ BL.toStrict $ encode s

getAddressState::Address->DBM AddressState
getAddressState address = do
  states <- getKeyVals $ addressAsNibbleString address
  case states of
    [] -> do
      putAddressState address blankAddressState
      return blankAddressState
    [state] -> return $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd state
    _ -> error ("getAddressStates found multiple states for: " ++ show (pretty address) ++ "\n" ++ intercalate "\n" (show . pretty <$> states))
  

getAllAddressStates::DBM [(N.NibbleString, AddressState)]
getAllAddressStates = do
  states <- getKeyVals ""
  return $ fmap (rlpDecode . rlpDeserialize . rlpDecode) <$> states

putAddressState::Address->AddressState->DBM ()
putAddressState address newState =
  do
     notused <- putAddressStateSql address newState
     putKeyVal (addressAsNibbleString address) $ rlpEncode $ rlpSerialize $ rlpEncode newState

deleteAddressState::Address->DBM ()
deleteAddressState address = 
  deleteKey (addressAsNibbleString address)

addressStateExists::Address->DBM Bool
addressStateExists address = 
  keyExists (addressAsNibbleString address)

putAddressStateSql ::Address -> AddressState -> DBM (Key AddressStateRef )
putAddressStateSql addr state = do
  ctx <- ST.get
  runResourceT $
    SQL.runSqlPool actions $ sqlDB $ ctx
  where actions = do
   {-         oldAddressStateId <- SQL.selectFirst [ AddressStateRefAddress SQL.==. addr ] [ LimitTo 1 ]
            case oldAddressStateId of
              (Just oaId) -> SQL.replace (entityKey $ oaId) $ aRef
              _ -> SQL.insert_ $ aRef
    -}
            SQL.insert $ aRef
          
        aRef = AddressStateRef addr nonce bal cRoot cHash
        nonce = addressStateNonce (state)
        bal = addressStateBalance (state)
        cRoot = addressStateContractRoot (state)
        cHash = addressStateCodeHash (state)

  
