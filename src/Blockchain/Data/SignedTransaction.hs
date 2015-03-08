{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Blockchain.Data.SignedTransaction (
  SignedTransaction(..),
  ) where

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH

import Data.Binary
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Util
import Blockchain.Data.Address
import Blockchain.Data.Code

--import Debug.Trace

{-
data SignedTX =
  SignedTX {
    tNonce :: Integer,
    gasPrice :: Integer,
    tGasLimit :: Integer,
    to  :: Address,
    value :: Integer,
    tcd :: CodeOrData,
    v  :: Word8,
    r :: Integer,
    s :: Integer
   } deriving (Show, Read, Eq)
-}
-- derivePersistField "SignedTX"
  
data SignedTransaction =
  SignedTransaction {
      unsignedTransaction::Transaction,
      v::Word8,
      r::Integer,
      s::Integer
    } deriving (Show, Read, Eq)

derivePersistField "SignedTransaction"

instance Format SignedTransaction where
  format SignedTransaction{unsignedTransaction = x, v=v', r=r', s=s'} =
      CL.blue "Transaction" ++
           tab (
                "\n" ++
                format x ++
                "v: " ++ show v' ++ "\n" ++
                "r: " ++ show r' ++ "\n" ++
                "s: " ++ show s' ++ "\n")

instance RLPSerializable SignedTransaction where
  rlpDecode (RLPArray [n, gp, gl, toAddr, val, i, vVal, rVal, sVal]) =
    SignedTransaction {
      unsignedTransaction=rlpDecode $ RLPArray [n, gp, gl, toAddr, val, i],
      v = fromInteger $ rlpDecode vVal,
      r = rlpDecode rVal,
      s = rlpDecode sVal
      }
  rlpDecode x = error ("rlpDecode for Transaction called on non block object: " ++ show x)

  rlpEncode t =
      RLPArray [
        n, gp, gl, toAddr, val, i,
        rlpEncode $ toInteger $ v t,
        rlpEncode $ r t,
        rlpEncode $ s t
        ]
      where
        (RLPArray [n, gp, gl, toAddr, val, i]) = rlpEncode (unsignedTransaction t)

{-
instance Format SignedTX where
  format (SignedTX nonce gasprice gaslimit toAddr val cd v' r' s') =
      case cd of
          TCode init ->
              CL.blue "Transaction" ++
                 tab (
                    "\n" ++
                    "gasPrice" ++ show gasprice ++ "\n" ++
                    "tGasLimit" ++ show gaslimit ++ "\n" ++
                    "value" ++ show val ++ "\n" ++
                    "tInit" ++ show init ++ "\n" ++
                    "v: " ++ show v' ++ "\n" ++
                    "r: " ++ show r' ++ "\n" ++
                    "s: " ++ show s' ++ "\n")
          TData dat ->
              CL.blue "Transaction" ++
                 tab (
                    "\n" ++
                    "gasPrice" ++ show gasprice ++ "\n" ++
                    "tGasLimit" ++ show gaslimit ++ "\n" ++
                    "to" ++ show (pretty toAddr) ++ "\n" ++
                    "value" ++ show val ++ "\n" ++
                    "tData" ++ "\n" ++ show dat  ++ "\n" ++
                    "v: " ++ show v' ++ "\n" ++
                    "r: " ++ show r' ++ "\n" ++
                    "s: " ++ show s' ++ "\n")
   

instance RLPSerializable SignedTX where
    rlpDecode (RLPArray [n, gp, gl, RLPString "", val, i, vVal, rVal, sVal]) =
        SignedTX 
           (rlpDecode n)
           (rlpDecode gp)
           (rlpDecode gl)
           (Address 0)
           (rlpDecode val)
           (TCode (rlpDecode i))
           (fromInteger $ rlpDecode vVal)
           (rlpDecode rVal)
           (rlpDecode sVal)
        

    rlpDecode (RLPArray [n, gp, gl, toAddr, val, i, vVal, rVal, sVal]) =
        SignedTX 
          (rlpDecode n)
          (rlpDecode gp)
          (rlpDecode gl)
          (rlpDecode toAddr)
          (rlpDecode val)
          (TData (rlpDecode i))
          (fromInteger $ rlpDecode vVal)
          (rlpDecode rVal)
          (rlpDecode sVal)
        

    rlpDecode x = error ("rlpDecode for Transaction called on non block object: " ++ show x)

    rlpEncode (SignedTX n gp gl to val cd v' r' s') =
      case cd of
        TCode init -> 
          RLPArray [
              rlpEncode n,
              rlpEncode gp,
              rlpEncode gl,
              rlpEncode (0 :: Integer),
              rlpEncode val,
              rlpEncode init,
              rlpEncode $ toInteger $ v',
              rlpEncode r',
              rlpEncode s'
          ]
 
        TData dat ->
          RLPArray [
              rlpEncode n,
              rlpEncode gp,
              rlpEncode gl,
              rlpEncode to,
              rlpEncode val,
              rlpEncode dat,
              rlpEncode $ toInteger $ v',
              rlpEncode r',
              rlpEncode s'
          ]
-}
