{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Blockchain.Data.SignedTransaction (
  SignedTransaction(..),
  txHash
  ) where

import Data.Aeson

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH

import qualified Data.Binary as BN

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC


import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Util
import Blockchain.Data.Address
import Blockchain.Data.Code
import Blockchain.SHA

import Data.Aeson
import GHC.Generics
       
  
data SignedTransaction =
  SignedTransaction {
      unsignedTransaction::Transaction,
      v::BN.Word8,
      r::Integer,
      s::Integer
    } deriving (Show, Read, Eq, Generic)

derivePersistField "SignedTransaction"

instance ToJSON SignedTransaction
instance FromJSON SignedTransaction

txHash :: SignedTransaction -> SHA
txHash =  hash . rlpSerialize . rlpEncode 

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
