{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
    
module Blockchain.Data.Transaction (
  Transaction(..)
  ) where


import Database.Persist
import Database.Persist.Types
import Database.Persist.TH

import qualified Data.ByteString as B
import Text.PrettyPrint.ANSI.Leijen

import Blockchain.Data.Address
import Blockchain.Data.Code
import qualified Blockchain.Colors as CL
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.Util

import Data.Aeson
import qualified GHC.Generics as G


--import Debug.Trace

data Transaction =
  MessageTX {
    tNonce::Integer,
    gasPrice::Integer,
    tGasLimit::Integer,
    to::Address,
    value::Integer,
    tData::B.ByteString
    } |
  ContractCreationTX {
    tNonce::Integer,
    gasPrice::Integer,
    tGasLimit::Integer,
    value::Integer,
    tInit::Code
    } deriving (Show, Read, Eq, G.Generic)


instance ToJSON Transaction
instance FromJSON Transaction
  
instance Format Transaction where
  format MessageTX{tNonce=n, gasPrice=gp, tGasLimit=gl, to=to', value=v, tData=d} =
    CL.blue "Message Transaction" ++
    tab (
      "\n" ++
      "tNonce: " ++ show n ++ "\n" ++
      "gasPrice: " ++ show gp ++ "\n" ++
      "tGasLimit: " ++ show gl ++ "\n" ++
      "to: " ++ show (pretty to') ++ "\n" ++
      "value: " ++ show v ++ "\n" ++
      "tData: " ++ tab ("\n" ++ format d) ++ "\n")
  format ContractCreationTX{tNonce=n, gasPrice=gp, tGasLimit=gl, value=v, tInit=Code init'} =
    CL.blue "Contract Creation Transaction" ++
    tab (
      "\n" ++
      "tNonce: " ++ show n ++ "\n" ++
      "gasPrice: " ++ show gp ++ "\n" ++
      "tGasLimit: " ++ show gl ++ "\n" ++
      "value: " ++ show v ++ "\n" ++
      "tInit: " ++ tab (format init') ++ "\n")

instance RLPSerializable Transaction where
  rlpDecode (RLPArray [n, gp, gl, RLPString "", val, i]) = --Note- Address 0 /= Address 000000....  Only Address 0 yields a ContractCreationTX
    ContractCreationTX {
      tNonce = rlpDecode n,
      gasPrice = rlpDecode gp,
      tGasLimit = rlpDecode gl,
      value = rlpDecode val,
      tInit = rlpDecode i
      }
  rlpDecode (RLPArray [n, gp, gl, toAddr, val, i]) =
    MessageTX {
      tNonce = rlpDecode n,
      gasPrice = rlpDecode gp,
      tGasLimit = rlpDecode gl,
      to = rlpDecode toAddr,
      value = rlpDecode val,
      tData = rlpDecode i
      }
  rlpDecode x = error ("rlpDecode for Transaction called on non block object: " ++ show x)

  rlpEncode MessageTX{tNonce=n, gasPrice=gp, tGasLimit=gl, to=to', value=v, tData=d} =
      RLPArray [
        rlpEncode n,
        rlpEncode gp,
        rlpEncode gl,
        rlpEncode to',
        rlpEncode v,
        rlpEncode d
        ]
  rlpEncode ContractCreationTX{tNonce=n, gasPrice=gp, tGasLimit=gl, value=v, tInit=init'} =
      RLPArray [
        rlpEncode n,
        rlpEncode gp,
        rlpEncode gl,
        rlpEncode (0::Integer),
        rlpEncode v,
        rlpEncode init'
        ]
    
