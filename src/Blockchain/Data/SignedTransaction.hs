{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.SignedTransaction (
  SignedTransaction(..),
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Binary
import Data.ByteString.Internal
import Network.Haskoin.Internals hiding (Address)
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Address
import qualified Blockchain.Colors as CL
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Data.Transaction
import Blockchain.Util

--import Debug.Trace


data SignedTransaction =
  SignedTransaction {
      unsignedTransaction::Transaction,
      v::Word8,
      r::Integer,
      s::Integer
    } deriving (Show)

instance Format SignedTransaction where
  format t@SignedTransaction{unsignedTransaction = x, v=v', r=r', s=s'} =
      CL.blue "Transaction" ++
           tab (
                "\n" ++
                format x ++
                "v: " ++ show v' ++ "\n" ++
                "r: " ++ show r' ++ "\n" ++
                "s: " ++ show s' ++ "\n")

addLeadingZerosTo64::String->String
addLeadingZerosTo64 x = replicate (64 - length x) '0' ++ x

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
