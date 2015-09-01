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
    
module Blockchain.Data.RawTransaction (
  RawTransaction(..)
  ) where


import Blockchain.Data.DataDefs

{-
instance Format RawTransaction where
  format RawTransaction{rawTransactionNonce=n,
                        rawTransactionGasPrice=gp,
                        rawTransactionGasLimit=gl,
                        rawTransactionTo=(Just to'),
                        rawTransactionValue=val,
                        rawTransactionCodeOrData=d,
                        rawTransactionV=v,
                        rawTransactionR=r,
                        rawTransactionS=s} =
    CL.blue "Message Transaction" ++
    tab (
      "\n" ++
      "nonce: " ++ show n ++ "\n" ++
      "gasPrice: " ++ show gp ++ "\n" ++
      "gasLimit: " ++ show gl ++ "\n" ++
      "to: " ++ show (pretty to') ++ "\n" ++
      "value: " ++ show val ++ "\n" ++
      "data: " ++ tab ("\n" ++ format d) ++ "\n" ++
      "v" ++ show v ++ "\n" ++
      "r" ++ show r ++ "\n" ++
      "s" ++ show s ++ "\n"
      )
  format RawTransaction{rawTransactionNonce=n,
                        rawTransactionGasPrice=gp,
                        rawTransactionGasLimit=gl,
                        rawTransactionTo=Nothing,
                        rawTransactionValue=val,
                        rawTransactionCodeOrData=c,
                        rawTransactionV=v,
                        rawTransactionR=r,
                        rawTransactionS=s} =
    CL.blue "Contract Transaction" ++
    tab (
      "\n" ++
      "nonce: " ++ show n ++ "\n" ++
      "gasPrice: " ++ show gp ++ "\n" ++
      "gasLimit: " ++ show gl ++ "\n" ++
      "value: " ++ show val ++ "\n" ++
      "code: " ++ tab ("\n" ++ format c) ++ "\n" ++
      "v" ++ show v ++ "\n" ++
      "r" ++ show r ++ "\n" ++
      "s" ++ show s ++ "\n"
      )
-}
{-
instance RLPSerializable RawTransaction where
  rlpDecode (RLPArray [n, gp, gl, RLPString "", val, i, v, r, s]) = --Note- Address 0 /= Address 000000....  Only Address 0 yields a ContractCreationTX
    RawTransaction {
      rawTransactionNonce = rlpDecode n,
      rawTransactionGasPrice = rlpDecode gp,
      rawTransactionGasLimit = rlpDecode gl,
      rawTransactionTo=Nothing,
      rawTransactionValue = rlpDecode val,
      rawTransactionCodeOrData = rlpDecode i,
      rawTransactionV = rlpDecode v,
      rawTransactionR = rlpDecode r,
      rawTransactionS = rlpDecode s
      }
  rlpDecode (RLPArray [n, gp, gl, toAddr, val, d, v, r, s]) =
    RawTransaction {
      rawTransactionNonce = rlpDecode n,
      rawTransactionGasPrice = rlpDecode gp,
      rawTransactionGasLimit = rlpDecode gl,
      rawTransactionTo=(Just $ rlpDecode toAddr),
      rawTransactionValue = rlpDecode val,
      rawTransactionCodeOrData = rlpDecode d,
      rawTransactionV = rlpDecode v,
      rawTransactionR = rlpDecode r,
      rawTransactionS = rlpDecode s
      }
  rlpDecode x = error ("rlpDecode for RawTransaction called on non block object: " ++ show x)

  rlpEncode RawTransaction{rawTransactionNonce=n,
                           rawTransactionGasPrice=gp,
                           rawTransactionGasLimit=gl,
                           rawTransactionTo=Nothing,
                           rawTransactionValue=val,
                           rawTransactionCodeOrData=c,
                           rawTransactionV=v,
                           rawTransactionR=r,
                           rawTransactionS=s} =
      RLPArray [
        rlpEncode n,
        rlpEncode gp,
        rlpEncode gl,
        rlpEncode (0::Integer),
        rlpEncode val,
        rlpEncode c,
        rlpEncode v,
        rlpEncode r,
        rlpEncode s
        ]
      
  rlpEncode RawTransaction{rawTransactionNonce=n,
                           rawTransactionGasPrice=gp,
                           rawTransactionGasLimit=gl,
                           rawTransactionTo=Just to,
                           rawTransactionValue=val,
                           rawTransactionCodeOrData=d,
                           rawTransactionV=v,
                           rawTransactionR=r,
                           rawTransactionS=s} =
      RLPArray [
        rlpEncode n,
        rlpEncode gp,
        rlpEncode gl,
        rlpEncode to,
        rlpEncode v,
        rlpEncode d,
        rlpEncode v,
        rlpEncode r,
        rlpEncode s
        ]
  -}  
