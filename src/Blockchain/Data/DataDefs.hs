{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Blockchain.Data.DataDefs (
  BlockData (..),
  Block (..),
  BlockRef (..),
  AddressState (..),
  SignedTX (..),
  migrateAll
  ) where

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH
import Database.Persist.Postgresql

import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString as B

import Blockchain.Data.Address
import Blockchain.SHA
import Blockchain.Data.SignedTransaction
import Blockchain.Util

import Blockchain.Database.MerklePatricia.SHAPtr



--import Debug.Trace



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlockData
    parentHash SHA
    unclesHash SHA
    coinbase Address
    stateRoot SHAPtr
    transactionsRoot SHAPtr
    receiptsRoot SHAPtr
    logBloom B.ByteString
    difficulty Integer
    number Integer
    gasLimit Integer
    gasUsed Integer
    timestamp UTCTime
    extraData Integer
    nonce SHA
    deriving Show Read Eq

Block
    blockData BlockData
    receiptTransactions [SignedTransaction]
    blockUncles [BlockData]
    deriving Show Read Eq

BlockRef
    hash SHA
    block Block
    deriving Show Read Eq

AddressState
    nonce Integer
    balance Integer
    contractRoot SHAPtr
    codeHash SHA
    deriving Show Read Eq

SignedTX
    hash SHA
    transaction SignedTransaction
    deriving Show Read Eq
|]
