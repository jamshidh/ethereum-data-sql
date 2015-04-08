{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
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
    
module Blockchain.Data.DataDefs (
  BlockData (..),
  BlockDataRef (..),
  Block (..),
  BlockRef (..),
  AddressState (..),
  AddressStateRef (..),
  SignedTX (..),
  SHA (..),
  Address (..),
  SHAPtr (..),
  Word160 (..),
  Address (..),
  entityDefs,
  migrateAll 
  ) where

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH
import Database.Persist.Postgresql
import Database.Persist.Quasi
 
--import Database.Persist.EntityDef
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString as B

import Blockchain.Data.Address
import Blockchain.SHA
import Blockchain.ExtWord
import Blockchain.Data.SignedTransaction
import Blockchain.Util

import Blockchain.Database.MerklePatricia

-- import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text.Encoding
import Control.Applicative


--import Debug.Trace
instance ToJSON SHAPtr where
  toJSON (SHAPtr s) = toJSON s
  
instance FromJSON SHAPtr where
  parseJSON (String t) = pure . SHAPtr $ encodeUtf8 $ t
  parseJSON v          = typeMismatch "SHAPtr" v
         
  
entityDefs :: [EntityDef]
entityDefs = $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")
  
share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
    $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")


