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
  AddressState (..),
  AddressStateRef (..),
  RawTransaction (..),
  Address (..),
  SHA (..),
  entityDefs,
  migrateAll 
  ) where

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH
import Database.Persist.Postgresql
import Database.Persist.Quasi

import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString as B

import Blockchain.Data.Address
import Blockchain.Database.MerklePatricia
import Blockchain.Data.Transaction
import Blockchain.Data.PersistTypes

import Data.Aeson
import Data.Aeson.Types
import Data.Text.Encoding
import Control.Applicative
import qualified Data.ByteString as BS

import Blockchain.SHA
import Blockchain.ExtWord
import Data.Word

import Blockchain.Data.MiscJSON

entityDefs :: [EntityDef]
entityDefs = $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
    $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")


