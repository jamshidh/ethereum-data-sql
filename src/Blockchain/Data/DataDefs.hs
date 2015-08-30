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
    
module Blockchain.Data.DataDefs where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Quasi

import Crypto.Types.PubKey.ECC
import Data.Text

import Data.Time

import Blockchain.Data.Address
import Blockchain.Database.MerklePatricia
import Blockchain.Data.Transaction
import Blockchain.Data.PersistTypes()

import qualified Data.ByteString as BS

import Blockchain.SHA
import Blockchain.ExtWord
import Data.Word

import Blockchain.Data.MiscJSON()

entityDefs :: [EntityDef]
entityDefs = $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")

share [mkPersist sqlSettings, mkMigrate "migrateAll"]  -- annoying: postgres doesn't like tables called user
    $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")


