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
import Blockchain.Data.PersistTypes ()
import Blockchain.Data.MiscJSON ()
import Blockchain.Data.Transaction
<<<<<<< HEAD
import Blockchain.Database.MerklePatricia
=======
import Blockchain.Data.PersistTypes()
>>>>>>> f171de2eadd6636faa63373faa631b410a95e7fe

import qualified Data.ByteString as BS

import Blockchain.SHA
import Blockchain.ExtWord
import Data.Word

<<<<<<< HEAD
=======
import Blockchain.Data.MiscJSON()

>>>>>>> f171de2eadd6636faa63373faa631b410a95e7fe
entityDefs :: [EntityDef]
entityDefs = $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")

share [mkPersist sqlSettings, mkMigrate "migrateAll"]  -- annoying: postgres doesn't like tables called user
    $(persistFileWith lowerCaseSettings "src/Blockchain/Data/DataDefs.txt")


