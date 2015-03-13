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

module Blockchain.Data.AddressState (
  AddressState(..),
  ) where

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH

import Data.Binary
import Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Address
import qualified Blockchain.Colors as CL
import Blockchain.Format
import qualified Data.NibbleString as N
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Util

import Blockchain.Database.MerklePatricia.SHAPtr
--import Debug.Trace

-- newtype SHAPtr = SHAPtr B.ByteString deriving (Show, Eq, Read)

data AddressState = AddressState { addressStateNonce::Integer, balance::Integer, contractRoot::SHAPtr, codeHash::SHA } deriving (Show, Read, Eq)

derivePersistField "AddressState"

