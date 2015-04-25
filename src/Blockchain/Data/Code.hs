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
{-# LANGUAGE DeriveGeneric              #-}

    
    
module Blockchain.Data.Code where

import Blockchain.Data.RLP
import Blockchain.Format

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
       
import Database.Persist
import Database.Persist.Types
import Database.Persist.TH

import GHC.Generics 
import Data.Aeson
import Data.Aeson.Types
import Data.Text.Encoding 
import Control.Applicative
       
newtype Code = Code{codeBytes::B.ByteString} deriving (Show, Eq, Read, Generic)

instance RLPSerializable Code where
    rlpEncode (Code bytes) = rlpEncode bytes
    rlpDecode = Code . rlpDecode

-- instance Format Code where
--    format Code {codeBytes=c} = B.unpack c
