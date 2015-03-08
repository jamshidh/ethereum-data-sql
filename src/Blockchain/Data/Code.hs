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

module Blockchain.Data.Code where

import Blockchain.Data.RLP
import Blockchain.Format

import qualified Data.ByteString as B

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH

newtype Code = Code{codeBytes::B.ByteString} deriving (Show, Eq, Read)

data CodeOrData = TCode Code | TData Integer deriving (Eq, Read, Show)

derivePersistField "CodeOrData"

instance RLPSerializable Code where
    rlpEncode (Code bytes) = rlpEncode bytes
    rlpDecode = Code . rlpDecode

-- instance Format Code where
--    format Code {codeBytes=c} = B.unpack c
