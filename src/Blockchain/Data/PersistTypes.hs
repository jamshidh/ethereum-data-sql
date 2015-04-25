{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.PersistTypes where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Types

import Blockchain.Data.Transaction
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.SHA
import Blockchain.Database.MerklePatricia

import qualified Data.ByteString as BS

-- may change later to something sensible
derivePersistField "Integer"
derivePersistField "Transaction"

instance PersistField SHAPtr where
  toPersistValue (SHAPtr s) = PersistByteString $ s
  fromPersistValue (PersistByteString s) = Right $ SHAPtr $ s
  fromPersistValue _ = Left $ "SHAPtr must be persisted as PersistByteString"

instance PersistFieldSql SHAPtr where
  sqlType _ = SqlString

instance PersistField SHA where
  toPersistValue (SHA s) = PersistByteString $ BS.pack $ integer2Bytes $ (fromIntegral $ s :: Integer)
  fromPersistValue (PersistByteString s) = Right $ SHA $ (fromIntegral $ (bytes2Integer $ BS.unpack $  s) :: Word256)
  fromPersistValue _ = Left $ "SHA must be persisted as PersistByteString"

instance PersistFieldSql SHA where
  sqlType _ = SqlString

