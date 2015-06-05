{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Crypto.Types.PubKey.ECC
-- these derived instances may later change

derivePersistField "Integer"
derivePersistField "Transaction"
derivePersistField "SHA"
derivePersistField "SHAPtr"
derivePersistField "Point"
derivePersistField "Word256"

{-
showHexFixed :: (Integral a, Show a) => Int -> a -> String
showHexFixed len val = padZeros $ showHex val ""
    where padZeros s = if length s >= len then s else padZeros ('0' : s)

instance PersistField Integer where
  toPersistValue i = PersistText . T.pack $ showHexFixed 66 i
  fromPersistValue (PersistText s) = read $ "0x" <> T.unpack s
  fromPersistValue x = Left $ T.pack "PersistField Integer: Expected hexadecimal char(66), received: " <> T.pack (show x)

instance PersistFieldSql Integer where
  sqlType _ = SqlOther $ T.pack "char(66)" -- fixed length character string, stored inline
-}
{-
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
-}
