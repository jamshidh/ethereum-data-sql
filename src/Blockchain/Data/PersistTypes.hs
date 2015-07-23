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

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import Data.Text.Encoding

import Blockchain.Handshake
import Crypto.Types.PubKey.ECC
import Numeric

derivePersistField "Transaction"

integerCap = 2^17

showHexFixed :: (Integral a, Show a) => Int -> a -> String
showHexFixed len val = padZeros $ showHex val ""
    where padZeros s = if length s >= len then s else padZeros ('0' : s)

instance PersistField Integer where
  toPersistValue i = PersistText . T.pack $ show i
  fromPersistValue (PersistText s) = Right $ read $ T.unpack s
  fromPersistValue x = Left $ T.pack $ "PersistField Integer: expected integer: " ++ (show x)

instance PersistFieldSql Integer where
  sqlType _ = SqlNumeric integerCap 0

instance PersistField Word256 where
  toPersistValue i = PersistText . T.pack $ showHexFixed 64 (fromIntegral i :: Integer) 
  fromPersistValue (PersistText s) = Right $ (fromIntegral $ ((fst . head .  readHex $ T.unpack s) :: Integer) :: Word256)
  fromPersistValue x = Left $ T.pack $ "PersistField Word256: expected integer: " ++ (show x)

instance PersistFieldSql Word256 where
  sqlType _ = SqlOther $ T.pack "varchar(64)" 

instance PersistField SHAPtr where
  toPersistValue (SHAPtr s) = PersistText . decodeUtf8 . B16.encode $ s
  fromPersistValue (PersistText s) = Right . SHAPtr . fst . B16.decode . encodeUtf8 $ s
  fromPersistValue _ = Left $ "SHAPtr must be persisted as PersistText"

instance PersistFieldSql SHAPtr where
  sqlType _ = SqlOther $ T.pack "varchar(64)" 

instance PersistField Point where
  toPersistValue p@(Point p1 p2) = PersistText . decodeUtf8 . B16.encode $ B.pack $ pointToBytes p
  fromPersistValue (PersistText s) = Right . bytesToPoint . B.unpack . fst . B16.decode . encodeUtf8 $ s
  fromPersistValue _ = Left $ "Point must be persisted as PersistText"

instance PersistField SHA where
  toPersistValue (SHA i) = PersistText . T.pack $ showHexFixed 64 i
  fromPersistValue (PersistText s) = Right $ SHA $ (fromIntegral $ ((fst . head .  readHex $ T.unpack s) :: Integer) :: Word256)
  fromPersistValue x = Left $ T.pack $ "PersistField SHA must be persisted as PersistText"

instance PersistFieldSql SHA where
  sqlType _ = SqlOther $ T.pack "varchar(64)" 

-- maybe do better here

