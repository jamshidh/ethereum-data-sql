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
{-# LANGUAGE ScopedTypeVariables        #-}
    
module Blockchain.Data.Address (
  Address(..),
  prvKey2Address,
  pubKey2Address,
  getNewAddress_unsafe,
  addressAsNibbleString,
  addressFromNibbleString,
  formatAddressWithoutColor
  ) where

import Control.Monad
import Control.Applicative
       
import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.NibbleString as N
import Data.Maybe
import Network.Haskoin.Crypto hiding (Address)
import Network.Haskoin.Internals hiding (Address)
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.ExtWord

import qualified Database.Persist as P 
import Database.Persist.Types
import Database.Persist.TH

import qualified Data.Text as T

import qualified Data.Aeson as AS
import Data.Aeson.Types
       
import GHC.Generics
import qualified Blockchain.Colors as C
import Blockchain.Format
import Blockchain.SHA
import Blockchain.SHA
import Blockchain.Util
import Web.PathPieces

newtype Address = Address Word160 deriving (Show, Eq, Read, Enum, Real, Bounded, Num, Ord, Generic, Integral)

{-
 Was necessary to make Address a primary key - which we no longer do (but rather index on the address field).
 May remove in the future
-}
instance PathPiece Address where
  toPathPiece (Address x) = T.pack $ showHex  (fromIntegral $ x :: Integer) ""
  fromPathPiece t = Just (Address wd160)
    where
      ((wd160, _):_) = readHex $ T.unpack $ t ::  [(Word160,String)]

{-
 make into a string rather than an object
-}                   
instance AS.ToJSON Address where
  toJSON (Address x) = AS.object [ "address" AS..= (showHex x "") ]
         
instance AS.FromJSON Address where
  parseJSON (Object v) = pure $ Address hex
    where
      ((hex, _):_) = readHex $ prsd :: [(Word160,String)]
      (Success prsd) = parse (.: "address") v
  parseJSON _ = mzero
   
instance Pretty Address where
  pretty (Address x) = yellow $ text $ padZeros 40 $ showHex x ""

instance Format Address where
  format (Address x) = C.yellow $ padZeros 40 $ showHex x ""

instance Binary Address where
  put (Address x) = sequence_ $ fmap put $ word160ToBytes $ fromIntegral x
  get = do
    bytes <- replicateM 20 get
    let byteString = B.pack bytes
    return (Address $ fromInteger $ byteString2Integer byteString)


prvKey2Address::PrvKey->Address
prvKey2Address prvKey =
  Address $ fromInteger $ byteString2Integer $ C.hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  --B16.encode $ hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  where
    PubKey point = derivePubKey prvKey
    x = fromMaybe (error "getX failed in prvKey2Address") $ getX point
    y = fromMaybe (error "getY failed in prvKey2Address") $ getY point

pubKey2Address::PubKey->Address
pubKey2Address (PubKey point) =
  Address $ fromInteger $ byteString2Integer $ C.hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  --B16.encode $ hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  where
    x = fromMaybe (error "getX failed in prvKey2Address") $ getX point
    y = fromMaybe (error "getY failed in prvKey2Address") $ getY point
pubKey2Address (PubKeyU _) = error "Missing case in pubKey2Address: PubKeyU"


instance RLPSerializable Address where
  rlpEncode (Address a) = RLPString $ BLC.unpack $ encode a
  rlpDecode (RLPString s) = Address $ decode $ BLC.pack s
  rlpDecode x = error ("Malformed rlp object sent to rlp2Address: " ++ show x)

getNewAddress_unsafe ::Address->Integer->Address
getNewAddress_unsafe a n =
    let theHash = hash $ rlpSerialize $ RLPArray [rlpEncode a, rlpEncode n]
    in decode $ BL.drop 12 $ encode theHash


addressAsNibbleString::Address->N.NibbleString
addressAsNibbleString (Address s) =
  byteString2NibbleString $ BL.toStrict $ encode s

addressFromNibbleString::N.NibbleString->Address
addressFromNibbleString = Address . decode . BL.fromStrict . nibbleString2ByteString 

formatAddressWithoutColor::Address->String
formatAddressWithoutColor (Address x) = padZeros 40 $ showHex x ""
