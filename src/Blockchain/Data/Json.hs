{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--TODO : Take this next line out
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Blockchain.Data.Json where

import Control.Applicative

import Blockchain.Data.DataDefs
import Blockchain.Data.Address
import Blockchain.Data.Transaction
import Blockchain.Data.Code
import Blockchain.Database.MerklePatricia

import Data.Aeson
import qualified Data.ByteString.Base16 as B16
import Database.Persist.Postgresql
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B

import Numeric

--import Debug.Trace

import Data.Word
import Data.Maybe

jsonBlk :: (ToJSON a, Monad m) => a -> m Value
jsonBlk a = return . toJSON $ a

data RawTransaction' = RawTransaction' RawTransaction String deriving (Eq, Show)

{- fix these later -}
instance FromJSON Code
instance ToJSON Code 

instance ToJSON SHAPtr 
instance FromJSON SHAPtr
{- note we keep the file MiscJSON around for the instances we don't want to export - ByteString, Point -}

instance ToJSON RawTransaction' where
    toJSON (RawTransaction' rt@(RawTransaction (Address fa) non gp gl (Just (Address ta)) val cod r s v _ bn h) next) =
        object ["next" .= next, "from" .= showHex fa "", "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "to" .= showHex ta "" , "value" .= show val, "codeOrData" .= cod, 
        "r" .= showHex r "",
        "s" .= showHex s "",
        "v" .= showHex v "",
        "blockNumber" .= bn,
        "hash" .= h,
        "transactionType" .= (show $ rawTransactionSemantics rt)
               ]
    toJSON (RawTransaction' rt@(RawTransaction (Address fa) non gp gl Nothing val cod r s v _ bn h) next) =
        object ["next" .= next, "from" .= showHex fa "", "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "value" .= show val, "codeOrData" .= cod,
        "r" .= showHex r "",
        "s" .= showHex s "",
        "v" .= showHex v "",
        "blockNumber" .= bn,
        "hash" .= h,
        "transactionType" .= (show $ rawTransactionSemantics rt)
               ]

instance FromJSON RawTransaction' where
    parseJSON (Object t) = do
      fa <- fmap (fst . head . readHex) (t .: "from")
      (tnon :: Int)  <- (t .: "nonce")
      (tgp :: Int) <- (t .: "gasPrice")
      (tgl :: Int) <- (t .: "gasLimit")
      tto <- (t .:? "to")
      let toFld = case tto of
            (Just str) -> fmap (Address . fst . head . readHex) str
            Nothing -> Nothing
      tval <- fmap read (t .: "value")
      tcd <- fmap (fst .  B16.decode . T.encodeUtf8 ) (t .: "codeOrData")
      (tr :: Integer) <- fmap (fst . head . readHex) (t .: "r")
      (ts :: Integer) <- fmap (fst . head . readHex) (t .: "s")
      (tv :: Word8) <- fmap (fst . head . readHex) (t .: "v")
      mbid <- (t .:? "blockId")
      mbn <- (t .:? "blockNumber")
      h <- (t .: "hash")
      let bid = case mbid of
            Just bd -> bd
            Nothing -> 1 -- annoying, needs to be reset on update
          bn = case mbn of
            Just b -> b
            Nothing -> -1
      
      return (RawTransaction' (RawTransaction (Address fa)
                                              (fromIntegral tnon :: Integer)
                                              (fromIntegral $ tgp :: Integer)
                                              (fromIntegral $ tgl :: Integer)
                                              (toFld :: Maybe Address)
                                              (tval :: Integer)
                                              (tcd :: B.ByteString)
                                              (tr :: Integer)
                                              (ts :: Integer)
                                              (tv :: Word8)
                                              (toSqlKey bid)
                                              bn
                                              h) "")
    parseJSON _ = error "bad param when calling parseJSON for RawTransaction'"

instance ToJSON RawTransaction where
    toJSON rt@(RawTransaction (Address fa) non gp gl (Just (Address ta)) val cod r s v _ bn h) =
        object ["from" .= showHex fa "", "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "to" .= showHex ta "" , "value" .= show val, "codeOrData" .= cod, 
        "r" .= showHex r "",
        "s" .= showHex s "",
        "v" .= showHex v "",
        "blockNumber" .= bn,
        "hash" .= h,
        "transactionType" .= (show $ rawTransactionSemantics rt)
               ]
    toJSON rt@(RawTransaction (Address fa) non gp gl Nothing val cod r s v _ bn h) =
        object ["from" .= showHex fa "", "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "value" .= show val, "codeOrData" .= cod,
        "r" .= showHex r "",
        "s" .= showHex s "",
        "v" .= showHex v "",
        "blockNumber" .= bn,
        "hash" .= h,
        "transactionType" .= (show $ rawTransactionSemantics rt)
               ]

instance FromJSON RawTransaction where
    parseJSON (Object t) = do
      fa <- fmap (fst . head . readHex) (t .: "from")
      (tnon :: Int)  <- (t .: "nonce")
      (tgp :: Int) <- (t .: "gasPrice")
      (tgl :: Int) <- (t .: "gasLimit")
      tto <- (t .:? "to")
      let toFld = case tto of
            (Just str) -> fmap (Address . fst . head . readHex) str
            Nothing -> Nothing
      tval <- fmap read (t .: "value")
      tcd <- fmap (fst .  B16.decode . T.encodeUtf8 ) (t .: "codeOrData")
      (tr :: Integer) <- fmap (fst . head . readHex) (t .: "r")
      (ts :: Integer) <- fmap (fst . head . readHex) (t .: "s")
      (tv :: Word8) <- fmap (fst . head . readHex) (t .: "v")
      mbid <- (t .:? "blockId")
      mbn <- (t .:? "blockNumber")
      h <- (t .: "hash")
      let bid = case mbid of
            Just bd -> bd
            Nothing -> 1 -- annoying, needs to be reset on update
          bn = case mbn of
            Just b -> b
            Nothing -> -1
      
      return (RawTransaction (Address fa)
                                              (fromIntegral tnon :: Integer)
                                              (fromIntegral $ tgp :: Integer)
                                              (fromIntegral $ tgl :: Integer)
                                              (toFld :: Maybe Address)
                                              (tval :: Integer)
                                              (tcd :: B.ByteString)
                                              (tr :: Integer)
                                              (ts :: Integer)
                                              (tv :: Word8)
                                              (toSqlKey bid)
                                              bn
                                              h) 
    parseJSON _ = error "bad param when calling parseJSON for RawTransaction"

rtToRtPrime :: (String , RawTransaction) -> RawTransaction'
rtToRtPrime (s, x) = RawTransaction' x s

rtToRtPrime' :: RawTransaction -> RawTransaction'
rtToRtPrime' x = RawTransaction' x ""

data Transaction' = Transaction' Transaction deriving (Eq, Show)

instance ToJSON Transaction' where
    toJSON (Transaction' tx@(MessageTX tnon tgp tgl (Address tto) tval td tr ts tv)) = 
        object ["kind" .= ("Transaction" :: String), 
                "from" .= ((uncurry showHex) $ ((fromMaybe (Address 0) (whoSignedThisTransaction tx)),"")),
                "nonce" .= tnon, 
                "gasPrice" .= tgp, 
                "gasLimit" .= tgl, 
                "to" .= showHex tto "", 
                "value" .= tval,
                "data" .= td, 
                "r" .= showHex tr "", 
                "s" .= showHex ts "", 
                "v" .= showHex tv "",
                "hash" .= transactionHash tx,
                "transactionType" .= (show $ transactionSemantics $ tx)]
    toJSON (Transaction' tx@(ContractCreationTX tnon tgp tgl tval (Code ti) tr ts tv)) = 
        object ["kind" .= ("Transaction" :: String), 
                "from" .= ((uncurry showHex) $ ((fromMaybe (Address 0) (whoSignedThisTransaction tx)),"")),      
                "nonce" .= tnon, 
                "gasPrice" .= tgp, 
                "gasLimit" .= tgl, 
                "value" .= tval, 
                "init" .= ti,
                "r" .= showHex tr "", 
                "s" .= showHex ts "", 
                "v" .= showHex tv "",
                "hash" .= transactionHash tx,
                "transactionType" .= (show $ transactionSemantics $ tx)]

{-- needs to be updated --}
instance FromJSON Transaction' where
    parseJSON (Object t) = do
      tto <- (t .:? "to")
      tnon <- (t .: "nonce")
      tgp <- (t .: "gasPrice")
      tgl <- (t .: "gasLimit")
      tval <- (t .: "value")
      tr <- (t .: "r")
      ts <- (t .: "s")
      tv <- (t .: "v")

      case tto of
        Nothing -> do
          ti <- (t .: "init")
          return (Transaction' (ContractCreationTX tnon tgp tgl tval ti tr ts tv))
        (Just to') -> do
          td <- (t .: "data")
          return (Transaction' (MessageTX tnon tgp tgl to' tval td tr ts tv))
    parseJSON _ = error "bad param when calling parseJSON for Transaction'"


instance ToJSON Transaction where
    toJSON (tx@(MessageTX tnon tgp tgl (Address tto) tval td tr ts tv)) = 
        object ["kind" .= ("Transaction" :: String), 
                "from" .= ((uncurry showHex) $ ((fromMaybe (Address 0) (whoSignedThisTransaction tx)),"")),
                "nonce" .= tnon, 
                "gasPrice" .= tgp, 
                "gasLimit" .= tgl, 
                "to" .= showHex tto "", 
                "value" .= tval,
                "data" .= td, 
                "r" .= showHex tr "", 
                "s" .= showHex ts "", 
                "v" .= showHex tv "",
                "hash" .= transactionHash tx,
                "transactionType" .= (show $ transactionSemantics $ tx)]
    toJSON (tx@(ContractCreationTX tnon tgp tgl tval (Code ti) tr ts tv)) = 
        object ["kind" .= ("Transaction" :: String), 
                "from" .= ((uncurry showHex) $ ((fromMaybe (Address 0) (whoSignedThisTransaction tx)),"")),      
                "nonce" .= tnon, 
                "gasPrice" .= tgp, 
                "gasLimit" .= tgl, 
                "value" .= tval, 
                "init" .= ti,
                "r" .= showHex tr "", 
                "s" .= showHex ts "", 
                "v" .= showHex tv "",
                "hash" .= transactionHash tx,
                "transactionType" .= (show $ transactionSemantics $ tx)]

tToTPrime :: Transaction -> Transaction'
tToTPrime x = Transaction' x

data Block' = Block' Block String deriving (Eq, Show)

instance ToJSON Block' where
      toJSON (Block' (Block bd rt bu) next) =
        object ["next" .= next, "kind" .= ("Block" :: String), "blockData" .= bdToBdPrime bd,
         "receiptTransactions" .= map tToTPrime rt,
         "blockUncles" .= map bdToBdPrime bu]

      --TODO- check if this next case is needed
      --toJSON _ = object ["malformed Block" .= True]

instance ToJSON Block where
      toJSON (Block bd rt bu) =
        object ["kind" .= ("Block" :: String), "blockData" .= bdToBdPrime bd,
         "receiptTransactions" .= map tToTPrime rt,
         "blockUncles" .= map bdToBdPrime bu]

      --TODO- check if this next case is needed
      --toJSON _ = object ["malformed Block" .= True]

bToBPrime :: (String , Block) -> Block'
bToBPrime (s, x) = Block' x s


bToBPrime' :: Block -> Block'
bToBPrime' x = Block' x ""

data BlockData' = BlockData' BlockData deriving (Eq, Show)

instance ToJSON BlockData' where
      toJSON (BlockData' (BlockData ph uh (Address a) sr tr rr _ d num gl gu ts ed non mh)) = 
        object ["kind" .= ("BlockData" :: String), "parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
        "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
        "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
        "mixHash" .= mh]
      
      --TODO- check if this next case is needed
      --toJSON _ = object ["malformed BlockData" .= True]

bdToBdPrime :: BlockData -> BlockData'
bdToBdPrime x = BlockData' x

data BlockDataRef' = BlockDataRef' BlockDataRef deriving (Eq, Show)

instance ToJSON BlockDataRef' where
      toJSON (BlockDataRef' (BlockDataRef ph uh (Address a) sr tr rr _ d num gl gu ts ed non mh bi h pow isConf td)) = 
        object ["parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
        "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
        "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
        "mixHash" .= mh, "blockId" .= bi, "hash" .= h, "powVerified" .= pow, "isConfirmed" .= isConf, "totalDifficulty" .= td]



bdrToBdrPrime :: BlockDataRef -> BlockDataRef'
bdrToBdrPrime x = BlockDataRef' x

data AddressStateRef' = AddressStateRef' AddressStateRef deriving (Eq, Show)

instance ToJSON AddressStateRef' where
    toJSON (AddressStateRef' (AddressStateRef (Address x) n b cr c bId bNum)) = 
        object ["kind" .= ("AddressStateRef" :: String), "address" .= (showHex x ""), "nonce" .= n, "balance" .= show b, 
        "contractRoot" .= cr, "code" .= c, "latestBlockId" .= bId, "latestBlockNum" .= bNum]


instance FromJSON AddressStateRef' where
    parseJSON (Object s) = do
      kind <- s .: "kind"
      if kind /= ("AddressStateRef" :: String)
        then fail "JSON is not AddressStateRef"
        else asrToAsrPrime <$> 
              (AddressStateRef
                <$> Address . fst . head . readHex <$> s .: "address"
                <*> s .: "nonce"
                <*> (read <$> (s .: "balance"))
                <*> s .: "contractRoot"
                <*> s .: "code"
                <*> s .: "latestBlockId"
                <*> s .: "latestBlockNum"
              )
    parseJSON _ = fail "JSON not an object"

asrToAsrPrime :: AddressStateRef -> AddressStateRef'
asrToAsrPrime x = AddressStateRef' x

--jsonFix x@(AddressStateRef a b c d e) = AddressStateRef' x
--jsonFix x@(BlockDataRef a b c d e f g h i j k l m n o p q) = BlockDataRef' x

data Address' = Address' Address deriving (Eq, Show)
adToAdPrime::Address->Address'
adToAdPrime x = Address' x

--instance ToJSON Address' where
--  toJSON (Address' x) = object [ "address" .= (showHex x "") ]

data TransactionType = Contract | FunctionCall | Transfer  deriving (Eq, Show)

--instance ToJSON TransactionType where 
--   toJSON x = object ["transactionType" .= show x]

transactionSemantics :: Transaction -> TransactionType
transactionSemantics (MessageTX _ _ _ (Address _) _ td _ _ _) = work
    where work | (B.length td) > 0 = FunctionCall
               | otherwise = Transfer
transactionSemantics _ = Contract

isAddr :: Maybe Address -> Bool
isAddr a = case a of
      Just _   -> True
      Nothing  -> False

rawTransactionSemantics :: RawTransaction -> TransactionType
rawTransactionSemantics (RawTransaction _ _ _ _ ta _ cod _ _ _ _ _ _) = work
     where work | (not (isAddr ta))  = Contract
                | (isAddr ta) &&  ((B.length cod) > 0)        = FunctionCall
                | otherwise = Transfer
