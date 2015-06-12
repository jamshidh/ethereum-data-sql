{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Blockchain.Data.BlockDB (
  Block(..),
  BlockData(..),
  blockHash,
  powFunc,
  headerHashWithoutNonce,
  addNonceToBlock,
  findNonce,
  fastFindNonce,
  nonceIsValid,
  getBlock,
  getBlockLite,
  putBlock,
  putBlockSql,
  putBlockLite,
  rawTX2TX,
  tx2RawTX
) where 

import Database.Persist hiding (get)
import Database.Persist.Types
import Database.Persist.TH
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E

import qualified Crypto.Hash.SHA3 as C
import Data.Binary hiding (get,put)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Foreign.ForeignPtr.Unsafe
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.DBM 
import Blockchain.Data.Address
import qualified Blockchain.Colors as CL
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.RawTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.DataDefs
import Blockchain.Data.Code

import Control.Monad.State
import Control.Monad.Trans.Resource

rawTX2TX :: RawTransaction -> Transaction
rawTX2TX (RawTransaction from nonce gp gl (Just to) val dat r s v _ _ _) = (MessageTX nonce gp gl to val dat r s v)
rawTX2TX (RawTransaction from nonce gp gl Nothing val init r s v _ _ _) = (ContractCreationTX nonce gp gl val (Code init) r s v)

tx2RawTX :: Transaction -> (Key Block) -> Integer ->  RawTransaction
tx2RawTX tx blkId blkNum =
  case tx of
    (MessageTX nonce gp gl to val dat r s v) -> (RawTransaction (whoSignedThisTransaction tx) nonce gp gl (Just to) val dat r s v blkId (fromIntegral $ blkNum) (hash $ rlpSerialize $ rlpEncode tx))
    (ContractCreationTX nonce gp gl val (Code init) r s v) ->  (RawTransaction (whoSignedThisTransaction tx) nonce gp gl Nothing val init r s v blkId (fromIntegral $ blkNum) (hash $ rlpSerialize $ rlpEncode tx))
    _ -> error "couldn't convert Transaction to RawTransaction"      


calcTotalDifficulty :: Block -> BlockId -> DBM Integer
calcTotalDifficulty b bid = do
  ctx <- get
  let bd = blockBlockData b

  parent <- runResourceT $
     SQL.runSqlPool (getParent (blockDataParentHash bd)) $ sqlDB ctx
  case parent of
    Nothing ->
      case (blockDataNumber bd) of
        0 -> return (blockDataDifficulty bd)
        _ ->  error "couldn't find parent to calculate difficulty"
    Just p -> return $ (blockDataRefTotalDifficulty . entityVal $ p) + (blockDataDifficulty bd)
     
  where getParent h = do
          SQL.selectFirst [ BlockDataRefHash SQL.==. h ] []

calcTotalDifficultyLite :: Block -> BlockId -> DBMLite Integer
calcTotalDifficultyLite b bid = do
  ctx <- get
  let bd = blockBlockData b

  parent <- runResourceT $
     SQL.runSqlPool (getParent (blockDataParentHash bd)) $ sqlDBLite ctx
  case parent of
    Nothing ->
      case (blockDataNumber bd) of
        0 -> return (blockDataDifficulty bd)
        _ ->  error "couldn't find parent to calculate difficulty"
    Just p -> return $ (blockDataRefTotalDifficulty . entityVal $ p) + (blockDataDifficulty bd)
     
  where getParent h = do
          SQL.selectFirst [ BlockDataRefHash SQL.==. h ] []

-- blk2BlkDataRef :: Block -> BlockId ->  BlockDataRef
blk2BlkDataRef b blkId = do
  difficulty <- calcTotalDifficulty b blkId
  return (BlockDataRef pH uH cB sR tR rR lB d n gL gU t eD nc mH blkId (blockHash b) True True difficulty) --- Horrible! Apparently I need to learn the Lens library, yesterday
  where
      bd = (blockBlockData b)
      pH = blockDataParentHash bd
      uH = blockDataUnclesHash bd
      cB = blockDataCoinbase bd
      sR = blockDataStateRoot bd
      tR = blockDataTransactionsRoot bd
      rR = blockDataReceiptsRoot bd
      lB = blockDataLogBloom bd
      n =  blockDataNumber bd
      d  = blockDataDifficulty bd
      gL = blockDataGasLimit bd
      gU = blockDataGasUsed bd
      t  = blockDataTimestamp bd
      eD = blockDataExtraData bd
      nc = blockDataNonce bd
      mH = blockDataMixHash bd
      
blk2BlkDataRefLite b blkId = do
  difficulty <- calcTotalDifficultyLite b blkId
  return (BlockDataRef pH uH cB sR tR rR lB d n gL gU t eD nc mH blkId (blockHash b) True True difficulty) --- Horrible! Apparently I need to learn the Lens library, yesterday
  where
      bd = (blockBlockData b)
      pH = blockDataParentHash bd
      uH = blockDataUnclesHash bd
      cB = blockDataCoinbase bd
      sR = blockDataStateRoot bd
      tR = blockDataTransactionsRoot bd
      rR = blockDataReceiptsRoot bd
      lB = blockDataLogBloom bd
      n =  blockDataNumber bd
      d  = blockDataDifficulty bd
      gL = blockDataGasLimit bd
      gU = blockDataGasUsed bd
      t  = blockDataTimestamp bd
      eD = blockDataExtraData bd
      nc = blockDataNonce bd
      mH = blockDataMixHash bd


getBlock::SHA->DBM (Maybe Block)
getBlock h = 
  fmap (rlpDecode . rlpDeserialize) <$> blockDBGet (BL.toStrict $ encode h)

getBlockLite :: SHA->DBMLite (Maybe Block)
getBlockLite h = do
  ctx <- get
  entBlkL <- runResourceT $
    SQL.runSqlPool actions $ sqlDBLite $ ctx

  case entBlkL of
    [] -> return Nothing
    lst -> return $ Just . entityVal . head $ lst
  where actions = E.select $ E.from $ \(bdRef, block) -> do
                                   E.where_ ( (bdRef E.^. BlockDataRefHash E.==. E.val h ) E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. block E.^. BlockId ))
                                   return block                        

putBlock::Block->DBM ()
putBlock b = do
  _ <- putBlockSql b
  let bytes = rlpSerialize $ rlpEncode b
  blockDBPut (BL.toStrict $ encode $ blockHash b) bytes


putBlockSql ::Block->DBM (Key BlockDataRef)
putBlockSql b = do
  ctx <- get
  
  runResourceT $
    SQL.runSqlPool actions $ sqlDB $ ctx 
  where actions = do
          blkId <- SQL.insert $ b                      
          toInsert <- lift $ lift $ blk2BlkDataRef b blkId
          mapM_ (insertOrUpdate blkId) ((map (\tx -> tx2RawTX tx blkId (blockDataNumber (blockBlockData b)))  (blockReceiptTransactions b)))
          SQL.insert $ toInsert


        insertOrUpdate blkid rawTX  = do
            (txId :: [Entity RawTransaction])
                 <- SQL.selectList [ RawTransactionTxHash SQL.==. (rawTransactionTxHash rawTX )]
                                   [ ]
            case txId of
                [] -> do
                      _ <- SQL.insert rawTX
                      return ()
                lst -> mapM_ (\t -> SQL.update (SQL.entityKey t)
                                              [ RawTransactionBlockId SQL.=. blkid, 
                                                RawTransactionBlockNumber SQL.=. (fromIntegral $ blockDataNumber (blockBlockData b)) ])
                             lst

putBlockLite ::Block->DBMLite (Key BlockDataRef)
putBlockLite b = do
  ctx <- get
  
  runResourceT $
    SQL.runSqlPool actions $ sqlDBLite $ ctx 
  where actions = do
          blkId <- SQL.insert $ b                      
          toInsert <- lift $ lift $ blk2BlkDataRefLite b blkId
          mapM_ (insertOrUpdate blkId) ((map (\tx -> tx2RawTX tx blkId (blockDataNumber (blockBlockData b)))  (blockReceiptTransactions b)))
          SQL.insert $ toInsert


        insertOrUpdate blkid rawTX  = do
            (txId :: [Entity RawTransaction])
                 <- SQL.selectList [ RawTransactionTxHash SQL.==. (rawTransactionTxHash rawTX )]
                                   [ ]
            case txId of
                [] -> do
                      _ <- SQL.insert rawTX
                      return ()
                lst -> mapM_ (\t -> SQL.update (SQL.entityKey t)
                                              [ RawTransactionBlockId SQL.=. blkid, 
                                                RawTransactionBlockNumber SQL.=. (fromIntegral $ blockDataNumber (blockBlockData b)) ])
                             lst



instance Format Block where
  format b@Block{blockBlockData=bd, blockReceiptTransactions=receipts, blockBlockUncles=uncles} =
    CL.blue ("Block #" ++ show (blockDataNumber bd)) ++ " " ++
    tab (show (pretty $ blockHash b) ++ "\n" ++
         format bd ++
         (if null receipts
          then "        (no transactions)\n"
          else tab (intercalate "\n    " (format <$> receipts))) ++
         (if null uncles
          then "        (no uncles)"
          else tab ("Uncles:" ++ tab ("\n" ++ intercalate "\n    " (format <$> uncles)))))
              
instance RLPSerializable Block where
  rlpDecode (RLPArray [bd, RLPArray transactionReceipts, RLPArray uncles]) =
    Block (rlpDecode bd) (rlpDecode <$> transactionReceipts) (rlpDecode <$> uncles)
  rlpDecode (RLPArray arr) = error ("rlpDecode for Block called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("rlpDecode for Block called on non block object: " ++ show x)

  rlpEncode Block{blockBlockData=bd, blockReceiptTransactions=receipts, blockBlockUncles=uncles} =
    RLPArray [rlpEncode bd, RLPArray (rlpEncode <$> receipts), RLPArray $ rlpEncode <$> uncles]

instance RLPSerializable BlockData where
  rlpDecode (RLPArray [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15]) =
    BlockData {
      blockDataParentHash = rlpDecode v1,
      blockDataUnclesHash = rlpDecode v2,
      blockDataCoinbase = rlpDecode v3,
      blockDataStateRoot = rlpDecode v4,
      blockDataTransactionsRoot = rlpDecode v5,
      blockDataReceiptsRoot = rlpDecode v6,
      blockDataLogBloom = rlpDecode v7,
      blockDataDifficulty = rlpDecode v8,
      blockDataNumber = rlpDecode v9,
      blockDataGasLimit = rlpDecode v10,
      blockDataGasUsed = rlpDecode v11,
      blockDataTimestamp = posixSecondsToUTCTime $ fromInteger $ rlpDecode v12,
      blockDataExtraData = rlpDecode v13,
      blockDataMixHash = rlpDecode v14,
      blockDataNonce = bytesToWord64 $ B.unpack $ rlpDecode v15
      }  
  rlpDecode (RLPArray arr) = error ("Error in rlpDecode for Block: wrong number of items, expected 15, got " ++ show (length arr) ++ ", arr = " ++ show (pretty arr))
  rlpDecode x = error ("rlp2BlockData called on non block object: " ++ show x)


  rlpEncode bd =
    RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ blockDataTransactionsRoot bd,
      rlpEncode $ blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ blockDataExtraData bd,
      rlpEncode $ blockDataMixHash bd,
      rlpEncode $ B.pack $ word64ToBytes $ blockDataNonce bd
      ]

blockHash::Block->SHA
blockHash (Block info _ _) = hash . rlpSerialize . rlpEncode $ info

instance Format BlockData where
  format b = 
    "parentHash: " ++ show (pretty
                            $ blockDataParentHash b) ++ "\n" ++
    "unclesHash: " ++ show (pretty $ blockDataUnclesHash b) ++ 
    (if blockDataUnclesHash b == hash (B.pack [0xc0]) then " (the empty array)\n" else "\n") ++
    "coinbase: " ++ show (pretty $ blockDataCoinbase b) ++ "\n" ++
    "stateRoot: " ++ show (pretty $ blockDataStateRoot b) ++ "\n" ++
    "transactionsRoot: " ++ show (pretty $ blockDataTransactionsRoot b) ++ "\n" ++
    "receiptsRoot: " ++ show (pretty $ blockDataReceiptsRoot b) ++ "\n" ++
    "difficulty: " ++ show (blockDataDifficulty b) ++ "\n" ++
    "gasLimit: " ++ show (blockDataGasLimit b) ++ "\n" ++
    "gasUsed: " ++ show (blockDataGasUsed b) ++ "\n" ++
    "timestamp: " ++ show (blockDataTimestamp b) ++ "\n" ++
    "extraData: " ++ show (pretty $ blockDataExtraData b) ++ "\n" ++
    "nonce: " ++ showHex (blockDataNonce b) "" ++ "\n"


--------------------------
--Mining stuff

--used as part of the powFunc
noncelessBlockData2RLP::BlockData->RLPObject
noncelessBlockData2RLP bd =
  RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ blockDataTransactionsRoot bd,
      rlpEncode $ blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ blockDataExtraData bd
      ]

{-
noncelessBlock2RLP::Block->RLPObject
noncelessBlock2RLP Block{blockData=bd, receiptTransactions=receipts, blockUncles=[]} =
  RLPArray [noncelessBlockData2RLP bd, RLPArray (rlpEncode <$> receipts), RLPArray []]
noncelessBlock2RLP _ = error "noncelessBock2RLP not definted for blockUncles /= []"
-}

sha2ByteString::SHA->B.ByteString
sha2ByteString (SHA val) = BL.toStrict $ encode val

headerHashWithoutNonce::Block->ByteString
headerHashWithoutNonce b = C.hash 256 $ rlpSerialize $ noncelessBlockData2RLP $ blockBlockData b

powFunc::Block->Integer
powFunc b =
  --trace (show $ headerHashWithoutNonce b) $
  byteString2Integer $ 
  C.hash 256 $
    headerHashWithoutNonce b
    `B.append`
    B.pack (word64ToBytes (blockDataNonce $ blockBlockData b))

nonceIsValid::Block->Bool
nonceIsValid b = powFunc b * blockDataDifficulty (blockBlockData b) < (2::Integer)^(256::Integer)

addNonceToBlock::Block->Integer->Block
addNonceToBlock b n =
  b {
    blockBlockData=(blockBlockData b) {blockDataNonce= fromInteger n}
    }

findNonce::Block->Integer
findNonce b =
    fromMaybe (error "Huh?  You ran out of numbers!!!!") $
              find (nonceIsValid . addNonceToBlock b) [1..]


----------

fastFindNonce::Block->IO Integer
fastFindNonce b = do
  let (theData, _, _) = toForeignPtr $ headerHashWithoutNonce b
  let (theThreshold, _, _) = toForeignPtr threshold
  retValue <- mallocArray 32
  retInt <- c_fastFindNonce (unsafeForeignPtrToPtr theData) (unsafeForeignPtrToPtr theThreshold) retValue
  print retInt
  retData <- peekArray 32 retValue
  return $ byteString2Integer $ B.pack retData
  where
    threshold::B.ByteString
    threshold = fst $ B16.decode $ BC.pack $ padZeros 64 $ showHex ((2::Integer)^(256::Integer) `quot` blockDataDifficulty (blockBlockData b)) ""

foreign import ccall "findNonce" c_fastFindNonce::Ptr Word8->Ptr Word8->Ptr Word8->IO Int
--foreign import ccall "fastFindNonce" c_fastFindNonce::ForeignPtr Word8->ForeignPtr Word8->ForeignPtr Word8


{-
fastFindNonce::Block->Integer
fastFindNonce b =
  byteString2Integer $ BC.pack $ 
  BC.unpack $
  C.hash 256 (
    first `B.append` second)
  where
    first = headerHashWithoutNonce b

fastPowFunc::Block->Integer
fastPowFunc b =
  --trace (show $ headerHashWithoutNonce b) $
  byteString2Integer $ BC.pack $ 
  BC.unpack $
  C.hash 256 (
    headerHashWithoutNonce b
    `B.append`
    sha2ByteString (nonce $ blockData b))
-}
