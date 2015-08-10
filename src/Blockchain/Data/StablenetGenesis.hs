{-# LANGUAGE OverloadedStrings, TupleSections #-}


module Blockchain.Data.StablenetGenesis where

import Data.Bits
import qualified Data.ByteString as B
import Data.Time.Clock.POSIX

import Blockchain.Data.Address
import Blockchain.Data.GenesisInfo
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.SHA


stablenetGenesisInfo::GenesisInfo
stablenetGenesisInfo =
  GenesisInfo {
    genesisInfoParentHash = SHA 0,
    genesisInfoUnclesHash = hash (B.pack [0xc0]), 
    genesisInfoCoinbase = Address 0,
    genesisInfoAccountInfo = stablenetAccountInfo,
    genesisInfoTransactionsRoot = emptyTriePtr,
    genesisInfoReceiptsRoot = emptyTriePtr,
    genesisInfoLogBloom = B.replicate 256 0,
    genesisInfoDifficulty = 0x020000, --1 << 17
    genesisInfoNumber = 0,
    genesisInfoGasLimit = 3141592,
    genesisInfoGasUsed = 0,
    genesisInfoTimestamp = posixSecondsToUTCTime 0,
    genesisInfoExtraData = 0,
    genesisInfoMixHash = SHA 0,
    genesisInfoNonce = 42 -- hash $ B.pack [42]
    }

alternateAddresses::[Word160]
alternateAddresses =
        [
          0xe1fd0d4a52b75a694de8b55528ad48e2e2cf7859,

          0xaf8b2d3fe28201476fc0a3961f8f9690693f3ef4,
          0x51a7b750eef433d30b607588f148b1916e809a57,
          0x782eb06293e83013a27fe9a1565024f8de69c4e6,
          0x45186086fed161da87c1adfe2cc0959527bdeb88,
          0xb2e30da3a268fecf2da0bf3ce2ac585fc061abbe,
          0x8eb0d47301ca98f19a9f6084926ca05e8db8d464,
          0x5508f4a16df16b7a1c7e91bdae9e7e39fbbe1ff4,
          0x259eb98117964225ebfc6e36240a5c4691a3c713,
          0xe5517ed95c86dd485882b05fb2810a4ecbd8a408,
          0xeeca09c9ce60d0bc611321bed0946e5fb7a16d37,
          0x189ed0357fe1e21a77f910e972ede149370c38e4,
          0x68a03e2e5c82a1abb1cbe00a61a95429f33c4b6b,
          0xc3f4ad9d47bfe5cb8fb5d9e0ea551920dabf6864,
          0x7c46dafbab2ed794ac06a19ee2d5e13ac4a8aef6,
          0x37acd0bde7ed52ebe5ed248c178925649e71c4cf,
          0xe9268b4e8347d490b882ea5c7353fa77115401c2,
          0xe194c5fbc5779b4543fa6c4160176563acd640a7,
          0xc8ee43b12a93a6d3a70bc65cdf8d8025cfdb541c,
          0x2e8496cccee0512f9998c705c033be824ca4f4dd,
          0xb2eafe67d2a39ecf612fc4cdd603001049fc72e0,
          0x6ff6c2d937cf5a31a0d9a59364465ad91d454829,
          0xa320f8711110fb9ee0a3b0205332045fdcd8e003,
          0xe53ee7e0ad171aea4266d45365c32c6e8a45c938,
          0x91da47c2864b1a5a187c7d0d8325b8bd9e65b628,
          0x03d1b8a84cf8bf47fd095689b177312db60c12e9,
          0x964e04041948bef28afadf5b60cf37303af371a0,
          0x7afcf309ea63569bcb25818db72c85965eca6975,
          0x165957d17265ed3f84ae3f0c70f709cbf24ec6cd,
          0x189d231bc2a10f140b596914f960d5882d84e874,
          0x75b082195b59660c20965f71d8ceb6dbb0d44da2,
          0x2f52c22cfd08987c079772d531dece6c792a49b,
          0xe946066f6f02051a3a90e82149b0541b5da19d1,
          0x3049e55d9d9ad702dae2776b5d739cee66b766a6,
          0x8a0efc1e4a419274a84535b419183f0833bf9a00,
          0xd2752fb239da7494e2aa0b28f3677c59db0db23f,
          0xec9ddd74505d37daf3b6a60a2d14c2e6b631d3ce,
          0x93e61dfc56715bd0e924dfecbe17dbb54e5959c5,
          0xb5a780a4baca2e220e5a525220ee0969b5278475,
          0x99628e3c65acd923446b7948895a4d464c7d0d2c,
          0xb024b78b9ad5a65eccf7f852f430796396c8fbd8,
          0x5cef2aa68a9ccdb23b1326e9b5a29dbc0f1e79cf,
          0x0d6a1f62bcd926b623b72ab4c6a692496736d395,
          0x2b5f3d3cfc176bd77e42e99a0f2132605524d629,
          0xf40cf138e9dd48837e07b7a8e65c1bf20a9ba0e9,
          0x825f7814d778ee708d7be40d5f5c0f44317965f2,
          0x8b941081307156ad6e428c2e74a2bbf70054004d,
          0x0065e00f66ec04bd33019aa721a74ed7b02db9c9,
          0x24da7b589fa048ce38aa40b661bfb1647b925fde,
          0xa7ddd7558321d6ce4bbbf96b6905d98a9968bfdd,
          0x5827a72671940a93d99ec86f47db244d0e0fa442
        ]

stablenetAccountInfo::[(Word160, Integer)]
stablenetAccountInfo = map (, 1 `shiftL` 250) alternateAddresses

