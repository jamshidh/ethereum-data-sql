

module Blockchain.Data.TestnetGenesis where

import qualified Data.ByteString as B
import Data.Time.Clock.POSIX

import Network.Haskoin.Crypto
import Blockchain.Constants
import Blockchain.Data.Address
import Blockchain.Data.GenesisInfo
import Blockchain.Database.MerklePatricia
import Blockchain.SHA

genesisInfo::GenesisInfo
genesisInfo =
  GenesisInfo {
    genesisInfoParentHash = SHA 0,
    genesisInfoUnclesHash = hash (B.pack [0xc0]), 
    genesisInfoCoinbase = Address 0,
    genesisInfoAccountInfo = testnetAccountInfo,
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
  
testnetAccountInfo :: [(Word160,Integer)]
testnetAccountInfo =
        [
          (0x0000000000000000000000000000000000000001, 1 * wei),
          (0x0000000000000000000000000000000000000002, 1 * wei),
          (0x0000000000000000000000000000000000000003, 1 * wei),
          (0x0000000000000000000000000000000000000004, 1 * wei),
          (0xdbdbdb2cbd23b783741e8d7fcf51e459b497e4a6, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xe6716f9544a56c530d868e4bfbacb172315bdead, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xb9c015918bdaba24b4ff057a92a3873d6eb201be, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x1a26338f0d905e295fccb71fa9ea849ffa12aaf4, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x2ef47100e0787b915105fd5e3f4ff6752079d5cb, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xcd2a3d9f938e13cd947ec05abc7fe734df8dd826, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x6c386a4b26f73c802f34673f7248bb118f97424a, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xe4157b34ea9615cfbde6b4fda419828124b70c78, 1606938044258990275541962092341162602522202993782792835301376 * wei)
        ]
        
