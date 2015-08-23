{-# LANGUAGE TemplateHaskell #-}

import HFlags

import Blockchain.Setup

defineFlag "genesisBlockName" "canonical" "use the alternate stablenet genesis block"
$(return []) --see https://github.com/nilcons/hflags/issues/8

main::IO ()
main = do
  _ <- $initHFlags "Setup EthereumH DBs"
  
  oneTimeSetup flags_genesisBlockName
