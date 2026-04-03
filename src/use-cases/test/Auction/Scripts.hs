{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-- 1.1.0.0 will be enabled in conway
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.1.0.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- | Scripts used for testing
module Auction.Scripts (
  auctionValidatorScript,
  saveAuctionValidatorScript,
) where

import Auction.Validator qualified as Auction
import Cardano.Api qualified as C
import Convex.PlutusTx (compiledCodeToScript)
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinUnit)

-- | Compiling a parameterized validator for 'Scripts.Auction.auctionUntypedValidator'
auctionValidatorCompiled :: Auction.AuctionParams -> CompiledCode (BuiltinData -> BuiltinUnit)
auctionValidatorCompiled params =
  case $$(PlutusTx.compile [||Auction.auctionUntypedValidator||])
    `PlutusTx.applyCode` PlutusTx.liftCodeDef params of
    Left err -> error err
    Right cc -> cc

-- | Serialized validator for 'Scripts.Auction.auctionUntypedValidator'
auctionValidatorScript :: Auction.AuctionParams -> C.PlutusScript C.PlutusScriptV3
auctionValidatorScript = compiledCodeToScript . auctionValidatorCompiled

-- | Save the validator script to a file
saveAuctionValidatorScript :: Auction.AuctionParams -> FilePath -> IO ()
saveAuctionValidatorScript params filePath = do
  let script = auctionValidatorScript params
  C.writeFileTextEnvelope (C.File filePath) Nothing script >>= \case
    Left err -> print $ C.displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath
