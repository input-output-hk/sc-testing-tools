{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
-- 1.1.0.0 will be enabled in conway
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.1.0.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- | Scripts used for testing
module Escrow.Scripts (
  escrowValidatorScript,
  Escrow.EscrowParams (..),
) where

import Cardano.Api qualified as C
import Convex.PlutusTx (compiledCodeToScript)
import Escrow.Validator qualified as Escrow
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinUnit)

-- | Compiling a parameterized validator for 'Scripts.Escrow.validator'
escrowValidatorCompiled :: Escrow.EscrowParams -> CompiledCode (BuiltinData -> BuiltinUnit)
escrowValidatorCompiled params =
  case $$(PlutusTx.compile [||Escrow.validator||])
    `PlutusTx.applyCode` PlutusTx.liftCodeDef params of
    Left err -> error err
    Right cc -> cc

-- | Serialized validator for 'Scripts.Escrow.validator'
escrowValidatorScript :: Escrow.EscrowParams -> C.PlutusScript C.PlutusScriptV3
escrowValidatorScript = compiledCodeToScript . escrowValidatorCompiled

-- | Save the validator script to a file
saveEscrowValidatorScript :: Escrow.EscrowParams -> FilePath -> IO ()
saveEscrowValidatorScript params filePath = do
  let script = escrowValidatorScript params
  C.writeFileTextEnvelope (C.File filePath) Nothing script >>= \case
    Left err -> print $ C.displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath
