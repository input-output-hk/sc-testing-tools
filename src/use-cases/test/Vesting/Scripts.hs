{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
-- 1.1.0.0 will be enabled in conway
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.1.0.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- | Scripts used for testing
module Vesting.Scripts (
  vestingValidatorScript,
  Vesting.VestingParams (..),
  saveVestingValidatorScript,
) where

import Cardano.Api qualified as C
import Convex.PlutusTx (compiledCodeToScript)
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinUnit)
import Vesting.Validator qualified as Vesting

-- | Compiling a parameterized validator for 'Scripts.Vesting.validator'
vestingValidatorCompiled :: Vesting.VestingParams -> CompiledCode (BuiltinData -> BuiltinUnit)
vestingValidatorCompiled params =
  case $$(PlutusTx.compile [||Vesting.validator||])
    `PlutusTx.applyCode` PlutusTx.liftCodeDef params of
    Left err -> error err
    Right cc -> cc

-- | Serialized validator for 'Scripts.Vesting.validator'
vestingValidatorScript :: Vesting.VestingParams -> C.PlutusScript C.PlutusScriptV3
vestingValidatorScript = compiledCodeToScript . vestingValidatorCompiled

-- | Save the validator script to a file
saveVestingValidatorScript :: Vesting.VestingParams -> FilePath -> IO ()
saveVestingValidatorScript params filePath = do
  let script = vestingValidatorScript params
  C.writeFileTextEnvelope (C.File filePath) Nothing script >>= \case
    Left err -> print $ C.displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath
