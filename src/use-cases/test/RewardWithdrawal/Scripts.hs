{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
-- 1.1.0.0 will be enabled in conway
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.1.0.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- | Scripts used for testing
module RewardWithdrawal.Scripts (
  rewardWithdrawalValidatorScript,
  rewardWithdrawalCovIdx,
  RewardWithdrawal.RewardWithdrawalParams (..),
  saveRewardWithdrawalValidatorScript,
) where

import Cardano.Api qualified as C
import Convex.PlutusTx (compiledCodeToScript)
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude (BuiltinUnit)
import RewardWithdrawal.Validator qualified as RewardWithdrawal

-- | The unapplied 'RewardWithdrawal.Validator.validator', before any parameters are baked in
rewardWithdrawalValidatorUnapplied :: CompiledCode (RewardWithdrawal.RewardWithdrawalParams -> BuiltinData -> BuiltinUnit)
rewardWithdrawalValidatorUnapplied = $$(PlutusTx.compile [||RewardWithdrawal.validator||])

-- | Compiling a parameterized validator for 'RewardWithdrawal.Validator.validator'
rewardWithdrawalValidatorCompiled :: RewardWithdrawal.RewardWithdrawalParams -> CompiledCode (BuiltinData -> BuiltinUnit)
rewardWithdrawalValidatorCompiled params =
  case rewardWithdrawalValidatorUnapplied
    `PlutusTx.applyCode` PlutusTx.liftCodeDef params of
    Left err -> error err
    Right cc -> cc

-- | Serialized validator for 'RewardWithdrawal.Validator.validator'
rewardWithdrawalValidatorScript :: RewardWithdrawal.RewardWithdrawalParams -> C.PlutusScript C.PlutusScriptV3
rewardWithdrawalValidatorScript = compiledCodeToScript . rewardWithdrawalValidatorCompiled

{- | Coverage annotations baked into the compiled validator.

Empty unless the script was compiled with
@-fplugin-opt PlutusTx.Plugin:coverage-all@, so this doubles as the runtime
answer to \"was coverage enabled for this build?\".
-}
rewardWithdrawalCovIdx :: CoverageIndex
rewardWithdrawalCovIdx = getCovIdx rewardWithdrawalValidatorUnapplied

-- | Save the validator script to a file
saveRewardWithdrawalValidatorScript :: RewardWithdrawal.RewardWithdrawalParams -> FilePath -> IO ()
saveRewardWithdrawalValidatorScript params filePath = do
  let script = rewardWithdrawalValidatorScript params
  C.writeFileTextEnvelope (C.File filePath) Nothing script >>= \case
    Left err -> print $ C.displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath
