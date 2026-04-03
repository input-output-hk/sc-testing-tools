{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-- 1.1.0.0 will be enabled in conway
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.1.0.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- | Scripts used for testing
module MultiPlayerPingPong.Scripts (
  multiPlayerPingPongValidatorScript,
  saveMultiPlayerPingPongValidatorScript,
) where

import Cardano.Api qualified as C
import Convex.PlutusTx (compiledCodeToScript)
import MultiPlayerPingPong.Validator qualified as MultiPlayerPingPong
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinUnit)

-- | Compiling a validator for 'Scripts.MultiPlayerPingPong.multiPlayerPingPongValidator'
multiPlayerPingPongValidatorCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
multiPlayerPingPongValidatorCompiled = $$(PlutusTx.compile [||MultiPlayerPingPong.validator||])

-- | Serialized validator for 'Scripts.MultiPlayerPingPong.multiPlayerPingPongValidator'
multiPlayerPingPongValidatorScript :: C.PlutusScript C.PlutusScriptV3
multiPlayerPingPongValidatorScript = compiledCodeToScript multiPlayerPingPongValidatorCompiled

-- | Save a validator script to a file
saveMultiPlayerPingPongValidatorScript :: FilePath -> IO ()
saveMultiPlayerPingPongValidatorScript filePath = do
  C.writeFileTextEnvelope (C.File filePath) Nothing multiPlayerPingPongValidatorScript >>= \case
    Left err -> print $ C.displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath
