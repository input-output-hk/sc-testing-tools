{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Threat model for detecting Token Forgery vulnerabilities.

A Token Forgery Attack exploits minting policies that are too permissive.
If a minting policy allows tokens to be minted under weak conditions (e.g.,
just requiring any signature), an attacker can mint unauthorized tokens.

== Vulnerability Pattern ==

A vulnerable minting policy might only check:

@
MintValidation -> {
  // VULNERABLE: Anyone who signs can mint!
  list.length(self.extra_signatories) > 0
}
@

This is trivially satisfied by ANY signed transaction, allowing anyone to
forge tokens that should be restricted.

== Consequences ==

1. __Validation token bypass__: If a validator requires a "validation token"
   to prove authorization, attackers can mint their own tokens.

2. __Asset theft__: Forged tokens can be used to satisfy validator checks,
   potentially draining funds.

3. __Protocol manipulation__: In DeFi protocols, forged governance or
   utility tokens can manipulate voting, rewards, or access control.

== Mitigation ==

A secure minting policy should:

- Require specific authorized signers (not just "any signature")
- Check that minting is authorized by a governance mechanism
- Verify minting is part of a valid protocol operation
- Use one-shot minting for unique tokens (NFTs, thread tokens)

'tokenForgeryAttack' tests if additional tokens can be minted using a minting
policy the transaction under test already exercises, reusing the same
redeemer. If the transaction still validates with the extra minted tokens,
that minting policy is too permissive. Use 'tokenForgeryAttackWith' to test a
specific policy instead (e.g. one the transaction doesn't otherwise use).
-}
module Convex.ThreatModel.TokenForgery (
  -- * Threat models
  tokenForgeryAttack,
  tokenForgeryAttackWith,
) where

import Cardano.Api qualified as C
import Convex.ThreatModel
import Convex.ThreatModel.Cardano.Api (IsPlutusScriptInEra, mintedPlutusPolicies)
import Convex.ThreatModel.TxModifier (addPlutusScriptMint)
import GHC.Exts (fromList, toList)

{- | Check for Token Forgery vulnerabilities against a minting policy the transaction
under test already exercises.

For every Plutus minting policy the transaction mints a positive quantity under,
resolved from its own witness set or a reference-script UTxO, this picks one such
policy/asset pair and attempts to mint one additional unit of that asset under the
same policy with the same redeemer the transaction already used. If the modified
transaction still validates, the minting policy accepted more than it authorized.

Skips (via 'threatPrecondition') transactions that don't mint under any resolvable
Plutus policy. Use 'tokenForgeryAttackWith' to test a specific policy instead,
e.g. one unrelated to anything the transaction does.
-}
tokenForgeryAttack :: ThreatModel ()
tokenForgeryAttack = Named "Token Forgery Attack" $ do
  tx <- originalTx
  ThreatModelEnv _ utxos _ <- getThreatModelEnv
  let candidates =
        [ (assetName, scriptInAnyLang, redeemer)
        | (_policyId, assets, scriptInAnyLang, redeemer) <- mintedPlutusPolicies tx utxos
        , (assetName, quantity) <- toList assets
        , quantity > 0
        ]
  case candidates of
    [] -> failPrecondition "Transaction does not mint any Plutus policy assets"
    _ -> do
      (assetName, scriptInAnyLang, redeemer) <- pickAny candidates
      case scriptInAnyLang of
        C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) (C.PlutusScript _ script) ->
          mintExtraUnit redeemer script assetName
        C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) (C.PlutusScript _ script) ->
          mintExtraUnit redeemer script assetName
        C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) (C.PlutusScript _ script) ->
          mintExtraUnit redeemer script assetName
        _ -> failPrecondition "Minting policy is not a Plutus script (V1, V2, or V3)"

{- | Check for Token Forgery vulnerabilities with a specific minting policy and redeemer,
regardless of whether the transaction under test already uses it.

@
  -- Test with MintValidation redeemer (Constr 0 [])
  tokenForgeryAttackWith (ScriptDataConstructor 0 []) mintingPolicy assetName

  -- Test with custom redeemer
  tokenForgeryAttackWith myRedeemer mintingPolicy assetName
@
-}
tokenForgeryAttackWith
  :: (IsPlutusScriptInEra lang)
  => C.ScriptData
  -- ^ Redeemer for the minting policy
  -> C.PlutusScript lang
  -- ^ The minting policy to test
  -> C.AssetName
  -- ^ The asset name to mint
  -> ThreatModel ()
tokenForgeryAttackWith redeemer mintScript assetName =
  Named "Token Forgery Attack" $ mintExtraUnit redeemer mintScript assetName

mintExtraUnit :: (IsPlutusScriptInEra lang) => C.ScriptData -> C.PlutusScript lang -> C.AssetName -> ThreatModel ()
mintExtraUnit redeemer mintScript assetName = do
  -- Find an output to add the minted tokens to
  -- Prefer a key address output (like the change output)
  output <- anyOutputSuchThat (isKeyAddressAny . addressOf)

  counterexampleTM $
    paragraph
      [ "Testing Token Forgery vulnerability:"
      , "Attempting to mint additional tokens using the provided minting policy."
      , "Adding minted tokens to output at " ++ show (prettyAddress $ addressOf output) ++ "."
      ]

  counterexampleTM $
    paragraph
      [ "If this validates, the minting policy is too permissive."
      , "An attacker could forge tokens to:"
      , "1) Bypass validation token requirements"
      , "2) Steal assets protected by token checks"
      , "3) Manipulate protocol state"
      ]

  -- Calculate the minted asset value
  let scriptHash = C.hashScript $ C.PlutusScript plutusScriptVersion mintScript
      policyId = C.PolicyId scriptHash
      mintedValue = fromList [(C.AssetId policyId assetName, 1)]
      valueWithForgedToken = valueOf output <> mintedValue

  -- Top up ADA to the new min-UTxO requirement. Without this, adding a brand
  -- new asset to the output can push it below the min-UTxO for its (now
  -- larger) value, tripping BabbageOutputTooSmallUTxO in Phase 1 before the
  -- minting policy is ever exercised -- silently defeating the attack.
  ThreatModelEnv _ _ envPParams <- getThreatModelEnv
  let C.TxOut outAddr _ outDatum outRefScript = outputTxOut output
      candidateTxOut =
        C.TxOut
          outAddr
          (C.TxOutValueShelleyBased C.shelleyBasedEra (C.toMaryValue valueWithForgedToken))
          outDatum
          outRefScript
      minCoin = C.calculateMinimumUTxO C.shelleyBasedEra (C.unLedgerProtocolParameters envPParams) candidateTxOut
      shortfall = minCoin - C.selectLovelace valueWithForgedToken
      newValue
        | shortfall > 0 = valueWithForgedToken <> C.lovelaceToValue shortfall
        | otherwise = valueWithForgedToken

  -- Try to mint one additional token with the given policy and add it to the output
  -- This SHOULD fail - if it validates, the policy is vulnerable
  shouldNotValidate $
    changeValueOf output newValue
      <> addPlutusScriptMint mintScript assetName (C.Quantity 1) redeemer
