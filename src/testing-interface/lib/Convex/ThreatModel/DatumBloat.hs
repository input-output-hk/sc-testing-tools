{-# LANGUAGE OverloadedStrings #-}

{- | Threat model for detecting Datum Bloat Attack vulnerabilities.

A Datum Bloat Attack exploits validators that don't limit the size of data
fields within their datums. Unlike the Large Data Attack (which adds extra
constructor fields), this attack inflates /existing/ fields - specifically
lists and byte strings within the datum structure.

== Consequences ==

1. __Increased execution costs__: Processing bloated datums wastes CPU/memory
   execution units, making transactions more expensive.

2. __Permanent fund locking__: If a list or bytestring field is bloated sufficiently:

   - Deserializing the datum may exceed execution unit limits
   - The transaction required to spend the UTxO may exceed protocol size limits

   In these cases, the UTxO becomes __permanently unspendable__ and funds
   are locked forever with no possibility of recovery.

== Vulnerable Patterns ==

=== Pattern 1: Unbounded list fields ===

@
type Datum {
  owner: VerificationKeyHash,
  messages: List<ByteArray>  -- No list length limit!
}
@

An attacker can append arbitrarily many items to the messages list,
bloating the datum beyond transaction limits. Caught by 'datumListBloatAttack'.

=== Pattern 2: Unbounded ByteString fields ===

@
type Datum {
  owner: VerificationKeyHash,
  messages: List<ByteArray>  -- No ByteArray SIZE limit!
}
@

An attacker can replace small ByteArrays with huge ones (e.g., "Hello" -> 100KB).
Caught by 'datumByteBloatAttack'.

== Mitigation ==

A secure validator should either:

- Enforce maximum field sizes in the validator logic
- Check list lengths explicitly (e.g., @length messages <= maxMessages@)
- Limit ByteArray sizes (e.g., @lengthOfByteString msg <= maxMsgSize@)
- Hash large data instead of storing it inline

This threat model tests if a script output with an inline datum still validates
when list fields are bloated with additional large items, or when byte string
fields are replaced with much larger ones.
-}
module Convex.ThreatModel.DatumBloat (
  -- * List bloating attacks
  datumListBloatAttack,
  datumListBloatAttackWith,
  datumListBloatAttackWithGen,
  bloatLists,

  -- * ByteString inflation attacks
  datumByteBloatAttack,
  datumByteBloatAttackWith,
  datumByteBloatAttackWithGen,
  inflateBytes,
  inflateFirstListItem,
) where

import Convex.ThreatModel
import Data.ByteString qualified as BS
import Test.QuickCheck (Gen, choose, shrinkIntegral)

{- | Default datum-list-bloat attack. The number of items and item size are
drawn per transaction from curated ranges, so QuickCheck explores the
parameter space and shrinks counterexamples toward the smallest triggering
values.
-}
datumListBloatAttack :: ThreatModel ()
datumListBloatAttack = datumListBloatAttackWithGen ((,) <$> choose (1, 20) <*> choose (1, 500))

{- | Datum-list-bloat attack with fixed parameters. Keep using this for
deterministic regression tests and golden seeds.
-}
datumListBloatAttackWith :: Int -> Int -> ThreatModel ()
datumListBloatAttackWith numItems itemSize = datumListBloatAttackWithGen (pure (numItems, itemSize))

{- | Datum-list-bloat attack parameterised by a generator for the number of
items and the per-item byte size. This is the primitive the other two forms
delegate to.
-}
datumListBloatAttackWithGen :: Gen (Int, Int) -> ThreatModel ()
datumListBloatAttackWithGen gen =
  Named "Datum List Bloat Attack" $ do
    (numItems, itemSize) <- forAllTM gen shrinkPair

    -- Skip iterations where the draw is too small to be a meaningful attack.
    ensure (numItems >= 1 && itemSize >= 1)

    requireScriptInput

    -- Get all outputs from the transaction
    outputs <- getTxOutputs

    -- Filter to script outputs with inline datums
    let scriptOutputsWithDatum = filter isScriptOutputWithInlineDatum outputs

    -- Precondition: there must be at least one script output with inline datum
    threatPrecondition $ ensure (not $ null scriptOutputsWithDatum)

    -- Pick a target output
    target <- pickAny scriptOutputsWithDatum

    -- Extract the inline datum (we know it exists due to the filter)
    originalDatum <- case getInlineDatum target of
      Nothing -> failPrecondition "Script output missing inline datum"
      Just originalDatum' -> pure originalDatum'

    -- Check if the datum contains any lists to bloat
    unless (containsList originalDatum) $
      failPrecondition "Datum contains no list fields to bloat"

    let bloatedDatum = bloatLists numItems itemSize originalDatum

    counterexampleTM $
      paragraph
        [ "The transaction contains a script output at index"
        , show (outputIx target)
        , "with an inline datum containing list fields."
        ]

    counterexampleTM $
      paragraph
        [ "Testing if the lists can be bloated with"
        , show numItems
        , "items of"
        , show itemSize
        , "bytes each while still passing validation."
        ]

    counterexampleTM $
      paragraph
        [ "If this validates, the script doesn't enforce datum field size limits."
        , "An attacker could exploit this to:"
        , "1) Inflate the datum beyond transaction size limits"
        , "2) Increase execution costs for processing the datum"
        , "3) Potentially lock funds permanently if limits are exceeded"
        ]

    tabulateTM "items" [bucketItems numItems]
    tabulateTM "item bytes" [bucketSize itemSize]

    -- Try to validate with the bloated datum
    shouldNotValidate $ changeDatumOf target (toInlineDatum bloatedDatum)
 where
  unless False action = action
  unless True _ = pure ()

-- | Shrink a pair of positive integers toward (1, 1).
shrinkPair :: (Int, Int) -> [(Int, Int)]
shrinkPair (a, b) =
  [(a', b) | a' <- shrinkPositive a]
    ++ [(a, b') | b' <- shrinkPositive b]

-- | Coarse bucket for the item-count distribution report.
bucketItems :: Int -> String
bucketItems n
  | n <= 5 = "001-005"
  | n <= 10 = "006-010"
  | n <= 15 = "011-015"
  | otherwise = "016-020"

-- | Coarse bucket for the per-item byte-size distribution report.
bucketSize :: Int -> String
bucketSize n
  | n <= 50 = "001-050"
  | n <= 100 = "051-100"
  | n <= 250 = "101-250"
  | otherwise = "251-500"

{- | Recursively bloat all list fields in a @ScriptData@ value.

For @ScriptDataList items@, appends @numItems@ copies of
@ScriptDataBytes (BS.replicate itemSize 0x42)@ to the list.

Recursively processes @ScriptDataConstructor@ fields and nested lists.

For other @ScriptData@ variants (Map, Number, Bytes), returns
the value unchanged.
-}
bloatLists :: Int -> Int -> ScriptData -> ScriptData
bloatLists numItems itemSize = go
 where
  largeItem = ScriptDataBytes (BS.replicate itemSize 0x42)

  go (ScriptDataConstructor idx fields) =
    ScriptDataConstructor idx (map go fields)
  go (ScriptDataList items) =
    ScriptDataList (map go items ++ replicate numItems largeItem)
  go (ScriptDataMap entries) =
    ScriptDataMap [(go k, go v) | (k, v) <- entries]
  go other = other

-- | Check if a @ScriptData@ value contains any list fields.
containsList :: ScriptData -> Bool
containsList (ScriptDataConstructor _ fields) = any containsList fields
containsList (ScriptDataList _) = True
containsList (ScriptDataMap entries) = any (\(k, v) -> containsList k || containsList v) entries
containsList _ = False

-- | Check if an output is a script output with an inline datum.
isScriptOutputWithInlineDatum :: Output -> Bool
isScriptOutputWithInlineDatum output =
  not (isKeyAddressAny (addressOf output)) && hasInlineDatum output

-- | Check if an output has an inline datum.
hasInlineDatum :: Output -> Bool
hasInlineDatum output =
  case datumOfTxOut (outputTxOut output) of
    TxOutDatumInline{} -> True
    _ -> False

-- | Extract the inline datum from an output if present.
getInlineDatum :: Output -> Maybe ScriptData
getInlineDatum output =
  case datumOfTxOut (outputTxOut output) of
    TxOutDatumInline _ hashableData -> Just (getScriptData hashableData)
    _ -> Nothing

-- | Convert a @ScriptData@ to an inline @Datum@ (TxOutDatum CtxTx Era).
toInlineDatum :: ScriptData -> Datum
toInlineDatum sd =
  TxOutDatumInline BabbageEraOnwardsConway (unsafeHashableScriptData sd)

-- ----------------------------------------------------------------------------
-- ByteString Inflation Attack
-- ----------------------------------------------------------------------------

{- | Default datum-byte-bloat attack. The inflation size is drawn per
transaction from a curated range, so QuickCheck explores the parameter
space and shrinks counterexamples toward the smallest triggering value.
-}
datumByteBloatAttack :: ThreatModel ()
datumByteBloatAttack = datumByteBloatAttackWithGen (choose (1, 10000))

{- | Datum-byte-bloat attack with a fixed inflation size. Keep using this
for deterministic regression tests and golden seeds.
-}
datumByteBloatAttackWith :: Int -> ThreatModel ()
datumByteBloatAttackWith = datumByteBloatAttackWithGen . pure

{- | Datum-byte-bloat attack parameterised by a generator for the inflation
size. This is the primitive the other two forms delegate to.
-}
datumByteBloatAttackWithGen :: Gen Int -> ThreatModel ()
datumByteBloatAttackWithGen gen =
  Named "Datum Byte Bloat Attack" $ do
    inflatedSize <- forAllTM gen shrinkPositive

    -- Skip iterations where the draw is too small to be a meaningful attack.
    ensure (inflatedSize >= 1)

    requireScriptInput

    outputs <- getTxOutputs
    let scriptOutputsWithDatum = filter isScriptOutputWithInlineDatum outputs
    threatPrecondition $ ensure (not $ null scriptOutputsWithDatum)
    target <- pickAny scriptOutputsWithDatum

    originalDatum <- case getInlineDatum target of
      Nothing -> failPrecondition "Script output missing inline datum"
      Just originalDatum' -> pure originalDatum'

    let bloatedDatum = inflateFirstListItem inflatedSize originalDatum

    -- Only proceed if something actually changed (datum has list with items to inflate)
    threatPrecondition $ ensure (bloatedDatum /= originalDatum)

    counterexampleTM $
      paragraph
        [ "The transaction contains a script output with an inline datum."
        , "Testing if the first item in list fields can be inflated to"
        , show inflatedSize
        , "bytes while still passing validation."
        ]

    counterexampleTM $
      paragraph
        [ "If this validates, the script doesn't limit ByteString field sizes,"
        , "enabling a datum bloat DoS attack where an attacker can add"
        , "a huge message/data item to bloat the datum beyond spendable limits."
        ]

    tabulateTM "inflated bytes" [bucket inflatedSize]

    shouldNotValidate $ changeDatumOf target (toInlineDatum bloatedDatum)

{- | Shrink a positive integer toward 1 (the smallest meaningful value),
never reaching 0.
-}
shrinkPositive :: Int -> [Int]
shrinkPositive = filter (>= 1) . shrinkIntegral

-- | Coarse bucket for the inflation-size distribution report.
bucket :: Int -> String
bucket n
  | n <= 1000 = "0001-1000"
  | n <= 5000 = "1001-5000"
  | n <= 10000 = "5001-10000"
  | otherwise = "10000+"

{- | Replace all @ScriptDataBytes@ with inflated versions.

Preserves the first field of the top-level constructor (typically an
owner/address hash that must match exactly for validation).

Inflates all other @ScriptDataBytes@ found at any depth with a ByteString
of the given size filled with @0x42@ ('B').

For the tipjar use case, this inflates EVERY message in the list, which
changes the structure too much. For validators that do structural checks
like @list.push(old_msgs, new_msg) == new_msgs@, this will fail.

Use 'inflateFirstListItem' for a more targeted attack that only inflates
the first (newest) message in a list.
-}
inflateBytes :: Int -> ScriptData -> ScriptData
inflateBytes size = goTop
 where
  largeBytes = BS.replicate size 0x42

  -- At top level, preserve first field of constructor
  goTop (ScriptDataConstructor idx fields) =
    case fields of
      (first : rest) -> ScriptDataConstructor idx (first : map go rest)
      [] -> ScriptDataConstructor idx []
  goTop other = go other

  -- Recursive case: inflate all ByteStrings
  go (ScriptDataConstructor idx fields) = ScriptDataConstructor idx (map go fields)
  go (ScriptDataList items) = ScriptDataList (map go items)
  go (ScriptDataMap entries) = ScriptDataMap [(go k, go v) | (k, v) <- entries]
  go (ScriptDataBytes _) = ScriptDataBytes largeBytes
  go other = other

{- | Inflate only the FIRST @ScriptDataBytes@ found in lists.

This is a more targeted attack for validators like tipjar that check:
@list.push(input_messages, new_msg) == output_messages@

The validator only cares that the NEW message (head of the list) was
correctly prepended. It doesn't check the SIZE of that message.

For a tipjar datum @Con0(owner_hash, [\"New\", \"Old1\", \"Old2\"])@:

* @owner_hash@ is preserved
* @\"New\"@ (first/newest message) gets inflated to 10KB
* @\"Old1\"@, @\"Old2\"@ are left unchanged (must match input)
* Result: @Con0(owner_hash, [<10KB>, \"Old1\", \"Old2\"])@

The validator check:
* Input: @[\"Old1\", \"Old2\"]@
* @list.push([\"Old1\", \"Old2\"], <10KB>) = [<10KB>, \"Old1\", \"Old2\"]@
* This equals the output! Vulnerability exploited.
-}
inflateFirstListItem :: Int -> ScriptData -> ScriptData
inflateFirstListItem size = goTop
 where
  largeBytes = BS.replicate size 0x42

  -- At top level, preserve first field of constructor (owner hash)
  goTop (ScriptDataConstructor idx fields) =
    case fields of
      (first : rest) -> ScriptDataConstructor idx (first : map goList rest)
      [] -> ScriptDataConstructor idx []
  goTop other = goList other

  -- Find lists and inflate only the first item
  goList (ScriptDataConstructor idx fields) = ScriptDataConstructor idx (map goList fields)
  goList (ScriptDataList (firstItem : restItems)) =
    -- Inflate only the first item in the list, leave rest unchanged
    ScriptDataList (inflateItem firstItem : restItems)
  goList (ScriptDataList []) = ScriptDataList []
  goList (ScriptDataMap entries) = ScriptDataMap [(goList k, goList v) | (k, v) <- entries]
  goList other = other

  -- Inflate a single item (recursively inflate all ByteStrings in it)
  inflateItem (ScriptDataBytes _) = ScriptDataBytes largeBytes
  inflateItem (ScriptDataConstructor idx fields) =
    ScriptDataConstructor idx (map inflateItem fields)
  inflateItem (ScriptDataList items) = ScriptDataList (map inflateItem items)
  inflateItem (ScriptDataMap entries) =
    ScriptDataMap [(inflateItem k, inflateItem v) | (k, v) <- entries]
  inflateItem other = other
