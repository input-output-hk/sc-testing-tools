{-# LANGUAGE OverloadedStrings #-}

{- | Threat model for detecting Large Data Attack vulnerabilities.

A Large Data Attack exploits permissive @FromData@ parsers in Plutus validators
that ignore extra members when deserializing @Constr@, @List@ or @Map@ data.
If a validator's datum parser only reads the members it expects and ignores
additional ones, an attacker can "bloat" the datum with extra members while
preserving the validator's interpretation.

== Consequences ==

1. __Increased execution costs__: Processing bloated datums wastes CPU/memory
   execution units, making transactions more expensive.

2. __Permanent fund locking__: If the datum is bloated sufficiently:

   - Deserializing the datum may exceed execution unit limits
   - The transaction required to spend the UTxO may exceed protocol size limits

   In these cases, the UTxO becomes __permanently unspendable__ and funds
   are locked forever with no possibility of recovery.

== Root Cause ==

'unstableMakeIsData' and 'makeIsDataIndexed' generate parsers that use
wildcard patterns for constructor fields:

@
case (index, args) of
  (0, _) -> MyConstructor  -- The "_" ignores ALL extra fields!
@

This means @Constr 0 []@ and @Constr 0 [junk1, junk2, ..., junk10000]@ both
parse to the same value, allowing attackers to inject arbitrary data.

The same hole exists in the other two container shapes. A list-encoded datum
is a @List@ rather than a @Constr@ on-chain: a parser that takes the elements
it expects off the front of the list (@pasList@ plus positional access)
ignores every element after them, so @List [a, b]@ and
@List [a, b, junk1, ..., junk10000]@ also parse to the same value. A @Map@
datum is read by key, and @PlutusTx.AssocMap.lookup@ returns the first
matching entry, so appending entries under unused keys leaves every lookup
answering exactly as it did before.

== Mitigation ==

A secure validator should either:

- Use strict manual @FromData@ instances that check field count exactly
- Validate the datum hash matches an expected value
- Check datum structure explicitly in the validator logic

This threat model tests if a script output with an inline datum still validates
when additional members are appended to the datum's container structure (see
'bloatData'). If it does, the validator has a Large Data Attack
vulnerability. A datum that is a bare atom has nothing to append to, so the
attack is skipped as a failed precondition rather than asserting against an
unmodified transaction.
-}
module Convex.ThreatModel.LargeData (
  largeDataAttack,
  largeDataAttackWith,
  largeDataAttackWithGen,
  bloatData,
) where

import Convex.ThreatModel
import Data.ByteString qualified as BS
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Test.QuickCheck (Gen, choose, shrinkIntegral)

{- | Default large-data attack. The number of injected fields is drawn per
transaction from a curated range, so QuickCheck explores the parameter space
and shrinks counterexamples toward the smallest triggering value.
-}
largeDataAttack :: ThreatModel ()
largeDataAttack = largeDataAttackWithGen (choose (1, 1000))

{- | Large-data attack with a fixed field count. Keep using this for
deterministic regression tests and golden seeds.
-}
largeDataAttackWith :: Int -> ThreatModel ()
largeDataAttackWith = largeDataAttackWithGen . pure

{- | Large-data attack parameterised by a generator for the number of extra
members injected into the target inline datum. This is the primitive the
other two forms delegate to.
-}
largeDataAttackWithGen :: Gen Int -> ThreatModel ()
largeDataAttackWithGen fieldsGen =
  Named "Large Data Attack" $ do
    n <- forAllTM fieldsGen shrinkPositive

    -- Skip iterations where the draw is too small to be a meaningful attack.
    ensure (n >= 1)

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

    let bloatedDatum = bloatData n originalDatum

    {- A datum shape 'bloatData' cannot grow - today an atom, which has no
    members to append to - would leave the transaction byte-for-byte
    unchanged. Asserting 'shouldNotValidate' on an untouched transaction is
    vacuous: that transaction comes from a passing positive test, so it
    validates, and the attack would report a "vulnerability" for every
    contract whose datum it never actually bloated. Compare the result rather
    than enumerating the shapes here, so a shape 'bloatData' stops handling
    cannot reintroduce that. -}
    if bloatedDatum == originalDatum
      then
        failPrecondition $
          unwords
            [ "Large data attack cannot bloat a"
            , datumShape originalDatum
            , "datum: the modification would be a no-op"
            ]
      else pure ()

    counterexampleTM $
      paragraph
        [ "Injecting " ++ show n
        , "extra members into the inline datum of output"
        , show (outputIx target)
        , "and asserting the transaction no longer validates."
        ]
    tabulateTM "fields injected" [bucket n]

    -- Try to validate with the bloated datum
    shouldNotValidate $ changeDatumOf target (toInlineDatum bloatedDatum)

{- | Shrink a positive integer toward 1 (the smallest meaningful value),
never reaching 0.
-}
shrinkPositive :: Int -> [Int]
shrinkPositive = filter (>= 1) . shrinkIntegral

-- | Coarse bucket for the parameter distribution report.
bucket :: Int -> String
bucket n
  | n <= 10 = "001-010"
  | n <= 100 = "011-100"
  | n <= 500 = "101-500"
  | otherwise = "501-1000"

{- | Bloat a @ScriptData@ value by appending @n@ extra members to it.

Every container shape is handled, because each one is read on-chain by a
parser that can ignore trailing members:

- @ScriptDataConstructor idx fields@ - what 'unstableMakeIsData' and
  'makeIsDataIndexed' produce, parsed via @Constr@ pattern matching. The junk
  fields are @ScriptDataNumber 42@: a generated parser either matches a
  fixed-length prefix and ignores the rest, or matches the exact field list
  and fails, so the junk's type cannot change the verdict (and a record's
  fields are heterogeneous, so there is no "matching" type to mirror).
- @ScriptDataList xs@ - what a homogeneous list datum produces, and what a
  list-encoded record produces (e.g. plutus-tx's @makeIsDataAsList@, or an
  Aiken type declared as a list), parsed via @pasList@ plus element access. A
  parser that reads the first /k/ elements ignores every element after them.
- @ScriptDataMap kvs@ - parsed via @pasMap@ plus key lookup. Appending
  entries whose keys do not already occur preserves every existing lookup,
  because @PlutusTx.AssocMap.lookup@ returns the /first/ match and neither
  @FromData@ nor @UnsafeFromData@ for @Map@ validates key uniqueness,
  ordering, or size.

For a list or a map the junk is derived from what is already there - a
member of the list, or an existing value under a fresh key of the same shape
as the existing keys (see 'junkMemberLike' and 'junkEntriesFor'). That
matters because the two parser flavours walk a different distance: the
permissive @UnsafeFromData@ instance builds its list lazily, so junk past the
member being read is never even forced, but the strict @FromData@ instance
traverses every member and yields @Nothing@ if one fails to parse - which
would make the validator reject the datum outright and have the attack report
a /secure/ contract for the wrong reason. Junk shaped like a member already
in the datum parses by construction.

The mirroring only pays off where the container is homogeneous - a
@[ByteString]@, a @Map PubKeyHash Integer@ - which is where the strict
instance is @FromData [a]@ or @FromData (Map k v)@ and does traverse
everything. A list encoding a heterogeneous record (@makeIsDataAsList@) is
read positionally like a @Constr@ instead: a fixed-length pattern rejects any
appended member whatever its type, and a prefix pattern never forces one, so
no choice of junk changes that verdict. Mirroring is never worse than a
constant, so both cases take the same path. (An empty list or map gives
nothing to mirror, so the junk falls back to @ScriptDataNumber 42@ and a
strict parser expecting some other member type will reject it.)

The atoms @ScriptDataNumber@ and @ScriptDataBytes@ are returned unchanged -
they have no members to append to. 'largeDataAttackWithGen' fails its
precondition on an unchanged result, so those shapes are reported as skipped
rather than silently asserted against an untouched transaction.
-}
bloatData :: Int -> ScriptData -> ScriptData
bloatData n sd = case sd of
  ScriptDataConstructor idx fields ->
    ScriptDataConstructor idx (fields ++ replicate n (ScriptDataNumber 42))
  ScriptDataList xs ->
    ScriptDataList (xs ++ replicate n (junkMemberLike xs))
  ScriptDataMap kvs ->
    ScriptDataMap (kvs ++ take n (junkEntriesFor kvs))
  -- Atoms: nothing to append to, return unchanged
  _ -> sd

{- | Junk member for a list or a map, derived from the members already there
so that it parses as their type by construction (see 'bloatData').

Two refinements on "copy a member":

- The /smallest/ member is copied, not the first. The junk is replicated up
  to 'largeDataAttack''s 1000 times, so copying a large member could push the
  rebuilt transaction past @maxTxSize@ or the output under its min-UTxO
  deposit. That comes back as a Phase 1 skip, which is only a warning - a
  vulnerable contract would quietly go unreported.
- A member bigger than 'junkSizeBudget' is shrunk rather than copied whole: a
  byte string is truncated and a number replaced outright, both staying the
  same 'ScriptData' shape, so they still parse as the member type while
  making the junk's size independent of the datum's. A container has no such
  safe shrink - dropping its members can break a strict parser for /its/
  type - so a large container member is still copied as is.
-}
junkMemberLike :: [ScriptData] -> ScriptData
junkMemberLike [] = ScriptDataNumber 42
junkMemberLike xs
  | dataSize smallest <= junkSizeBudget = smallest
  | otherwise = shrink smallest
 where
  smallest = minimumBy (comparing dataSize) xs
  shrink (ScriptDataBytes bs) = ScriptDataBytes (BS.take junkSizeBudget bs)
  shrink (ScriptDataNumber _) = ScriptDataNumber 42
  shrink other = other

{- | Size at which a mirrored member is shrunk instead of copied. Small
enough that 1000 copies stay well inside @maxTxSize@, large enough that an
ordinary member (a hash, a small number) is mirrored verbatim.
-}
junkSizeBudget :: Int
junkSizeBudget = 8

-- | Rough serialised size of a datum, to compare members by.
dataSize :: ScriptData -> Int
dataSize sd = case sd of
  ScriptDataNumber _ -> 1
  ScriptDataBytes bs -> 1 + BS.length bs
  ScriptDataList xs -> 1 + sum (map dataSize xs)
  ScriptDataConstructor _ fields -> 1 + sum (map dataSize fields)
  ScriptDataMap kvs -> 1 + sum [dataSize k + dataSize v | (k, v) <- kvs]

{- | An unbounded supply of junk map entries whose keys do not already occur
in the map and have the same shape as the keys that do - both properties are
needed: a colliding key could change what an existing lookup answers, and a
key of the wrong shape makes a strict @FromData@ reject the whole datum (see
'bloatData').

A fresh key is the first existing key with one leaf perturbed past every leaf
of its kind anywhere in the key list: larger, for a number, or longer, for a
byte string. Such a key cannot equal an existing one, because that key either
has a different structure at the perturbed position or a smaller (shorter)
leaf there. Perturbing a leaf of a copied key - rather than minting a key of
some shape of our own - is what keeps the shape parseable for a key type of
any shape, including a @Constr@ (e.g. @Map AssetClass Integer@) or a nested
container.

A key built entirely from empty containers has no leaf to perturb, and no
type-correct fresh key can be derived from it; the empty result then leaves
the datum unchanged, which 'largeDataAttackWithGen' reports as a skip rather
than asserting against an untouched transaction.
-}
junkEntriesFor :: [(ScriptData, ScriptData)] -> [(ScriptData, ScriptData)]
junkEntriesFor kvs = map (\k -> (k, junkValue)) freshKeys
 where
  keys = map fst kvs
  junkValue = junkMemberLike (map snd kvs)

  freshKeys = case keys of
    -- No key to mirror the shape of, so a number is as good a guess as any.
    [] -> map ScriptDataNumber [0 ..]
    template : _ ->
      -- Whether a leaf can be perturbed at all does not depend on the index,
      -- so one probe settles it - and guards the 'mapMaybe' below against an
      -- infinitely unproductive traversal.
      case perturb 0 template of
        Nothing -> []
        Just _ -> mapMaybe (\i -> perturb i template) [0 ..]

  perturb i = replaceFirstLeaf $ \leaf -> case leaf of
    ScriptDataBytes _ -> ScriptDataBytes (BS.replicate (maxBytesLen + 1) 0 <> word32BE i)
    _ -> ScriptDataNumber (maxNumber + 1 + toInteger i)

  maxNumber = maximum (0 : [i | k <- keys, ScriptDataNumber i <- leaves k])
  maxBytesLen = maximum (0 : [BS.length bs | k <- keys, ScriptDataBytes bs <- leaves k])

{- | Apply a function to the first number or byte-string leaf of a datum, in
left-to-right order, keeping the surrounding structure. 'Nothing' when the
datum contains no leaf at all.
-}
replaceFirstLeaf :: (ScriptData -> ScriptData) -> ScriptData -> Maybe ScriptData
replaceFirstLeaf f sd = case sd of
  ScriptDataNumber{} -> Just (f sd)
  ScriptDataBytes{} -> Just (f sd)
  ScriptDataList xs -> ScriptDataList <$> inFirst xs
  ScriptDataConstructor idx fields -> ScriptDataConstructor idx <$> inFirst fields
  ScriptDataMap kvs ->
    -- Flatten to a member list and rebuild, so that a nested map's key is
    -- itself a candidate leaf position.
    ScriptDataMap . pairs <$> inFirst (unpairs kvs)
 where
  inFirst [] = Nothing
  inFirst (x : xs) = case replaceFirstLeaf f x of
    Just x' -> Just (x' : xs)
    Nothing -> (x :) <$> inFirst xs

  unpairs ps = concat [[k, v] | (k, v) <- ps]
  pairs (k : v : rest) = (k, v) : pairs rest
  pairs _ = []

-- | Every number and byte-string leaf of a datum.
leaves :: ScriptData -> [ScriptData]
leaves sd = case sd of
  ScriptDataNumber{} -> [sd]
  ScriptDataBytes{} -> [sd]
  ScriptDataList xs -> concatMap leaves xs
  ScriptDataConstructor _ fields -> concatMap leaves fields
  ScriptDataMap kvs -> concat [leaves k <> leaves v | (k, v) <- kvs]

-- | Big-endian 4-byte encoding, to make each generated key distinct.
word32BE :: Int -> BS.ByteString
word32BE i = BS.pack [fromIntegral (i `div` d `mod` 256) | d <- [16777216, 65536, 256, 1]]

{- | Name of a @ScriptData@ constructor, for the skip reason reported when
'bloatData' cannot bloat a datum.
-}
datumShape :: ScriptData -> String
datumShape sd = case sd of
  ScriptDataConstructor{} -> "Constr"
  ScriptDataList{} -> "List"
  ScriptDataMap{} -> "Map"
  ScriptDataNumber{} -> "Number"
  ScriptDataBytes{} -> "Bytes"

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
