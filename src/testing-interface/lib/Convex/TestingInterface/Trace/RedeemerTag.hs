{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Build a 'RedeemerTagger' for the opt-in redeemer labels streamed by
@--streaming-json@ (the @redeemerKind@ / @redeemerPayload@ fields on each
script input).

The 'RedeemerTagger' is a method on the 'TestingInterface' class with a no-op
default, so you only need one of the helpers below when you actually want
human-readable redeemer labels in the stream. The tagger is handed the redeemer
as Plutus 'Data', so it behaves the same for Plutus and Aiken validators
provided the Haskell side has a 'FromData' instance for the redeemer type.

* 'autoRedeemerTag' decodes the redeemer and labels it with its 'show' output.
  That is enough for plain enumerations such as @Ping | Pong | Stop@, where the
  constructor name is the label you want.

* 'labelRedeemer' decodes the redeemer and runs your own function over it, so
  you control both the label and an optional JSON payload. Reach for it when
  'show' is not a clean label (for example a single-constructor record) or when
  you want the redeemer's fields attached as payload.

If neither fits, build a 'RedeemerTagger' straight from any
'Data -> Maybe RedeemerTag' function; @AikenBankSpec@ shows this by matching on
'Constr' indices by hand.

The @PingPongSpec@ is the simplest end-to-end example:

@
instance TestingInterface MyModel where
  redeemerTagger = autoRedeemerTag (Proxy \@MyRedeemer)
@
-}
module Convex.TestingInterface.Trace.RedeemerTag (
  autoRedeemerTag,
  labelRedeemer,
) where

import Data.Text qualified as Text
import PlutusLedgerApi.V1 qualified as PlutusLedgerApi
import PlutusTx.IsData.Class (FromData)

import Convex.TestingInterface.Trace (
  RedeemerTag (..),
  RedeemerTagger (..),
 )

{- | Label a redeemer with its 'show' output and no payload.

Returns 'Nothing' when the 'Data' does not decode as @r@, so the tagger is a
no-op for that redeemer and (when several taggers are combined via their
'Monoid' instance) the next one gets a chance.
-}
autoRedeemerTag
  :: forall r proxy
   . (FromData r, Show r)
  => proxy r
  -> RedeemerTagger
autoRedeemerTag _ =
  RedeemerTagger $ \d ->
    case PlutusLedgerApi.fromData d :: Maybe r of
      Just val -> Just (RedeemerTag (Text.pack (show val)) Nothing)
      Nothing -> Nothing

{- | Decode the redeemer as @r@ and turn it into a 'RedeemerTag' with a
user-supplied function, giving full control over the label and an optional
JSON payload.

Returns 'Nothing' when the 'Data' does not decode as @r@.
-}
labelRedeemer
  :: forall r proxy
   . (FromData r)
  => proxy r
  -> (r -> RedeemerTag)
  -> RedeemerTagger
labelRedeemer _ f =
  RedeemerTagger $ \d ->
    case PlutusLedgerApi.fromData d :: Maybe r of
      Just val -> Just (f val)
      Nothing -> Nothing
