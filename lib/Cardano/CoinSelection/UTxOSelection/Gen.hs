{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CoinSelection.UTxOSelection.Gen
    ( genUTxOSelection
    , genUTxOSelectionNonEmpty
    , shrinkUTxOSelection
    , shrinkUTxOSelectionNonEmpty
    )
    where

import Prelude

import Cardano.CoinSelection.UTxOIndex.Gen
    ( genUTxOIndex
    , shrinkUTxOIndex
    )
import Cardano.CoinSelection.UTxOSelection
    ( UTxOSelection
    , UTxOSelectionNonEmpty
    )
import Data.Maybe
    ( mapMaybe
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , liftShrink2
    , shrinkMapBy
    , suchThatMap
    )
import Test.QuickCheck.Arbitrary
    ( coarbitraryShow
    )

-- TODO: This import needs cardano-wallet-test-utils or equivalent:
-- import Test.QuickCheck.Extra
--     ( genFunction
--     )

import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection

-- TODO: genUTxOSelection depends on genFunction from
-- Test.QuickCheck.Extra (cardano-wallet-test-utils).
-- This needs to be provided by cardano-coin-selection or re-implemented.

--------------------------------------------------------------------------------
-- Selections that may be empty
--------------------------------------------------------------------------------

-- | Generates a random 'UTxOSelection'.
--
-- TODO: Restore full implementation once genFunction from
-- Test.QuickCheck.Extra is available.
genUTxOSelection :: forall u. (Ord u, Show u) => Gen u -> Gen (UTxOSelection u)
genUTxOSelection _genUTxO = error
    "genUTxOSelection: requires genFunction from cardano-wallet-test-utils"

shrinkUTxOSelection
    :: Ord u => (u -> [u]) -> (UTxOSelection u -> [UTxOSelection u])
shrinkUTxOSelection shrinkUTxO =
    shrinkMapBy UTxOSelection.fromIndexPair UTxOSelection.toIndexPair $
        liftShrink2
            (shrinkUTxOIndex shrinkUTxO)
            (shrinkUTxOIndex shrinkUTxO)

--------------------------------------------------------------------------------
-- Selections that are non-empty
--------------------------------------------------------------------------------

-- | Generates a random non-empty 'UTxOSelectionNonEmpty'.
--
-- TODO: Restore full implementation once genUTxOSelection is working.
genUTxOSelectionNonEmpty
    :: (Ord u, Show u) => Gen u -> Gen (UTxOSelectionNonEmpty u)
genUTxOSelectionNonEmpty _genUTxO = error
    "genUTxOSelectionNonEmpty: requires genFunction from \
    \cardano-wallet-test-utils"

shrinkUTxOSelectionNonEmpty
    :: Ord u => (u -> [u]) -> (UTxOSelectionNonEmpty u -> [UTxOSelectionNonEmpty u])
shrinkUTxOSelectionNonEmpty shrinkUTxO
    = mapMaybe UTxOSelection.toNonEmpty
    . shrinkUTxOSelection shrinkUTxO
    . UTxOSelection.fromNonEmpty
