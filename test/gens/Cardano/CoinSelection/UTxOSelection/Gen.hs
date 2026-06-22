{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
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
import Test.QuickCheck.Gen.Unsafe
    ( promote
    )

import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection

--------------------------------------------------------------------------------
-- Selections that may be empty
--------------------------------------------------------------------------------

-- | Generates a random 'UTxOSelection'.
genUTxOSelection
    :: forall u
     . (Ord u, Show u)
    => Gen u
    -> Gen (UTxOSelection u)
genUTxOSelection genUTxO =
    UTxOSelection.fromIndexFiltered
        <$> genUTxOFilter
        <*> genUTxOIndex genUTxO
  where
    genUTxOFilter :: Gen (u -> Bool)
    genUTxOFilter =
        genFunction coarbitraryShow arbitrary

shrinkUTxOSelection
    :: (Ord u) => (u -> [u]) -> (UTxOSelection u -> [UTxOSelection u])
shrinkUTxOSelection shrinkUTxO =
    shrinkMapBy UTxOSelection.fromIndexPair UTxOSelection.toIndexPair $
        liftShrink2
            (shrinkUTxOIndex shrinkUTxO)
            (shrinkUTxOIndex shrinkUTxO)

--------------------------------------------------------------------------------
-- Selections that are non-empty
--------------------------------------------------------------------------------

-- | Generates a random non-empty 'UTxOSelectionNonEmpty'.
genUTxOSelectionNonEmpty
    :: (Ord u, Show u)
    => Gen u
    -> Gen (UTxOSelectionNonEmpty u)
genUTxOSelectionNonEmpty genUTxO =
    genUTxOSelection genUTxO
        `suchThatMap` UTxOSelection.toNonEmpty

shrinkUTxOSelectionNonEmpty
    :: (Ord u)
    => (u -> [u]) -> (UTxOSelectionNonEmpty u -> [UTxOSelectionNonEmpty u])
shrinkUTxOSelectionNonEmpty shrinkUTxO =
    mapMaybe UTxOSelection.toNonEmpty
        . shrinkUTxOSelection shrinkUTxO
        . UTxOSelection.fromNonEmpty

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- | Generates a function using QuickCheck's coarbitrary mechanism.
genFunction
    :: (a -> Gen b -> Gen b) -> Gen b -> Gen (a -> b)
genFunction coarbitraryFn gen =
    promote (`coarbitraryFn` gen)
