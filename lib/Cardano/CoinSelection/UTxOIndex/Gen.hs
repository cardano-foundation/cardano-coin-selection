{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CoinSelection.UTxOIndex.Gen
    ( genUTxOIndex
    , genUTxOIndexLarge
    , genUTxOIndexLargeN
    , shrinkUTxOIndex
    ) where

import Prelude

import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.CoinSelection.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive
    , shrinkTokenBundleSmallRangePositive
    )
import Cardano.CoinSelection.UTxOIndex
    ( UTxOIndex
    )
import Control.Monad
    ( replicateM
    )
import Test.QuickCheck
    ( Gen
    , choose
    , liftShrink2
    , listOf
    , shrinkList
    , shrinkMapBy
    )

import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Indices generated according to the size parameter
--------------------------------------------------------------------------------

-- | Generates a random 'UTxOIndex'.
genUTxOIndex
    :: forall u. (Ord u) => Gen u -> Gen (UTxOIndex u)
genUTxOIndex genUTxO =
    UTxOIndex.fromSequence <$> listOf genEntry
  where
    genEntry :: Gen (u, TokenBundle)
    genEntry =
        (,)
            <$> genUTxO
            <*> genTokenBundleSmallRangePositive

-- | Shrinks a 'UTxOIndex'.
shrinkUTxOIndex
    :: forall u
     . (Ord u)
    => (u -> [u])
    -> UTxOIndex u
    -> [UTxOIndex u]
shrinkUTxOIndex shrinkUTxO =
    shrinkMapBy
        UTxOIndex.fromSequence
        UTxOIndex.toList
        (shrinkList shrinkEntry)
  where
    shrinkEntry :: (u, TokenBundle) -> [(u, TokenBundle)]
    shrinkEntry =
        liftShrink2
            shrinkUTxO
            shrinkTokenBundleSmallRangePositive

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

-- | Generates a large random 'UTxOIndex'.
genUTxOIndexLarge
    :: (Ord u) => Gen u -> Gen (UTxOIndex u)
genUTxOIndexLarge genUTxO =
    genUTxOIndexLargeN genUTxO =<< choose (1024, 4096)

-- | Generates a large random 'UTxOIndex' with a specified number of entries.
genUTxOIndexLargeN
    :: forall u
     . (Ord u)
    => Gen u
    -> Int
    -> Gen (UTxOIndex u)
genUTxOIndexLargeN genUTxO n =
    UTxOIndex.fromSequence <$> replicateM n genEntry
  where
    genEntry :: Gen (u, TokenBundle)
    genEntry =
        (,)
            <$> genUTxO
            <*> genTokenBundleSmallRangePositive
