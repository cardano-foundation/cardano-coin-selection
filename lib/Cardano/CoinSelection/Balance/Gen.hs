{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CoinSelection.Balance.Gen
    ( genSelectionSkeleton
    , genSelectionStrategy
    , shrinkSelectionSkeleton
    , shrinkSelectionStrategy
    )
where

import Prelude

import Cardano.CoinSelection.Balance
    ( SelectionSkeleton (..)
    , SelectionStrategy (..)
    )
import Cardano.CoinSelection.Context
    ( SelectionContext (..)
    )
import Cardano.CoinSelection.Types.Coin
    ( Coin (..)
    )
import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle
    )
import Generics.SOP
    ( NP (..)
    )
import Test.QuickCheck
    ( Gen
    , NonNegative (..)
    , arbitrary
    , arbitraryBoundedEnum
    , listOf
    , shrink
    , shrinkList
    , shrinkMapBy
    , suchThat
    )

-- TODO: These imports need cardano-wallet-test-utils or equivalent:
-- import Cardano.CoinSelection.Types.AssetId.Gen
--     ( genAssetId
--     , shrinkAssetId
--     )
-- import Cardano.CoinSelection.Types.TokenBundle.Gen
--     ( genTokenBundleSmallRange
--     , shrinkTokenBundleSmallRange
--     )
-- import Test.QuickCheck.Extra
--     ( genericRoundRobinShrink
--     , (<:>)
--     , (<@>)
--     )

import qualified Cardano.CoinSelection.Types.TokenBundle as TokenBundle
import qualified Data.Set as Set

-- TODO: The following generators and shrinkers depend on Gen modules
-- (genAssetId, shrinkAssetId, genTokenBundleSmallRange,
-- shrinkTokenBundleSmallRange) and Test.QuickCheck.Extra
-- (genericRoundRobinShrink, (<:>), (<@>)) from cardano-wallet-test-utils.
-- These need to be provided by cardano-coin-selection or re-implemented.

--------------------------------------------------------------------------------
-- Selection skeletons
--------------------------------------------------------------------------------

{- | Generates a random 'SelectionSkeleton'.

TODO: Restore full implementation once Gen modules for AssetId and
TokenBundle are available.
-}
genSelectionSkeleton
    :: Gen (Address ctx) -> Gen (SelectionSkeleton ctx)
genSelectionSkeleton _genAddress =
    error
        "genSelectionSkeleton: requires genAssetId, genTokenBundleSmallRange, \
        \and genAddress generators from cardano-wallet-test-utils"

{- | Shrinks a 'SelectionSkeleton'.

TODO: Restore full implementation once Gen modules and
Test.QuickCheck.Extra are available.
-}
shrinkSelectionSkeleton
    :: (Address ctx -> [Address ctx])
    -> (SelectionSkeleton ctx -> [SelectionSkeleton ctx])
shrinkSelectionSkeleton _shrinkAddress _skeleton = []

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

--------------------------------------------------------------------------------
-- Selection strategies
--------------------------------------------------------------------------------

genSelectionStrategy :: Gen SelectionStrategy
genSelectionStrategy = arbitraryBoundedEnum

shrinkSelectionStrategy :: SelectionStrategy -> [SelectionStrategy]
shrinkSelectionStrategy = \case
    -- Shrinking from "optimal" to "minimal" should increase the likelihood of
    -- making a successful selection, as the "minimal" strategy is designed to
    -- generate smaller selections.
    SelectionStrategyMinimal -> []
    SelectionStrategyOptimal -> [SelectionStrategyMinimal]
