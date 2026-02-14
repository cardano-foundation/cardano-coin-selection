{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
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
import Cardano.CoinSelection.Gen.Extra
    ( genericRoundRobinShrink
    , (<:>)
    , (<@>)
    )
import Cardano.CoinSelection.Types.AssetId.Gen
    ( genAssetId
    , shrinkAssetId
    )
import Cardano.CoinSelection.Types.Coin
    ( Coin (..)
    )
import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.CoinSelection.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
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

import qualified Cardano.CoinSelection.Types.TokenBundle as TokenBundle
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Selection skeletons
--------------------------------------------------------------------------------

genSelectionSkeleton
    :: Gen (Address ctx) -> Gen (SelectionSkeleton ctx)
genSelectionSkeleton genAddress =
    SelectionSkeleton
        <$> genSkeletonInputCount
        <*> genSkeletonOutputs
        <*> genSkeletonChange
  where
    genSkeletonInputCount =
        getNonNegative <$> arbitrary @(NonNegative Int)
    genSkeletonOutputs =
        listOf genSkeletonOutput
    genSkeletonOutput =
        (,)
            <$> genAddress
            <*> genTokenBundleSmallRange
                `suchThat` tokenBundleHasNonZeroCoin
    genSkeletonChange =
        listOf (Set.fromList <$> listOf genAssetId)

shrinkSelectionSkeleton
    :: (Address ctx -> [Address ctx])
    -> (SelectionSkeleton ctx -> [SelectionSkeleton ctx])
shrinkSelectionSkeleton shrinkAddress =
    genericRoundRobinShrink
        <@> shrinkSkeletonInputCount
        <:> shrinkSkeletonOutputs
        <:> shrinkSkeletonChange
        <:> Nil
  where
    shrinkSkeletonInputCount =
        shrink @Int
    shrinkSkeletonOutputs =
        shrinkList shrinkSkeletonOutput
    shrinkSkeletonOutput =
        genericRoundRobinShrink
            <@> shrinkAddress
            <:> filter tokenBundleHasNonZeroCoin
            . shrinkTokenBundleSmallRange
            <:> Nil
    shrinkSkeletonChange =
        shrinkList $
            shrinkMapBy
                Set.fromList
                Set.toList
                (shrinkList shrinkAssetId)

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b =
    TokenBundle.getCoin b /= Coin 0

--------------------------------------------------------------------------------
-- Selection strategies
--------------------------------------------------------------------------------

genSelectionStrategy :: Gen SelectionStrategy
genSelectionStrategy = arbitraryBoundedEnum

shrinkSelectionStrategy
    :: SelectionStrategy -> [SelectionStrategy]
shrinkSelectionStrategy = \case
    SelectionStrategyMinimal -> []
    SelectionStrategyOptimal -> [SelectionStrategyMinimal]
