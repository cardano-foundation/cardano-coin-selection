module Cardano.CoinSelection.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    , genTokenBundleSmallRangePositive
    , genTokenBundle
    , shrinkTokenBundle
    , shrinkTokenBundleSmallRange
    , shrinkTokenBundleSmallRangePositive
    ) where

import Prelude

import Cardano.CoinSelection.Gen.Extra
    ( shrinkInterleaved
    )
import Cardano.CoinSelection.Types.Coin.Gen
    ( genCoin
    , genCoinPositive
    , shrinkCoin
    , shrinkCoinPositive
    )
import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.CoinSelection.Types.TokenMap.Gen
    ( genTokenMap
    , genTokenMapSmallRange
    , shrinkTokenMap
    )
import Test.QuickCheck
    ( Gen
    )

genTokenBundle :: Gen TokenBundle
genTokenBundle = TokenBundle
    <$> genCoin
    <*> genTokenMap

shrinkTokenBundle :: TokenBundle -> [TokenBundle]
shrinkTokenBundle (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoin)
        (m, shrinkTokenMap)

genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange = TokenBundle
    <$> genCoin
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange = shrinkTokenBundle

genTokenBundleSmallRangePositive :: Gen TokenBundle
genTokenBundleSmallRangePositive = TokenBundle
    <$> genCoinPositive
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRangePositive
    :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRangePositive (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinPositive)
        (m, shrinkTokenMap)
