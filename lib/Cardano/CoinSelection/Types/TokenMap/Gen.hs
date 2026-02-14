module Cardano.CoinSelection.Types.TokenMap.Gen
    ( genTokenMap
    , genTokenMapSmallRange
    , shrinkTokenMap
    ) where

import Prelude

import Cardano.CoinSelection.Gen.Extra
    ( shrinkInterleaved
    )
import Cardano.CoinSelection.Types.AssetId.Gen
    ( genAssetId
    , shrinkAssetId
    )
import Cardano.CoinSelection.Types.TokenMap
    ( TokenMap
    )
import Cardano.CoinSelection.Types.TokenQuantity.Gen
    ( genTokenQuantity
    , shrinkTokenQuantity
    )
import Control.Monad
    ( replicateM
    )
import Test.QuickCheck
    ( Gen
    , choose
    , oneof
    , shrinkList
    , sized
    )

import qualified Cardano.CoinSelection.Types.TokenMap as TokenMap

genTokenMap :: Gen TokenMap
genTokenMap = sized $ \size -> do
    assetCount <- choose (0, size)
    TokenMap.fromFlatList
        <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetId
        <*> genTokenQuantity

genTokenMapSmallRange :: Gen TokenMap
genTokenMapSmallRange = do
    assetCount <- oneof
        [ pure 0
        , pure 1
        , choose (2, 16)
        ]
    TokenMap.fromFlatList
        <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetId
        <*> genTokenQuantity

shrinkTokenMap :: TokenMap -> [TokenMap]
shrinkTokenMap
    = fmap TokenMap.fromFlatList
    . shrinkList shrinkAssetQuantity
    . TokenMap.toFlatList
  where
    shrinkAssetQuantity (a, q) = shrinkInterleaved
        (a, shrinkAssetId)
        (q, shrinkTokenQuantity)
