module Cardano.CoinSelection.Types.AssetId.Gen
    ( genAssetId
    , genAssetIdLargeRange
    , shrinkAssetId
    ) where

import Prelude

import Cardano.CoinSelection.Gen.Extra
    ( genSized2With
    , shrinkInterleaved
    )
import Cardano.CoinSelection.Types.AssetId
    ( AssetId (..)
    )
import Cardano.CoinSelection.Types.AssetName.Gen
    ( genAssetName
    , genAssetNameLargeRange
    , shrinkAssetName
    )
import Cardano.CoinSelection.Types.TokenPolicyId.Gen
    ( genTokenPolicyId
    , genTokenPolicyIdLargeRange
    , shrinkTokenPolicyId
    )
import Test.QuickCheck
    ( Gen
    )

genAssetId :: Gen AssetId
genAssetId =
    genSized2With AssetId genTokenPolicyId genAssetName

shrinkAssetId :: AssetId -> [AssetId]
shrinkAssetId (AssetId p t) =
    uncurry AssetId
        <$> shrinkInterleaved
            (p, shrinkTokenPolicyId)
            (t, shrinkAssetName)

genAssetIdLargeRange :: Gen AssetId
genAssetIdLargeRange =
    AssetId
        <$> genTokenPolicyIdLargeRange
        <*> genAssetNameLargeRange
