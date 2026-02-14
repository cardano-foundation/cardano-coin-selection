{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
module Cardano.CoinSelection.Types.AssetId
    ( AssetId (..)
    ) where

import Prelude

import Cardano.CoinSelection.Types.AssetName
    ( AssetName
    )
import Cardano.CoinSelection.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Control.DeepSeq
    ( NFData
    )
import GHC.Generics
    ( Generic
    )

{- | A combination of a token policy identifier and an asset name that can be
  used as a compound identifier.
-}
data AssetId = AssetId
    { policyId
        :: !TokenPolicyId
    , assetName
        :: !AssetName
    }
    deriving stock (Eq, Generic, Ord, Read, Show)

instance NFData AssetId
