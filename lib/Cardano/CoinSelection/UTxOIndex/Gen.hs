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
import Cardano.CoinSelection.UTxOIndex
    ( UTxOIndex
    )
import Control.Monad
    ( replicateM
    )
import Generics.SOP
    ( NP (..)
    )
import Test.QuickCheck
    ( Gen
    , choose
    , listOf
    , shrinkList
    , shrinkMapBy
    )

-- TODO: These imports need cardano-wallet-test-utils or equivalent:
-- import Cardano.CoinSelection.Types.TokenBundle.Gen
--     ( genTokenBundleSmallRangePositive
--     , shrinkTokenBundleSmallRangePositive
--     )
-- import Test.QuickCheck.Extra
--     ( genericRoundRobinShrink
--     , (<:>)
--     , (<@>)
--     )

import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex

-- TODO: The following generators and shrinkers depend on Gen modules
-- (genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive)
-- and Test.QuickCheck.Extra (genericRoundRobinShrink, (<:>), (<@>)) from
-- cardano-wallet-test-utils.
-- These need to be provided by cardano-coin-selection or re-implemented.

--------------------------------------------------------------------------------
-- Indices generated according to the size parameter
--------------------------------------------------------------------------------

{- | Generates a random 'UTxOIndex'.

TODO: Restore full implementation once genTokenBundleSmallRangePositive
is available.
-}
genUTxOIndex :: forall u. (Ord u) => Gen u -> Gen (UTxOIndex u)
genUTxOIndex _genUTxO =
    error
        "genUTxOIndex: requires genTokenBundleSmallRangePositive from \
        \cardano-wallet-test-utils"

{- | Shrinks a 'UTxOIndex'.

TODO: Restore full implementation once shrinkTokenBundleSmallRangePositive
and Test.QuickCheck.Extra are available.
-}
shrinkUTxOIndex
    :: forall u. (Ord u) => (u -> [u]) -> UTxOIndex u -> [UTxOIndex u]
shrinkUTxOIndex _shrinkUTxO _index = []

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

{- | Generates a large random 'UTxOIndex'.

TODO: Restore full implementation once genTokenBundleSmallRangePositive
is available.
-}
genUTxOIndexLarge :: (Ord u) => Gen u -> Gen (UTxOIndex u)
genUTxOIndexLarge _genUTxO =
    error
        "genUTxOIndexLarge: requires genTokenBundleSmallRangePositive from \
        \cardano-wallet-test-utils"

{- | Generates a large random 'UTxOIndex' with a specified number of entries.

TODO: Restore full implementation once genTokenBundleSmallRangePositive
is available.
-}
genUTxOIndexLargeN
    :: forall u. (Ord u) => Gen u -> Int -> Gen (UTxOIndex u)
genUTxOIndexLargeN _genUTxO _n =
    error
        "genUTxOIndexLargeN: requires genTokenBundleSmallRangePositive from \
        \cardano-wallet-test-utils"
