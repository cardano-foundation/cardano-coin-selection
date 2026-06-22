{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
module Cardano.CoinSelection.Types.AssetName.Gen
    ( genAssetName
    , genAssetNameLargeRange
    , shrinkAssetName
    , testAssetNames
    , mkAssetName
    ) where

import Prelude

import Cardano.CoinSelection.Types.AssetName
    ( AssetName (..)
    )
import Test.QuickCheck
    ( Gen
    , elements
    , sized
    , vector
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

genAssetName :: Gen AssetName
genAssetName = sized $ \n ->
    elements $ take (max 1 n) testAssetNames

shrinkAssetName :: AssetName -> [AssetName]
shrinkAssetName i
    | i == simplest = []
    | otherwise = [simplest]
  where
    simplest = case testAssetNames of
        (x : _) -> x
        [] -> error "impossible"

genAssetNameLargeRange :: Gen AssetName
genAssetNameLargeRange =
    UnsafeAssetName . BS.pack <$> vector 32

testAssetNames :: [AssetName]
testAssetNames = mkAssetName <$> ['A' .. 'Z']

mkAssetName :: Char -> AssetName
mkAssetName = UnsafeAssetName . B8.snoc "Asset"
