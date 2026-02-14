module Cardano.CoinSelection.Types.TokenPolicyId.Gen
    ( genTokenPolicyId
    , genTokenPolicyIdLargeRange
    , shrinkTokenPolicyId
    , testTokenPolicyIds
    , mkTokenPolicyId
    ) where

import Prelude

import Cardano.CoinSelection.Types.Hash
    ( Hash (..)
    )
import Cardano.CoinSelection.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Test.QuickCheck
    ( Gen
    , elements
    , sized
    , vector
    )

import qualified Data.ByteString as BS

genTokenPolicyId :: Gen TokenPolicyId
genTokenPolicyId = sized $ \n ->
    elements $ take (max 1 n) testTokenPolicyIds

shrinkTokenPolicyId :: TokenPolicyId -> [TokenPolicyId]
shrinkTokenPolicyId i
    | i == simplest = []
    | otherwise = [simplest]
  where
    simplest = case testTokenPolicyIds of
        (x : _) -> x
        [] -> error "impossible"

genTokenPolicyIdLargeRange :: Gen TokenPolicyId
genTokenPolicyIdLargeRange =
    UnsafeTokenPolicyId . Hash . BS.pack <$> vector 28

testTokenPolicyIds :: [TokenPolicyId]
testTokenPolicyIds = mkTokenPolicyId <$> [0 .. 15]

{- | Create a test token policy ID from a byte value.

Constructs a 28-byte hash by repeating the given byte.
-}
mkTokenPolicyId :: Word -> TokenPolicyId
mkTokenPolicyId w =
    UnsafeTokenPolicyId . Hash . BS.pack $
        replicate 28 (fromIntegral w)
