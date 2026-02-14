{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | QuickCheck utilities extracted from @cardano-wallet-test-utils@.
module Cardano.CoinSelection.Gen.Extra
    ( chooseNatural
    , shrinkNatural
    , genSized2
    , genSized2With
    , shrinkInterleaved
    ) where

import Prelude

import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Maybe
    ( mapMaybe
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( Gen
    , chooseInteger
    , scale
    , shrinkIntegral
    , suchThatMap
    )

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (lo, hi) =
    chooseInteger (intCast lo, intCast hi)
        `suchThatMap` intCastMaybe @Integer @Natural

shrinkNatural :: Natural -> [Natural]
shrinkNatural n =
    mapMaybe (intCastMaybe @Integer @Natural) $
        shrinkIntegral $
            intCast n

genSized2 :: Gen a -> Gen b -> Gen (a, b)
genSized2 genA genB =
    (,)
        <$> scaleToRoot 2 genA
        <*> scaleToRoot 2 genB

genSized2With :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
genSized2With f genA genB = uncurry f <$> genSized2 genA genB

shrinkInterleaved
    :: (a, a -> [a]) -> (b, b -> [b]) -> [(a, b)]
shrinkInterleaved (a, shrinkA) (b, shrinkB) =
    interleave
        [(a', b) | a' <- shrinkA a]
        [(a, b') | b' <- shrinkB b]
  where
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    interleave xs [] = xs
    interleave [] ys = ys

scaleToRoot :: Int -> Gen a -> Gen a
scaleToRoot n =
    scale $
        floor @Double @Int
            . (** (1.0 / fromIntegral @Int @Double n))
            . fromIntegral @Int @Double
