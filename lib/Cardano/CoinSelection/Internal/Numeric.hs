{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
module Cardano.CoinSelection.Internal.Numeric
    ( -- * Coalescing values
      padCoalesce

      -- * Partitioning natural numbers
    , equipartitionNatural
    , partitionNatural

      -- * Partial orders
    , inAscendingPartialOrder
    ) where

import Prelude hiding
    ( round
    )

import Algebra.PartialOrd
    ( PartialOrd (..)
    )
import Control.Arrow
    ( (&&&)
    )
import Data.Function
    ( (&)
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Ord
    ( Down (..)
    , comparing
    )
import Data.Ratio
    ( (%)
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Safe
    ( tailMay
    )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Public functions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Coalescing values
--------------------------------------------------------------------------------

{- | Adjusts the source list so that its length is the same as the target list,
  either by padding the list, or by coalescing a subset of the elements,
  while preserving the total sum.

If the source list is shorter than the target list, this function repeatedly
inserts 'mempty' into the list until the desired length has been reached.

If the source list is longer than the target list, this function repeatedly
coalesces the smallest pair of elements with '<>' until the desired length
has been reached.

The resulting list is guaranteed to be sorted into ascending order, and the
sum of the elements is guaranteed to be the same as the sum of elements in
the source list.

Examples (shown with ordinary list notation):

>>> padCoalesce [Sum 1] (replicate 4 ())
[Sum 0, Sum 0, Sum 0, Sum 1]

>>> padCoalesce [Sum (-1)] (replicate 4 ())
[Sum (-1), Sum 0, Sum 0, Sum 0]

>>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 3 ())
[Sum 3, Sum 4, Sum 8]

>>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 2 ())
[Sum 7, Sum 8]

>>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 1 ())
[Sum 15]
-}
padCoalesce
    :: forall m a
     . (Monoid m, Ord m)
    => NonEmpty m
    -- ^ Source list
    -> NonEmpty a
    -- ^ Target list
    -> NonEmpty m
padCoalesce sourceUnsorted target
    | sourceLength < targetLength =
        applyN (targetLength - sourceLength) pad source
    | sourceLength > targetLength =
        applyN (sourceLength - targetLength) coalesce source
    | otherwise =
        source
  where
    source = NE.sort sourceUnsorted

    sourceLength = NE.length source
    targetLength = NE.length target

    pad :: NonEmpty m -> NonEmpty m
    pad = NE.insert mempty

    coalesce :: NonEmpty m -> NonEmpty m
    coalesce (x :| y : zs) = NE.insert (x <> y) zs
    coalesce xs = xs

--------------------------------------------------------------------------------
-- Partial orders
--------------------------------------------------------------------------------

inAscendingPartialOrder :: (Foldable f, PartialOrd a) => f a -> Bool
inAscendingPartialOrder = all (uncurry leq) . consecutivePairs . F.toList

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

-- Apply the same function multiple times to a value.
--
applyN :: Int -> (a -> a) -> a -> a
applyN n f = F.foldr (.) id (replicate n f)

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

--------------------------------------------------------------------------------
-- Partitioning natural numbers
--------------------------------------------------------------------------------

{- | Partitions a natural number into equal parts.

The resulting list is sorted in ascending order.
-}
equipartitionNatural
    :: (HasCallStack)
    => Natural
    -> NonEmpty a
    -> NonEmpty Natural
equipartitionNatural n count =
    NE.reverse $
        fromMaybe zeroError $
            partitionNatural n (1 <$ count)
  where
    zeroError =
        error
            "equipartitionNatural: count must be non-empty."

{- | Partitions a natural number into a number of parts, where the size of
  each part is proportional to the size of its corresponding element in the
  given list of weights.
-}
partitionNatural
    :: Natural
    -> NonEmpty Natural
    -> Maybe (NonEmpty Natural)
partitionNatural target weights
    | totalWeight == 0 = Nothing
    | otherwise = Just portionsRounded
  where
    portionsRounded :: NonEmpty Natural
    portionsRounded =
        portionsUnrounded
            & NE.zip indices
            & NE.sortBy
                ( comparing
                    (Down . (fractionalPart &&& integralPart) . snd)
                )
            & NE.zipWith (fmap . round) roundings
            & NE.sortBy (comparing fst)
            & fmap snd
      where
        indices :: NonEmpty Int
        indices = 0 :| [1 ..]

    portionsUnrounded :: NonEmpty Rational
    portionsUnrounded = computeIdealPortion <$> weights
      where
        computeIdealPortion c =
            fromIntegral target
                * fromIntegral c
                % fromIntegral totalWeight

    roundings :: NonEmpty RoundingDirection
    roundings =
        applyN shortfall (NE.cons RoundUp) (NE.repeat RoundDown)
      where
        shortfall =
            fromIntegral target
                - fromIntegral @Integer
                    (F.sum $ round RoundDown <$> portionsUnrounded)

    totalWeight :: Natural
    totalWeight = F.sum weights

fractionalPart :: Rational -> Rational
fractionalPart = snd . properFraction @_ @Integer

integralPart :: Rational -> Integer
integralPart = floor

data RoundingDirection
    = RoundUp
    | RoundDown
    deriving (Eq, Show)

round
    :: (RealFrac a, Integral b)
    => RoundingDirection -> a -> b
round = \case
    RoundUp -> ceiling
    RoundDown -> floor
