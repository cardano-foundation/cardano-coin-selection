{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CoinSelection.Internal.Numeric
    (
      -- * Coalescing values
      padCoalesce

      -- * Partial orders
    , inAscendingPartialOrder

    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..)
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
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

-- | Adjusts the source list so that its length is the same as the target list,
--   either by padding the list, or by coalescing a subset of the elements,
--   while preserving the total sum.
--
-- If the source list is shorter than the target list, this function repeatedly
-- inserts 'mempty' into the list until the desired length has been reached.
--
-- If the source list is longer than the target list, this function repeatedly
-- coalesces the smallest pair of elements with '<>' until the desired length
-- has been reached.
--
-- The resulting list is guaranteed to be sorted into ascending order, and the
-- sum of the elements is guaranteed to be the same as the sum of elements in
-- the source list.
--
-- Examples (shown with ordinary list notation):
--
-- >>> padCoalesce [Sum 1] (replicate 4 ())
-- [Sum 0, Sum 0, Sum 0, Sum 1]
--
-- >>> padCoalesce [Sum (-1)] (replicate 4 ())
-- [Sum (-1), Sum 0, Sum 0, Sum 0]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 3 ())
-- [Sum 3, Sum 4, Sum 8]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 2 ())
-- [Sum 7, Sum 8]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 1 ())
-- [Sum 15]
--
padCoalesce :: forall m a. (Monoid m, Ord m)
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
