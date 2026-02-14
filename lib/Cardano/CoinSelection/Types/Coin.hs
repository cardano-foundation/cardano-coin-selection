{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the 'Coin' data type, which represents a quantity of
-- lovelace.
--
module Cardano.CoinSelection.Types.Coin
    ( -- * Type
      Coin (..)

      -- * Conversions (Safe)
    , fromIntegralMaybe
    , fromNatural
    , fromWord64
    , toInteger
    , toNatural
    , toWord64Maybe

      -- * Conversions (Unsafe)
    , unsafeFromIntegral
    , unsafeToWord64

      -- * Arithmetic operations
    , add
    , subtract
    , difference
    , distance
    ) where

import Prelude hiding
    ( fromIntegral
    , subtract
    , toInteger
    )

import Control.DeepSeq
    ( NFData (..)
    )
import Data.Bits
    ( Bits
    )
import Data.Hashable
    ( Hashable
    )
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Monoid
    ( Sum (..)
    )
import Data.Monoid.Cancellative
    ( LeftReductive
    , Reductive ((</>))
    , RightReductive
    )
import Data.Monoid.GCD
    ( GCDMonoid
    , LeftGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    , OverlappingGCDMonoid
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.Semigroup.Commutative
    ( Commutative
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Quiet
    ( Quiet (..)
    )

-- | A 'Coin' represents a quantity of lovelace.
--
-- Reminder: 1 ada = 1,000,000 lovelace.
--
-- The 'Coin' type has 'Semigroup' and 'Monoid' instances that correspond
-- to ordinary addition and summation.
--
newtype Coin = Coin
    { unCoin :: Natural
    }
    deriving stock (Ord, Eq, Generic)
    deriving (Read, Show) via Quiet Coin
    deriving (Commutative, Semigroup, Monoid, MonoidNull) via Sum Natural
    deriving (LeftReductive, RightReductive, Reductive) via Sum Natural
    deriving (LeftGCDMonoid, RightGCDMonoid, GCDMonoid) via Sum Natural
    deriving (OverlappingGCDMonoid, Monus) via Sum Natural

instance NFData Coin
instance Hashable Coin

--------------------------------------------------------------------------------
-- Conversions (Safe)
--------------------------------------------------------------------------------

-- | Constructs a 'Coin' from an 'Integral' value.
--
-- Returns 'Nothing' if the given value is negative.
--
fromIntegralMaybe :: (Bits i, Integral i) => i -> Maybe Coin
fromIntegralMaybe i = Coin <$> intCastMaybe i

-- | Constructs a 'Coin' from a 'Natural' value.
--
fromNatural :: Natural -> Coin
fromNatural = Coin

-- | Constructs a 'Coin' from a 'Word64' value.
--
fromWord64 :: Word64 -> Coin
fromWord64 = Coin . intCast

-- | Converts a 'Coin' to an 'Integer' value.
--
toInteger :: Coin -> Integer
toInteger = intCast . unCoin

-- | Converts a 'Coin' to a 'Natural' value.
--
toNatural :: Coin -> Natural
toNatural = unCoin

-- | Converts a 'Coin' to a 'Word64' value.
--
-- Returns 'Nothing' if the given value does not fit within the bounds of a
-- 64-bit word.
--
toWord64Maybe :: Coin -> Maybe Word64
toWord64Maybe (Coin c) = intCastMaybe c

--------------------------------------------------------------------------------
-- Conversions (Unsafe)
-------------------------------------------------------------------------------

-- | Constructs a 'Coin' from an 'Integral' value.
--
-- Callers of this function must take responsibility for checking that the
-- given value is not negative.
--
-- Produces a run-time error if the given value is negative.
--
unsafeFromIntegral
    :: HasCallStack
    => (Bits i, Integral i, Show i)
    => i
    -> Coin
unsafeFromIntegral i = fromMaybe onError (fromIntegralMaybe i)
  where
    onError =  error $ unwords
        [ "Coin.unsafeFromIntegral:"
        , show i
        , "is not a natural number."
        ]

-- | Converts a 'Coin' to a 'Word64' value.
--
-- Callers of this function must take responsibility for checking that the
-- given value will fit within the bounds of a 64-bit word.
--
-- Produces a run-time error if the given value is out of bounds.
--
unsafeToWord64 :: HasCallStack => Coin -> Word64
unsafeToWord64 c = fromMaybe onError (toWord64Maybe c)
  where
    onError = error $ unwords
        [ "Coin.unsafeToWord64:"
        , show c
        , "does not fit within the bounds of a 64-bit word."
        ]

--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------

-- | Subtracts the second coin from the first.
--
-- Returns 'Nothing' if the second coin is strictly greater than the first.
--
subtract :: Coin -> Coin -> Maybe Coin
subtract = (</>)

-- | Calculates the combined value of two coins.
--
add :: Coin -> Coin -> Coin
add = (<>)

-- | Subtracts the second coin from the first.
--
-- Returns 'Coin 0' if the second coin is strictly greater than the first.
--
difference :: Coin -> Coin -> Coin
difference = (<\>)

-- | Absolute difference between two coin amounts. The result is never negative.
distance :: Coin -> Coin -> Coin
distance a b = (a <\> b) <> (b <\> a)

-- NOTE: equipartition, partition, partitionDefault, and unsafePartition
-- have been removed because they depend on equipartitionNatural and
-- partitionNatural from Cardano.Numeric.Util, which are not included
-- in this package. These functions remain in cardano-wallet.
