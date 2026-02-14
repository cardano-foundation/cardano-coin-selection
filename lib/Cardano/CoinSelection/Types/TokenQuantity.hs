{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Represents an integral quantity of tokens.
--
module Cardano.CoinSelection.Types.TokenQuantity
    (
      -- * Type
      TokenQuantity (..)

      -- * Values
    , zero

      -- * Arithmetic operations
    , add
    , subtract
    , pred
    , predZero
    , succ
    , difference

      -- * Tests
    , isNonZero
    , isZero

    ) where

import Prelude hiding
    ( pred
    , subtract
    , succ
    )

import Control.DeepSeq
    ( NFData (..)
    )
import Data.Data
    ( Data
    )
import Data.Hashable
    ( Hashable
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
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )
import Quiet
    ( Quiet (..)
    )

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Represents an integral quantity of tokens.
--
-- At present, we use 'Natural' as our underlying type, as the only use case
-- for these quantities is to be included in token bundles held within
-- transaction outputs, and these must never be negative.
--
-- When we build support for minting and burning of tokens, we may wish to
-- parameterize this type and allow it to be instantiated with 'Integer'.
--
newtype TokenQuantity = TokenQuantity
    { unTokenQuantity :: Natural }
    deriving stock (Data, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)
    deriving (Read, Show) via Quiet TokenQuantity
    deriving (Commutative, Semigroup, Monoid, MonoidNull) via Sum Natural
    deriving (LeftReductive, RightReductive, Reductive) via Sum Natural
    deriving (LeftGCDMonoid, RightGCDMonoid, GCDMonoid) via Sum Natural
    deriving (OverlappingGCDMonoid, Monus) via Sum Natural

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

zero :: TokenQuantity
zero = TokenQuantity 0

--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------

add :: TokenQuantity -> TokenQuantity -> TokenQuantity
add = (<>)

-- | Subtracts the second token quantity from the first.
--
-- Returns 'Nothing' if the first quantity is less than the second quantity.
--
subtract :: TokenQuantity -> TokenQuantity -> Maybe TokenQuantity
subtract = (</>)

-- | Finds the predecessor of a given token quantity.
--
-- Returns 'Nothing' if the given quantity is zero.
--
pred :: TokenQuantity -> Maybe TokenQuantity
pred = (`subtract` TokenQuantity 1)

-- | Finds the predecessor of a given token quantity.
--
-- Returns 'zero' if the given quantity is 'zero'.
--
-- Satisfies the following property:
--
-- >>> predZero x == x `difference` 1
--
predZero :: TokenQuantity -> TokenQuantity
predZero = fromMaybe zero . pred

-- | Finds the successor of a given token quantity.
--
succ :: TokenQuantity -> TokenQuantity
succ = (`add` TokenQuantity 1)

-- | Subtracts the second token quantity from the first.
--
-- Returns 'zero' if the first quantity is less than the second quantity.
--
difference :: TokenQuantity -> TokenQuantity -> TokenQuantity
difference = (<\>)

-- NOTE: equipartition, partition, and partitionDefault have been removed
-- because they depend on equipartitionNatural and partitionNatural from
-- Cardano.Numeric.Util, which are not included in this package.
-- These functions remain in cardano-wallet.

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

isNonZero :: TokenQuantity -> Bool
isNonZero = (/= zero)

isZero :: TokenQuantity -> Bool
isZero = (== zero)
