{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
module Cardano.CoinSelection.UTxOIndex
    ( -- * Type
      UTxOIndex

      -- * Construction
    , empty
    , singleton
    , fromMap
    , fromSequence

      -- * Deconstruction
    , toList
    , toMap

      -- * Folding
    , fold

      -- * Modification
    , insert
    , insertMany
    , delete
    , deleteMany

      -- * Filtering and partitioning
    , filter
    , partition

      -- * Queries
    , assets
    , balance
    , lookup
    , member
    , null
    , size

      -- * Set operations
    , difference
    , disjoint

      -- * Selection
    , Asset (..)
    , SelectionFilter (..)
    , selectRandom
    , selectRandomWithPriority
    ) where

import Cardano.CoinSelection.UTxOIndex.Internal
