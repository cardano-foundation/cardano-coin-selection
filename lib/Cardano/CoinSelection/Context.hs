{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
module Cardano.CoinSelection.Context
    ( -- * Selection contexts
      SelectionContext (..)
    )
where

import Prelude

-- | Provides a shared context for types used by coin selection.
class
    ( Ord (Address c)
    , Ord (UTxO c)
    , Show (Address c)
    , Show (UTxO c)
    ) =>
    SelectionContext c
    where
    -- | A target address to which payments can be made.
    type Address c

    -- | A unique identifier for an individual UTxO.
    type UTxO c
