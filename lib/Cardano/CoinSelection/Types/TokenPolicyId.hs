{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Token policy identifiers.
--
module Cardano.CoinSelection.Types.TokenPolicyId
    ( TokenPolicyId (..)
    ) where

import Prelude

import Cardano.CoinSelection.Types.Hash
    ( Hash (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Data
    ( Data
    )
import Data.Hashable
    ( Hashable
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )

-- | Token policy identifiers, represented by the hash of the monetary policy
-- script.
newtype TokenPolicyId =
    -- | Construct a 'TokenPolicyId' without any validation.
    UnsafeTokenPolicyId { unTokenPolicyId :: Hash "TokenPolicy" }
    deriving stock (Data, Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenPolicyId)
    deriving anyclass Hashable

instance NFData TokenPolicyId
