{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

{- |
Copyright: © 2018-2020 IOHK
License: Apache-2.0

Types and functions relating to hash values.
-}
module Cardano.CoinSelection.Types.Hash
    ( Hash (..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..)
    )
import Data.ByteArray
    ( ByteArrayAccess
    )
import Data.ByteString
    ( ByteString
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
import GHC.TypeLits
    ( Symbol
    )
import Quiet
    ( Quiet (..)
    )

{- | A general-purpose type for representing hash values.

The type parameter @tag@ is a phantom type that can be used to
distinguish between different kinds of hash.
-}
newtype Hash (tag :: Symbol) = Hash {getHash :: ByteString}
    deriving stock (Data, Generic, Eq, Ord)
    deriving newtype (ByteArrayAccess)
    deriving (Read, Show) via (Quiet (Hash tag))
    deriving anyclass (NFData, Hashable)
