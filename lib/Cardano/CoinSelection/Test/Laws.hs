{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: (c) 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides testing functions to check that type class
-- instances obey laws.
module Cardano.CoinSelection.Test.Laws
    ( testLaws
    , testLawsMany
    ) where

import Prelude

import Control.Monad
    ( forM_
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , parallel
    )
import Test.QuickCheck.Classes
    ( Laws (..)
    )

testLaws
    :: forall a
     . Typeable a
    => (Proxy a -> Laws)
    -> Spec
testLaws getLaws =
    parallel
        $ describe description
        $ forM_ (lawsProperties laws)
        $ uncurry it
  where
    description =
        mconcat
            [ "Testing "
            , lawsTypeclass laws
            , " laws for type "
            , show (typeRep $ Proxy @a)
            ]
    laws = getLaws $ Proxy @a

testLawsMany
    :: forall a
     . Typeable a
    => [Proxy a -> Laws]
    -> Spec
testLawsMany getLawsMany =
    testLaws @a `mapM_` getLawsMany
