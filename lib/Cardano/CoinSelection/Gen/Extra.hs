{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0

QuickCheck utilities extracted from @cardano-wallet-test-utils@.
-}
module Cardano.CoinSelection.Gen.Extra
    ( -- * Generation
      chooseNatural
    , genFunction
    , genMapWith
    , genSized2
    , genSized2With

      -- * Shrinking
    , genericRoundRobinShrink
    , shrinkInterleaved
    , shrinkMapWith
    , shrinkNatural
    , (<@>)
    , (<:>)

      -- * Counterexamples
    , report
    , verify

      -- * Pretty-printing
    , Pretty (..)
    ) where

import Prelude

import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( mapMaybe
    )
import Generics.SOP
    ( I (..)
    , NP (..)
    , NS (..)
    , SOP (..)
    , apFn
    , fn
    , unI
    , type (-.->)
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable
    , chooseInteger
    , counterexample
    , liftArbitrary2
    , liftShrink2
    , listOf
    , property
    , scale
    , shrinkIntegral
    , shrinkList
    , shrinkMapBy
    , suchThatMap
    , (.&&.)
    )
import Test.QuickCheck.Gen.Unsafe
    ( promote
    )
import Text.Pretty.Simple
    ( pShow
    )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import qualified GHC.Generics as GHC
import qualified Generics.SOP.GGP as GGP

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (lo, hi) =
    chooseInteger (intCast lo, intCast hi)
        `suchThatMap` intCastMaybe @Integer @Natural

{- | Generates a function.

This is based on the implementation of 'Arbitrary' for @a -> b@.
-}
genFunction
    :: (a -> Gen b -> Gen b) -> Gen b -> Gen (a -> b)
genFunction coarbitraryFn gen =
    promote (`coarbitraryFn` gen)

{- | Generates a 'Map' with the given key and value generation
functions.
-}
genMapWith :: (Ord k) => Gen k -> Gen v -> Gen (Map k v)
genMapWith genKey genValue =
    Map.fromList
        <$> listOf (liftArbitrary2 genKey genValue)

genSized2 :: Gen a -> Gen b -> Gen (a, b)
genSized2 genA genB =
    (,)
        <$> scaleToRoot 2 genA
        <*> scaleToRoot 2 genB

genSized2With
    :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
genSized2With f genA genB =
    uncurry f <$> genSized2 genA genB

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

shrinkInterleaved
    :: (a, a -> [a]) -> (b, b -> [b]) -> [(a, b)]
shrinkInterleaved (a, shrinkA) (b, shrinkB) =
    interleave
        [(a', b) | a' <- shrinkA a]
        [(a, b') | b' <- shrinkB b]
  where
    interleave (x : xs) (y : ys) =
        x : y : interleave xs ys
    interleave xs [] = xs
    interleave [] ys = ys

shrinkNatural :: Natural -> [Natural]
shrinkNatural n =
    mapMaybe (intCastMaybe @Integer @Natural) $
        shrinkIntegral $
            intCast n

{- | Shrinks a 'Map' with the given key and value shrinking
functions.
-}
shrinkMapWith
    :: (Ord k)
    => (k -> [k])
    -> (v -> [v])
    -> Map k v
    -> [Map k v]
shrinkMapWith shrinkKey shrinkValue =
    shrinkMapBy Map.fromList Map.toList $
        shrinkList $
            liftShrink2 shrinkKey shrinkValue

--------------------------------------------------------------------------------
-- Generic round-robin shrinking
--------------------------------------------------------------------------------

interleaveRoundRobin :: [[a]] -> [a]
interleaveRoundRobin = concat . L.transpose

liftShrinker :: (a -> [a]) -> (I -.-> []) a
liftShrinker shrinker = fn (shrinker . unI)

groundRobinShrinkP
    :: NP (I -.-> []) xs -> NP I xs -> [NP I xs]
groundRobinShrinkP fns =
    interleaveRoundRobin . groundRobinShrinkP' fns
  where
    groundRobinShrinkP'
        :: NP (I -.-> []) xs
        -> NP I xs
        -> [[NP I xs]]
    groundRobinShrinkP' Nil Nil = []
    groundRobinShrinkP' (s :* ss) (x1 :* xs) =
        [[I x1' :* xs | x1' <- apFn s x1]]
            <> ( fmap (x1 :*)
                    <$> groundRobinShrinkP' ss xs
               )

groundRobinShrinkS
    :: NP (I -.-> []) xs
    -> SOP I (xs ': '[])
    -> [SOP I (xs ': '[])]
groundRobinShrinkS fs (SOP (Z xs)) =
    (SOP . Z) <$> groundRobinShrinkP fs xs
groundRobinShrinkS _ (SOP (S _)) =
    error "only defined for product types."

{- | Given a list of shrinkers for each element of a product
type, and a value of that product type, shrink the value
using a round-robin algorithm.

Uses @GHC.Generics@ so the user need not derive
@Generics.SOP.Generic@.
-}
genericRoundRobinShrink
    :: ( GHC.Generic a
       , GGP.GFrom a
       , GGP.GTo a
       , GGP.GCode a ~ '[xs]
       )
    => NP (I -.-> []) xs
    -> a
    -> [a]
genericRoundRobinShrink f x =
    GGP.gto <$> groundRobinShrinkS f (GGP.gfrom x)

{- | Apply a shrinking function list to
@genericRoundRobinShrink@.
-}
(<@>) :: (a -> b) -> a -> b
a <@> b = a b

infixl 6 <@>

-- | Cons a shrinking function onto a product of shrinkers.
(<:>)
    :: (x -> [x])
    -> NP (I -.-> []) xs
    -> NP (I -.-> []) (x : xs)
a <:> b = liftShrinker a :* b

infixr 7 <:>

--------------------------------------------------------------------------------
-- Counterexamples
--------------------------------------------------------------------------------

{- | Adds a named variable to the counterexample output of a
property.
-}
report
    :: (Show a, Testable prop)
    => a
    -> String
    -> prop
    -> Property
report a name =
    counterexample $
        name <> ":\n" <> prettyShow a

{- | Adds a named condition to a property.

On failure, reports the name of the condition that failed.
-}
verify
    :: (Testable t) => Bool -> String -> t -> Property
verify condition conditionTitle =
    (.&&.)
        ( counterexample counterexampleText $
            property condition
        )
  where
    counterexampleText =
        "Condition violated: " <> conditionTitle

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

{- | A combinator that causes the output of @show@ to be
pretty-printed.
-}
newtype Pretty a = Pretty {unPretty :: a}
    deriving (Eq)

instance (Show a) => Show (Pretty a) where
    show (Pretty a) =
        TL.unpack ("\n" <> pShow a <> "\n")

instance (Arbitrary a) => Arbitrary (Pretty a) where
    arbitrary = Pretty <$> arbitrary
    shrink (Pretty a) = Pretty <$> shrink a

prettyShow :: (Show a) => a -> String
prettyShow = TL.unpack . pShow

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

scaleToRoot :: Int -> Gen a -> Gen a
scaleToRoot n =
    scale $
        floor @Double @Int
            . (** (1.0 / fromIntegral @Int @Double n))
            . fromIntegral @Int @Double
