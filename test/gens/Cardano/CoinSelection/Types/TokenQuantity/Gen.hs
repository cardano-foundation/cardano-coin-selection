{-# LANGUAGE TypeApplications #-}

{- |
Copyright: © 2018-2026 IOHK, 2024-2026 Cardano Foundation
License: Apache-2.0
-}
module Cardano.CoinSelection.Types.TokenQuantity.Gen
    ( chooseTokenQuantity
    , genTokenQuantity
    , genTokenQuantityPositive
    , genTokenQuantityFullRange
    , shrinkTokenQuantity
    , shrinkTokenQuantityPositive
    , shrinkTokenQuantityFullRange
    ) where

import Prelude

import Cardano.CoinSelection.Gen.Extra
    ( chooseNatural
    )
import Cardano.CoinSelection.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Data.Coerce
    ( coerce
    )
import Data.Word
    ( Word64
    )
import Test.QuickCheck
    ( Gen
    , choose
    , frequency
    , shrink
    , sized
    )

chooseTokenQuantity
    :: (TokenQuantity, TokenQuantity) -> Gen TokenQuantity
chooseTokenQuantity = coerce chooseNatural

genTokenQuantity :: Gen TokenQuantity
genTokenQuantity = sized $ \n ->
    quantityFromInt <$> choose (0, n)

shrinkTokenQuantity :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantity =
    fmap quantityFromInteger . shrink . quantityToInteger

genTokenQuantityPositive :: Gen TokenQuantity
genTokenQuantityPositive = sized $ \n ->
    quantityFromInt <$> choose (1, max 1 n)

shrinkTokenQuantityPositive :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityPositive =
    fmap quantityFromInteger
        . filter (> 0)
        . shrink
        . quantityToInteger

genTokenQuantityFullRange :: Gen TokenQuantity
genTokenQuantityFullRange =
    frequency
        [ (1, pure minTQ)
        , (1, pure maxTQ)
        ,
            ( 8
            , quantityFromInteger
                <$> choose (1, quantityToInteger maxTQ - 1)
            )
        ]
  where
    minTQ = TokenQuantity 0
    maxTQ = TokenQuantity $ fromIntegral $ maxBound @Word64

shrinkTokenQuantityFullRange :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityFullRange = take 8 . shrinkTokenQuantity

quantityToInteger :: TokenQuantity -> Integer
quantityToInteger (TokenQuantity q) = fromIntegral q

quantityFromInt :: Int -> TokenQuantity
quantityFromInt i
    | i < 0 =
        error $
            "Unable to convert integer to token quantity: "
                <> show i
    | otherwise = TokenQuantity $ fromIntegral i

quantityFromInteger :: Integer -> TokenQuantity
quantityFromInteger i
    | i < 0 =
        error $
            "Unable to convert integer to token quantity: "
                <> show i
    | otherwise = TokenQuantity $ fromIntegral i
