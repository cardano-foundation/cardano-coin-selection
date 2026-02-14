module Cardano.CoinSelection.Types.Coin.Gen
    ( chooseCoin
    , genCoin
    , genCoinPositive
    , shrinkCoin
    , shrinkCoinPositive
    ) where

import Prelude

import Cardano.CoinSelection.Gen.Extra
    ( chooseNatural
    , shrinkNatural
    )
import Cardano.CoinSelection.Types.Coin
    ( Coin (..)
    )
import Data.Coerce
    ( coerce
    )
import Test.QuickCheck
    ( Gen
    , choose
    , sized
    )

chooseCoin :: (Coin, Coin) -> Gen Coin
chooseCoin = coerce chooseNatural

genCoin :: Gen Coin
genCoin = sized $ \n ->
    Coin . fromIntegral <$> choose (0, n)

shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin c) = Coin <$> shrinkNatural c

genCoinPositive :: Gen Coin
genCoinPositive = sized $ \n ->
    Coin . fromIntegral <$> choose (1, max 1 n)

shrinkCoinPositive :: Coin -> [Coin]
shrinkCoinPositive (Coin c) =
    Coin <$> filter (> 0) (shrinkNatural c)
