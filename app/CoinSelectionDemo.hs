{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CoinSelectionDemo
    ( DemoAddress (..)
    , DemoContext
    , DemoTargetOutput (..)
    , DemoUTxO (..)
    , DemoUTxOInput (..)
    , bundleCoin
    , coinToInteger
    , renderCoin
    , runDemoSelection
    , selectionChange
    , selectionInputs
    ) where

import Prelude

import Cardano.CoinSelection
    ( Selection (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionParams (..)
    , performSelection
    )
import Cardano.CoinSelection.Balance
    ( SelectionStrategy (..)
    )
import Cardano.CoinSelection.Context
    ( SelectionContext (..)
    )
import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    )
import Cardano.CoinSelection.Types.Coin
    ( Coin (..)
    )
import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.CoinSelection.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.CoinSelection.UTxOIndex qualified as UTxOIndex
import Cardano.CoinSelection.UTxOSelection qualified as UTxOSelection
import Control.Monad.Random.NonRandom
    ( NonRandom (..)
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.List
    ( sortOn
    )
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map

import Cardano.CoinSelection.Types.TokenBundle qualified as TokenBundle

data DemoContext

newtype DemoAddress = DemoAddress String
    deriving (Eq, Ord)

instance Show DemoAddress where
    show (DemoAddress address) = address

newtype DemoUTxO = DemoUTxO
    { demoUTxOId :: String
    }
    deriving (Eq, Ord)

instance Show DemoUTxO where
    show (DemoUTxO utxo) = utxo

data DemoUTxOInput = DemoUTxOInput
    { demoInputId :: String
    , demoInputLovelace :: Integer
    }

data DemoTargetOutput = DemoTargetOutput
    { demoOutputAddress :: String
    , demoOutputLovelace :: Integer
    }

instance SelectionContext DemoContext where
    type Address DemoContext = DemoAddress
    type UTxO DemoContext = DemoUTxO

runDemoSelection
    :: [DemoUTxOInput]
    -> [DemoTargetOutput]
    -> Either String (Selection DemoContext)
runDemoSelection utxos outputs =
    case runNonRandom $ runExceptT $ performSelection constraints params of
        Left err -> Left $ show err
        Right selection -> Right selection
  where
    params =
        SelectionParams
            { assetsToBurn =
                mempty
            , assetsToMint =
                mempty
            , extraCoinIn =
                Coin 0
            , extraCoinOut =
                Coin 0
            , outputsToCover =
                outputToCover <$> outputs
            , collateralRequirement =
                SelectionCollateralNotRequired
            , utxoAvailableForCollateral =
                Map.empty
            , utxoAvailableForInputs =
                UTxOSelection.fromIndex $
                    UTxOIndex.fromMap $
                        Map.fromList $
                            inputToAvailableUTxO <$> utxos
            , selectionStrategy =
                SelectionStrategyMinimal
            }

constraints :: SelectionConstraints DemoContext
constraints =
    SelectionConstraints
        { tokenBundleSizeAssessor =
            TokenBundleSizeAssessor (const TokenBundleSizeWithinLimit)
        , computeMinimumAdaQuantity =
            const $ const $ Coin 0
        , isBelowMinimumAdaQuantity =
            const $ const False
        , computeMinimumCost =
            const $ Coin 0
        , maximumCollateralInputCount =
            1
        , minimumCollateralPercentage =
            0
        , maximumOutputAdaQuantity =
            Coin 1000000000
        , maximumOutputTokenQuantity =
            TokenQuantity 1000000000
        , maximumLengthChangeAddress =
            DemoAddress "change-address"
        , nullAddress =
            DemoAddress "null-address"
        }

outputToCover :: DemoTargetOutput -> (DemoAddress, TokenBundle)
outputToCover DemoTargetOutput{demoOutputAddress, demoOutputLovelace} =
    (DemoAddress demoOutputAddress, coinBundle demoOutputLovelace)

inputToAvailableUTxO :: DemoUTxOInput -> (DemoUTxO, TokenBundle)
inputToAvailableUTxO DemoUTxOInput{demoInputId, demoInputLovelace} =
    (DemoUTxO demoInputId, coinBundle demoInputLovelace)

selectionInputs :: Selection DemoContext -> [(DemoUTxO, TokenBundle)]
selectionInputs =
    sortOn (demoUTxOId . fst) . NE.toList . inputs

selectionChange :: Selection DemoContext -> [TokenBundle]
selectionChange =
    sortOn bundleCoin . change

coinBundle :: Integer -> TokenBundle
coinBundle = TokenBundle.fromCoin . Coin . fromInteger

bundleCoin :: TokenBundle -> Coin
bundleCoin = TokenBundle.getCoin

coinToInteger :: Coin -> Integer
coinToInteger (Coin coin) = toInteger coin

renderCoin :: Coin -> String
renderCoin = show . coinToInteger
