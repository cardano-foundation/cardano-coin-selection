{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Main
    ( main
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
import System.Exit
    ( exitFailure
    )

import Cardano.CoinSelection.Types.TokenBundle qualified as TokenBundle

data SmokeContext

newtype SmokeAddress = SmokeAddress String
    deriving (Eq, Ord)

instance Show SmokeAddress where
    show (SmokeAddress address) = address

newtype SmokeUTxO = SmokeUTxO String
    deriving (Eq, Ord)

instance Show SmokeUTxO where
    show (SmokeUTxO utxo) = utxo

instance SelectionContext SmokeContext where
    type Address SmokeContext = SmokeAddress
    type UTxO SmokeContext = SmokeUTxO

main :: IO ()
main = case runSmokeSelection of
    Left err -> do
        putStrLn $ "selection-error: " <> err
        exitFailure
    Right selection ->
        putStr $ renderSelection selection

runSmokeSelection :: Either String (Selection SmokeContext)
runSmokeSelection =
    case runNonRandom $ runExceptT $ performSelection constraints params of
        Left err -> Left $ show err
        Right selection -> Right selection

constraints :: SelectionConstraints SmokeContext
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
            SmokeAddress "change-address"
        , nullAddress =
            SmokeAddress "null-address"
        }

params :: SelectionParams SmokeContext
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
            [(SmokeAddress "target-address", coinBundle 4500000)]
        , collateralRequirement =
            SelectionCollateralNotRequired
        , utxoAvailableForCollateral =
            Map.empty
        , utxoAvailableForInputs =
            UTxOSelection.fromIndex $
                UTxOIndex.fromMap $
                    Map.fromList
                        [ (SmokeUTxO "input-1", coinBundle 2000000)
                        , (SmokeUTxO "input-2", coinBundle 3000000)
                        , (SmokeUTxO "input-3", coinBundle 5000000)
                        ]
        , selectionStrategy =
            SelectionStrategyMinimal
        }

coinBundle :: Integer -> TokenBundle
coinBundle = TokenBundle.fromCoin . Coin . fromInteger

renderSelection :: Selection SmokeContext -> String
renderSelection selection =
    unlines $
        ["selected-inputs:"]
            <> fmap renderInput selectedInputs
            <> ["change-outputs:"]
            <> fmap renderChange changeOutputs
  where
    selectedInputs =
        sortOn fst $ NE.toList $ inputs selection
    changeOutputs =
        zip [(1 :: Int) ..] $ sortOn bundleCoin $ change selection

renderInput :: (SmokeUTxO, TokenBundle) -> String
renderInput (utxo, bundle) =
    "  " <> show utxo <> " lovelace=" <> renderCoin (bundleCoin bundle)

renderChange :: (Int, TokenBundle) -> String
renderChange (index, bundle) =
    "  change-"
        <> show index
        <> " lovelace="
        <> renderCoin (bundleCoin bundle)

bundleCoin :: TokenBundle -> Coin
bundleCoin = TokenBundle.getCoin

renderCoin :: Coin -> String
renderCoin (Coin coin) = show coin
