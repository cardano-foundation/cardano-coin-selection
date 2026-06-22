module Main
    ( main
    ) where

import Prelude

import Cardano.CoinSelection
    ( Selection
    )
import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle
    )
import System.Exit
    ( exitFailure
    )

import CoinSelectionDemo
    ( DemoContext
    , DemoTargetOutput (..)
    , DemoUTxO
    , DemoUTxOInput (..)
    , bundleCoin
    , renderCoin
    , runDemoSelection
    , selectionChange
    , selectionInputs
    )

main :: IO ()
main = case runDemoSelection smokeUtxos smokeOutputs of
    Left err -> do
        putStrLn $ "selection-error: " <> err
        exitFailure
    Right selection ->
        putStr $ renderSelection selection

smokeUtxos :: [DemoUTxOInput]
smokeUtxos =
    [ DemoUTxOInput "input-1" 2000000
    , DemoUTxOInput "input-2" 3000000
    , DemoUTxOInput "input-3" 5000000
    ]

smokeOutputs :: [DemoTargetOutput]
smokeOutputs =
    [DemoTargetOutput "target-address" 4500000]

renderSelection :: Selection DemoContext -> String
renderSelection selection =
    unlines $
        ["selected-inputs:"]
            <> fmap renderInput selectedInputs
            <> ["change-outputs:"]
            <> fmap renderChange changeOutputs
  where
    selectedInputs =
        selectionInputs selection
    changeOutputs =
        zip [(1 :: Int) ..] $ selectionChange selection

renderInput :: (DemoUTxO, TokenBundle) -> String
renderInput (utxo, bundle) =
    "  " <> show utxo <> " lovelace=" <> renderCoin (bundleCoin bundle)

renderChange :: (Int, TokenBundle) -> String
renderChange (index, bundle) =
    "  change-"
        <> show index
        <> " lovelace="
        <> renderCoin (bundleCoin bundle)
