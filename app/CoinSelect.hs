{-# LANGUAGE NamedFieldPuns #-}

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
import CoinSelectionDemo
    ( DemoContext
    , DemoTargetOutput (..)
    , DemoUTxO (..)
    , DemoUTxOInput (..)
    , bundleCoin
    , coinToInteger
    , runDemoSelection
    , selectionChange
    , selectionInputs
    )
import Data.Char
    ( isDigit
    )
import System.Exit
    ( exitFailure
    )
import System.IO
    ( hPutStrLn
    , stderr
    )

data InputDocument = InputDocument
    { documentUtxos :: [DemoUTxOInput]
    , documentOutputs :: [DemoTargetOutput]
    }

main :: IO ()
main = do
    input <- getContents
    case parseDocument input of
        Left err ->
            die $ "parse-error: " <> err
        Right document ->
            case selectCoins document of
                Left err ->
                    die $ "selection-error: " <> err
                Right result ->
                    putStr $ renderSelection result

selectCoins :: InputDocument -> Either String (Selection DemoContext)
selectCoins InputDocument{documentUtxos, documentOutputs} =
    runDemoSelection documentUtxos documentOutputs

parseDocument :: String -> Either String InputDocument
parseDocument =
    foldl' parseLine (Right $ InputDocument [] [])
        . zip [(1 :: Int) ..]
        . lines

parseLine
    :: Either String InputDocument
    -> (Int, String)
    -> Either String InputDocument
parseLine (Left err) _ =
    Left err
parseLine (Right document) (lineNumber, line) =
    case words line of
        [] ->
            Right document
        ["utxo", inputId, lovelaceText] -> do
            lovelace <- parseLovelace lineNumber lovelaceText
            Right
                document
                    { documentUtxos =
                        documentUtxos document <> [DemoUTxOInput inputId lovelace]
                    }
        ["output", address, lovelaceText] -> do
            lovelace <- parseLovelace lineNumber lovelaceText
            Right
                document
                    { documentOutputs =
                        documentOutputs document <> [DemoTargetOutput address lovelace]
                    }
        fields ->
            Left $
                "line "
                    <> show lineNumber
                    <> ": expected 'utxo <id> <lovelace>' or 'output <address> <lovelace>', got "
                    <> show (length fields)
                    <> " fields"

parseLovelace :: Int -> String -> Either String Integer
parseLovelace lineNumber value
    | null value || not (all isDigit value) =
        Left $
            "line "
                <> show lineNumber
                <> ": lovelace must be a non-negative base-10 integer"
    | otherwise =
        Right $ read value

renderSelection :: Selection DemoContext -> String
renderSelection selection =
    unlines $
        fmap renderInput (selectionInputs selection)
            <> fmap renderChange (selectionChange selection)

renderInput :: (DemoUTxO, TokenBundle) -> String
renderInput (utxo, bundle) =
    "selected "
        <> demoUTxOId utxo
        <> " "
        <> renderLovelace bundle

renderChange :: TokenBundle -> String
renderChange bundle =
    "change " <> renderLovelace bundle

renderLovelace :: TokenBundle -> String
renderLovelace =
    show . coinToInteger . bundleCoin

die :: String -> IO a
die message = do
    hPutStrLn stderr message
    exitFailure
