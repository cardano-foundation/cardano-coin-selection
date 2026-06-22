module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Char as Char
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CodeUnits
import Data.String.Common as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import FFI.CoinSelect as CoinSelect
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

defaultInput :: String
defaultInput =
  "utxo input-1 2000000\n\
  \utxo input-2 3000000\n\
  \output target-address 4500000\n"

data RunStatus
  = Running
  | Ready SelectionResult
  | Failed String

type SelectionResult =
  { selected :: Array SelectedInput
  , change :: String
  }

type SelectedInput =
  { inputId :: String
  , lovelace :: String
  }

type State =
  { input :: String
  , status :: RunStatus
  }

data Action
  = Initialize
  | UpdateInput String
  | Submit Event

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

component
  :: forall q i o
   . H.Component q i o Aff
component =
  H.mkComponent
    { initialState: \_ -> { input: defaultInput, status: Running }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { initialize = Just Initialize
            , handleAction = handleAction
            }
    }

render
  :: State
  -> H.ComponentHTML Action () Aff
render state =
  HH.main [ cls "app-shell" ]
    [ HH.header [ cls "app-header" ]
        [ HH.h1_ [ HH.text "Cardano Coin Selection" ]
        , renderStatus state.status
        ]
    , HH.div [ cls "workspace" ]
        [ renderInput state
        , renderOutput state.status
        ]
    ]

renderInput
  :: State
  -> H.ComponentHTML Action () Aff
renderInput state =
  HH.section [ cls "panel input-panel" ]
    [ HH.h2_ [ HH.text "Input" ]
    , HH.form
        [ cls "input-form"
        , HP.noValidate true
        , HE.onSubmit Submit
        ]
        [ HH.label
            [ cls "field-label"
            , HP.for "coin-selection-input"
            ]
            [ HH.text "Coin selection input" ]
        , HH.textarea
            [ cls "input-textarea"
            , HP.id "coin-selection-input"
            , HP.name "coin-selection-input"
            , HP.rows 8
            , HP.value state.input
            , HE.onValueInput UpdateInput
            ]
        , HH.div [ cls "action-row" ]
            [ HH.button
                [ cls "run-button"
                , HP.type_ HP.ButtonSubmit
                , HP.disabled (isRunning state.status)
                ]
                [ HH.text
                    if isRunning state.status then
                      "Running..."
                    else
                      "Run coin selection"
                ]
            ]
        ]
    ]

renderStatus
  :: RunStatus
  -> H.ComponentHTML Action () Aff
renderStatus = case _ of
  Running ->
    HH.p [ cls "status", HPA.live "polite" ] [ HH.text "Running" ]
  Ready _ ->
    HH.p [ cls "status", HPA.live "polite" ] [ HH.text "Ready" ]
  Failed _ ->
    HH.p [ cls "status error", HPA.live "polite" ] [ HH.text "Error" ]

renderOutput
  :: RunStatus
  -> H.ComponentHTML Action () Aff
renderOutput status =
  HH.section [ cls "panel result-panel" ]
    [ HH.h2_ [ HH.text "Result" ]
    , case status of
        Running ->
          HH.div [ cls "result-status" ]
            [ HH.text "Running coin selection..." ]
        Ready result ->
          renderResult result
        Failed err ->
          HH.div
            [ cls "error-box"
            , HPA.role "alert"
            ]
            [ HH.text err ]
    ]

renderResult
  :: SelectionResult
  -> H.ComponentHTML Action () Aff
renderResult result =
  HH.div [ cls "result-body" ]
    [ HH.table [ cls "result-table" ]
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ [ HH.text "Selected input" ]
                , HH.th_ [ HH.text "Lovelace" ]
                ]
            ]
        , HH.tbody_ (renderSelectedInput <$> result.selected)
        ]
    , HH.div [ cls "change-summary" ]
        [ HH.span [ cls "change-label" ] [ HH.text "Change " ]
        , HH.strong [ cls "change-value" ] [ HH.text result.change ]
        ]
    ]

renderSelectedInput
  :: SelectedInput
  -> H.ComponentHTML Action () Aff
renderSelectedInput input =
  HH.tr_
    [ HH.td_ [ HH.text input.inputId ]
    , HH.td_ [ HH.text input.lovelace ]
    ]

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize ->
    runSelection defaultInput
  UpdateInput input ->
    H.modify_ _ { input = input }
  Submit event -> do
    liftEffect $ Event.preventDefault event
    state <- H.get
    runSelection state.input

runSelection
  :: forall o
   . String
  -> H.HalogenM State Action () o Aff Unit
runSelection input =
  case validateInput input of
    Left err ->
      H.modify_ _ { status = Failed err }
    Right normalizedInput -> do
      H.modify_ _ { status = Running }
      result <- liftAff $ attempt (CoinSelect.runCoinSelect normalizedInput)
      H.modify_ _
        { status = case result of
            Left err ->
              Failed ("Wasm/FFI error: " <> message err)
            Right output ->
              case parseWasmOutput output of
                Left err -> Failed err
                Right parsed -> Ready parsed
        }

type NumberedLine =
  { number :: Int
  , text :: String
  }

type InputSummary =
  { hasUtxo :: Boolean
  , hasOutput :: Boolean
  }

validateInput
  :: String
  -> Either String String
validateInput rawInput =
  let
    lines = nonEmptyLines rawInput
  in
    if Array.null lines then
      Left "Input is empty. Add at least one utxo line and one output line."
    else
      case Array.foldl validateLine (Right { hasUtxo: false, hasOutput: false }) lines of
        Left err ->
          Left err
        Right summary ->
          if not summary.hasUtxo then
            Left "Input must include at least one utxo line."
          else if not summary.hasOutput then
            Left "Input must include at least one output line."
          else
            Right (String.joinWith "\n" (_.text <$> lines) <> "\n")

validateLine
  :: Either String InputSummary
  -> NumberedLine
  -> Either String InputSummary
validateLine (Left err) _ =
  Left err
validateLine (Right summary) line =
  case lineTokens line.text of
    [ "utxo", _, lovelace ] ->
      if isPositiveInteger lovelace then
        Right summary { hasUtxo = true }
      else
        Left (linePrefix line <> "utxo lovelace must be a positive integer.")
    [ "output", _, lovelace ] ->
      if isPositiveInteger lovelace then
        Right summary { hasOutput = true }
      else
        Left (linePrefix line <> "output lovelace must be a positive integer.")
    _ ->
      Left
        ( linePrefix line
            <> "expected `utxo <id> <lovelace>` or `output <addr> <lovelace>`."
        )

type OutputAccumulator =
  { selected :: Array SelectedInput
  , change :: Maybe String
  }

parseWasmOutput
  :: String
  -> Either String SelectionResult
parseWasmOutput stdout =
  let
    lines = nonEmptyLines stdout
  in
    if Array.null lines then
      Left "Wasm returned empty stdout."
    else
      case Array.foldl parseOutputLine (Right { selected: [], change: Nothing }) lines of
        Left err ->
          Left err
        Right output ->
          if Array.null output.selected then
            Left "Wasm output did not include selected inputs."
          else
            case output.change of
              Nothing ->
                Left "Wasm output did not include a change line."
              Just change ->
                Right { selected: output.selected, change }

parseOutputLine
  :: Either String OutputAccumulator
  -> NumberedLine
  -> Either String OutputAccumulator
parseOutputLine (Left err) _ =
  Left err
parseOutputLine (Right output) line =
  case lineTokens line.text of
    [ "selected", inputId, lovelace ] ->
      if isPositiveInteger lovelace then
        Right
          output
            { selected =
                output.selected <> [ { inputId, lovelace } ]
            }
      else
        Left (linePrefix line <> "selected lovelace must be a positive integer.")
    [ "change", lovelace ] ->
      if not (isPositiveInteger lovelace) then
        Left (linePrefix line <> "change lovelace must be a positive integer.")
      else
        case output.change of
          Just _ ->
            Left (linePrefix line <> "wasm output repeated the change line.")
          Nothing ->
            Right output { change = Just lovelace }
    _ ->
      Left (linePrefix line <> "unknown wasm output line `" <> line.text <> "`.")

nonEmptyLines
  :: String
  -> Array NumberedLine
nonEmptyLines input =
  input
    # String.split (Pattern "\n")
    # Array.mapWithIndex
        ( \index text ->
            { number: index + 1
            , text: String.trim text
            }
        )
    # Array.filter (\line -> not (String.null line.text))

lineTokens
  :: String
  -> Array String
lineTokens line =
  line
    # String.replaceAll (Pattern "\t") (Replacement " ")
    # String.split (Pattern " ")
    # Array.filter (\token -> not (String.null token))

linePrefix
  :: NumberedLine
  -> String
linePrefix line =
  "Line " <> show line.number <> ": "

isPositiveInteger
  :: String
  -> Boolean
isPositiveInteger text =
  let
    chars = CodeUnits.toCharArray text
  in
    not (Array.null chars)
      && Array.all isDigit chars
      && Array.any (_ /= '0') chars

isDigit
  :: Char
  -> Boolean
isDigit char =
  let
    code = Char.toCharCode char
  in
    code >= Char.toCharCode '0' && code <= Char.toCharCode '9'

isRunning
  :: RunStatus
  -> Boolean
isRunning = case _ of
  Running -> true
  Ready _ -> false
  Failed _ -> false

cls
  :: forall r i
   . String
  -> HH.IProp (class :: String | r) i
cls =
  HP.class_ <<< HH.ClassName
