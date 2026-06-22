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
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

data RunStatus
  = Running
  | Ready SelectionResult
  | Failed String

data Preset
  = ManySmall
  | OneBig
  | NearExact

type UtxoRow =
  { rowId :: Int
  , inputId :: String
  , lovelace :: String
  }

type SelectionResult =
  { selected :: Array SelectedInput
  , change :: String
  }

type SelectedInput =
  { inputId :: String
  , lovelace :: String
  }

type State =
  { utxos :: Array UtxoRow
  , target :: String
  , nextRowId :: Int
  , status :: RunStatus
  }

data Action
  = Initialize
  | UpdateUtxoId Int String
  | UpdateUtxoLovelace Int String
  | UpdateTarget String
  | AddUtxo
  | RemoveUtxo Int
  | ApplyPreset Preset
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
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { initialize = Just Initialize
            , handleAction = handleAction
            }
    }

initialState
  :: forall i
   . i
  -> State
initialState _ =
  let
    rows = presetRows ManySmall
  in
    { utxos: rows
    , target: presetTarget ManySmall
    , nextRowId: Array.length rows + 1
    , status: Running
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
        , renderOutput state
        ]
    ]

renderInput
  :: State
  -> H.ComponentHTML Action () Aff
renderInput state =
  HH.section [ cls "panel input-panel" ]
    [ HH.h2_ [ HH.text "UTxO Pool" ]
    , HH.form
        [ cls "input-form"
        , HP.noValidate true
        , HE.onSubmit Submit
        ]
        [ HH.div [ cls "preset-row" ] (renderPresetButton <$> presetButtons)
        , HH.div [ cls "target-row" ]
            [ HH.label
                [ cls "field-label"
                , HP.for "target-lovelace"
                ]
                [ HH.text "Target" ]
            , HH.input
                [ cls "target-input"
                , HP.id "target-lovelace"
                , HP.name "target-lovelace"
                , HP.type_ HP.InputText
                , HP.pattern "[0-9]*"
                , HP.value state.target
                , HE.onValueInput UpdateTarget
                ]
            ]
        , HH.div [ cls "utxo-toolbar" ]
            [ HH.h3_ [ HH.text "Available UTxOs" ]
            , HH.button
                [ cls "secondary-button"
                , HP.type_ HP.ButtonButton
                , HE.onClick (const AddUtxo)
                ]
                [ HH.text "Add row" ]
            ]
        , HH.table [ cls "utxo-table" ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.text "ID" ]
                    , HH.th_ [ HH.text "Lovelace" ]
                    , HH.th_ [ HH.text "Choice" ]
                    , HH.th_ [ HH.text "" ]
                    ]
                ]
            , HH.tbody_
                (renderUtxoRow (Array.length state.utxos) state.status <$> state.utxos)
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

renderPresetButton
  :: Preset
  -> H.ComponentHTML Action () Aff
renderPresetButton preset =
  HH.button
    [ cls "preset-button"
    , HP.type_ HP.ButtonButton
    , HE.onClick (const (ApplyPreset preset))
    ]
    [ HH.text (presetName preset) ]

renderUtxoRow
  :: Int
  -> RunStatus
  -> UtxoRow
  -> H.ComponentHTML Action () Aff
renderUtxoRow rowCount status row =
  let
    selected = isSelectedInput status row.inputId
  in
    HH.tr
      [ cls (utxoRowClass selected)
      , dataAttr "utxo-id" (String.trim row.inputId)
      ]
      [ HH.td_
          [ HH.input
              [ cls "row-input id-input"
              , HP.type_ HP.InputText
              , HP.value row.inputId
              , HPA.label ("ID for " <> displayInputId row)
              , HE.onValueInput (UpdateUtxoId row.rowId)
              ]
          ]
      , HH.td_
          [ HH.input
              [ cls "row-input amount-input"
              , HP.type_ HP.InputText
              , HP.pattern "[0-9]*"
              , HP.value row.lovelace
              , HPA.label ("Lovelace for " <> displayInputId row)
              , HE.onValueInput (UpdateUtxoLovelace row.rowId)
              ]
          ]
      , HH.td [ cls "choice-cell" ]
          [ if selected then
              HH.span [ cls "selected-badge" ] [ HH.text "Selected" ]
            else
              HH.span [ cls "available-badge" ] [ HH.text "Available" ]
          ]
      , HH.td [ cls "remove-cell" ]
          [ HH.button
              [ cls "icon-button"
              , HP.type_ HP.ButtonButton
              , HP.disabled (rowCount <= 1)
              , HPA.label ("Remove " <> displayInputId row)
              , HE.onClick (const (RemoveUtxo row.rowId))
              ]
              [ HH.text "Remove" ]
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
  :: State
  -> H.ComponentHTML Action () Aff
renderOutput state =
  HH.section [ cls "panel result-panel" ]
    [ HH.h2_ [ HH.text "Result" ]
    , case state.status of
        Running ->
          HH.div [ cls "result-status" ]
            [ HH.text "Running coin selection..." ]
        Ready result ->
          renderResult state.target result
        Failed err ->
          HH.div
            [ cls "error-box"
            , HPA.role "alert"
            ]
            [ HH.text err ]
    ]

renderResult
  :: String
  -> SelectionResult
  -> H.ComponentHTML Action () Aff
renderResult target result =
  HH.div [ cls "result-body" ]
    [ HH.div [ cls "totals-row" ]
        [ renderTotal "Selected total" (selectedTotal result)
        , renderTotal "Target" target
        , renderTotal "Change" result.change
        ]
    , HH.table [ cls "result-table" ]
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ [ HH.text "Selected input" ]
                , HH.th_ [ HH.text "Lovelace" ]
                ]
            ]
        , HH.tbody_ (renderSelectedInput <$> result.selected)
        ]
    ]

renderTotal
  :: String
  -> String
  -> H.ComponentHTML Action () Aff
renderTotal label value =
  HH.div [ cls "total-item" ]
    [ HH.span [ cls "total-label" ] [ HH.text (label <> " ") ]
    , HH.strong [ cls "total-value" ] [ HH.text value ]
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
  Initialize -> do
    state <- H.get
    runSelection state
  UpdateUtxoId rowId inputId ->
    H.modify_ \state ->
      state
        { utxos =
            updateUtxo rowId (\row -> row { inputId = inputId }) state.utxos
        }
  UpdateUtxoLovelace rowId lovelace ->
    H.modify_ \state ->
      state
        { utxos =
            updateUtxo rowId (\row -> row { lovelace = lovelace }) state.utxos
        }
  UpdateTarget target ->
    H.modify_ _ { target = target }
  AddUtxo ->
    H.modify_ \state ->
      state
        { utxos =
            state.utxos
              <>
                [ { rowId: state.nextRowId
                  , inputId: "input-" <> show state.nextRowId
                  , lovelace: "1000000"
                  }
                ]
        , nextRowId = state.nextRowId + 1
        }
  RemoveUtxo rowId ->
    H.modify_ \state ->
      state
        { utxos =
            if Array.length state.utxos <= 1 then
              state.utxos
            else
              Array.filter (\row -> row.rowId /= rowId) state.utxos
        }
  ApplyPreset preset -> do
    state <- H.get
    let
      next = applyPreset preset state
    H.put next
    runSelection next
  Submit event -> do
    liftEffect $ Event.preventDefault event
    state <- H.get
    runSelection state

runSelection
  :: forall o
   . State
  -> H.HalogenM State Action () o Aff Unit
runSelection state =
  case validateInput (buildSelectionInput state) of
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

buildSelectionInput
  :: State
  -> String
buildSelectionInput state =
  String.joinWith "\n"
    (utxoLine <$> state.utxos)
    <> "\noutput target-address "
    <> String.trim state.target
    <> "\n"

utxoLine
  :: UtxoRow
  -> String
utxoLine row =
  "utxo " <> String.trim row.inputId <> " " <> String.trim row.lovelace

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
      if not (isNonNegativeInteger lovelace) then
        Left (linePrefix line <> "change lovelace must be zero or a positive integer.")
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

isNonNegativeInteger
  :: String
  -> Boolean
isNonNegativeInteger text =
  let
    chars = CodeUnits.toCharArray text
  in
    not (Array.null chars)
      && Array.all isDigit chars

isDigit
  :: Char
  -> Boolean
isDigit char =
  let
    code = Char.toCharCode char
  in
    code >= Char.toCharCode '0' && code <= Char.toCharCode '9'

selectedTotal
  :: SelectionResult
  -> String
selectedTotal result =
  Array.foldl
    (\total selected -> addDecimalStrings total selected.lovelace)
    "0"
    result.selected

addDecimalStrings
  :: String
  -> String
  -> String
addDecimalStrings left right =
  normalizeDecimal
    (go (digitsReversed left) (digitsReversed right) 0 "")
  where
  digitsReversed =
    Array.reverse <<< map digitValue <<< CodeUnits.toCharArray

  go leftDigits rightDigits carry rendered =
    case Array.uncons leftDigits, Array.uncons rightDigits of
      Nothing, Nothing ->
        if carry == 0 then
          rendered
        else
          show carry <> rendered
      leftHead, rightHead ->
        let
          leftDigit = case leftHead of
            Nothing -> 0
            Just digit -> digit.head

          rightDigit = case rightHead of
            Nothing -> 0
            Just digit -> digit.head

          digitTotal = leftDigit + rightDigit + carry
          nextCarry = digitTotal `div` 10
          renderedDigit = digitTotal `mod` 10
          nextLeft = case leftHead of
            Nothing -> []
            Just digit -> digit.tail
          nextRight = case rightHead of
            Nothing -> []
            Just digit -> digit.tail
        in
          go nextLeft nextRight nextCarry (show renderedDigit <> rendered)

digitValue
  :: Char
  -> Int
digitValue char =
  Char.toCharCode char - Char.toCharCode '0'

normalizeDecimal
  :: String
  -> String
normalizeDecimal text =
  let
    stripped =
      text
        # CodeUnits.toCharArray
        # Array.dropWhile (_ == '0')
  in
    if Array.null stripped then
      "0"
    else
      CodeUnits.fromCharArray stripped

isSelectedInput
  :: RunStatus
  -> String
  -> Boolean
isSelectedInput status inputId =
  case status of
    Ready result ->
      Array.any
        (\selected -> selected.inputId == String.trim inputId)
        result.selected
    _ ->
      false

isRunning
  :: RunStatus
  -> Boolean
isRunning = case _ of
  Running -> true
  Ready _ -> false
  Failed _ -> false

presetButtons
  :: Array Preset
presetButtons =
  [ ManySmall, OneBig, NearExact ]

presetName
  :: Preset
  -> String
presetName = case _ of
  ManySmall -> "Many small UTxOs"
  OneBig -> "One big UTxO"
  NearExact -> "Near-exact match"

presetTarget
  :: Preset
  -> String
presetTarget = case _ of
  ManySmall -> "5000000"
  OneBig -> "6000000"
  NearExact -> "4500000"

presetRows
  :: Preset
  -> Array UtxoRow
presetRows = case _ of
  ManySmall ->
    [ mkRow 1 "small-1" "1000000"
    , mkRow 2 "small-2" "1500000"
    , mkRow 3 "small-3" "2000000"
    , mkRow 4 "small-4" "2500000"
    ]
  OneBig ->
    [ mkRow 1 "big-1" "8000000"
    , mkRow 2 "dust-1" "1000000"
    ]
  NearExact ->
    [ mkRow 1 "exact-1" "4500000" ]

mkRow
  :: Int
  -> String
  -> String
  -> UtxoRow
mkRow rowId inputId lovelace =
  { rowId, inputId, lovelace }

applyPreset
  :: Preset
  -> State
  -> State
applyPreset preset state =
  let
    rows = presetRows preset
  in
    state
      { utxos = rows
      , target = presetTarget preset
      , nextRowId = Array.length rows + 1
      , status = Running
      }

updateUtxo
  :: Int
  -> (UtxoRow -> UtxoRow)
  -> Array UtxoRow
  -> Array UtxoRow
updateUtxo rowId update =
  map \row ->
    if row.rowId == rowId then
      update row
    else
      row

displayInputId
  :: UtxoRow
  -> String
displayInputId row =
  let
    inputId = String.trim row.inputId
  in
    if String.null inputId then
      "unnamed row"
    else
      inputId

utxoRowClass
  :: Boolean
  -> String
utxoRowClass selected =
  "utxo-row"
    <>
      if selected then
        " selected"
      else
        ""

dataAttr
  :: forall r i
   . String
  -> String
  -> HH.IProp r i
dataAttr name =
  HP.attr (AttrName ("data-" <> name))

cls
  :: forall r i
   . String
  -> HH.IProp (class :: String | r) i
cls =
  HP.class_ <<< HH.ClassName
