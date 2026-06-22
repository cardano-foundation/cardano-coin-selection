module Main
  ( main
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Exception (message)
import FFI.CoinSelect as CoinSelect
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

defaultInput :: String
defaultInput =
  "utxo input-1 2000000\n\
  \utxo input-2 3000000\n\
  \output target-address 4500000\n"

data RunStatus
  = Running
  | Succeeded String
  | Failed String

type State =
  { status :: RunStatus
  }

data Action = Initialize

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
    { initialState: \_ -> { status: Running }
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
        [ HH.section [ cls "panel" ]
            [ HH.h2_ [ HH.text "Input" ]
            , HH.pre_ [ HH.text defaultInput ]
            ]
        , HH.section [ cls "panel" ]
            [ HH.h2_ [ HH.text "Output" ]
            , renderOutput state.status
            ]
        ]
    ]

renderStatus
  :: RunStatus
  -> H.ComponentHTML Action () Aff
renderStatus = case _ of
  Running ->
    HH.p [ cls "status" ] [ HH.text "Running" ]
  Succeeded _ ->
    HH.p [ cls "status" ] [ HH.text "Ready" ]
  Failed _ ->
    HH.p [ cls "status error" ] [ HH.text "Error" ]

renderOutput
  :: RunStatus
  -> H.ComponentHTML Action () Aff
renderOutput = case _ of
  Running ->
    HH.pre_ [ HH.text "Running coin selection..." ]
  Succeeded output ->
    HH.pre_ [ HH.text output ]
  Failed err ->
    HH.pre_ [ HH.text err ]

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    result <- liftAff $ attempt (CoinSelect.runCoinSelect defaultInput)
    H.modify_ _
      { status = case result of
          Right output -> Succeeded output
          Left err -> Failed (message err)
      }

cls
  :: forall r i
   . String
  -> HH.IProp (class :: String | r) i
cls =
  HP.class_ <<< HH.ClassName
