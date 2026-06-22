module FFI.CoinSelect
  ( runCoinSelect
  ) where

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import runCoinSelectImpl
  :: String -> Effect (Promise String)

runCoinSelect :: String -> Aff String
runCoinSelect stdinText =
  toAffE (runCoinSelectImpl stdinText)
