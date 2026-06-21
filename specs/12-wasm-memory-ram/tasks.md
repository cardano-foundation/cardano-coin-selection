# Tasks

## Slice 1: Conditional ByteArray Dependency

- [X] T012 Conditionalize the library bytearray dependency so
  `arch(wasm32)` selects `ram >=0.22 && <0.23` and native builds keep
  `memory >=0.15 && <0.20`.
- [X] T012 Confirm the `Data.ByteArray.ByteArrayAccess` surface is available
  without changing `lib/Cardano/CoinSelection/Types/Hash.hs`, or document any
  required minimal source change.
- [X] T012 Run `./gate.sh` and record the result.
- [X] T012 Commit with subject `build: use ram for wasm32 bytearray support`
  and trailer `Tasks: T012`.
