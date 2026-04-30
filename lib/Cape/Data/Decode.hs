{- | Single dispatch entry-point for decoding @builtin_data@ values from the
JSON test format.

The format of the @value@ field is determined by its JSON type:

* string → UPLC text Data syntax (parsed by "Cape.Data.UplcText")
* object → Plutus JSON detailed schema (parsed by "Cape.Data.PlutusJson")

Reference shorthand (@\"\@name\"@) is _not_ handled here — that resolution
happens upstream in "Cape.Tests" before the value reaches this module.
-}
module Cape.Data.Decode (
  decodeBuiltinData,
) where

import Prelude

import Cape.Data.PlutusJson (parsePlutusJsonData)
import Cape.Data.UplcText (parseUplcDataText)
import Data.Aeson (Value)
import Data.Aeson qualified as Json
import PlutusCore.Data (Data)

decodeBuiltinData :: Value -> Either Text Data
decodeBuiltinData v = case v of
  Json.String t -> parseUplcDataText t
  Json.Object _ -> parsePlutusJsonData v
  Json.Array _ -> wrongShape "array"
  Json.Number _ -> wrongShape "number"
  Json.Bool _ -> wrongShape "boolean"
  Json.Null -> wrongShape "null"
  where
    wrongShape kind =
      Left $
        "builtin_data must be a string (UPLC text Data syntax) "
          <> "or object (Plutus JSON detailed schema), got "
          <> kind
