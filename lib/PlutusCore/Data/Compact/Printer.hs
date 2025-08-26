{- | Printer for compact BuiltinData text representation

This module provides functionality to convert PlutusCore.Data values
to their compact text representation, which can be parsed back using
the companion Parser module.

= Format Specification

The compact format uses the following representation:

* Integers: @42@, @-123@
* ByteStrings: @#@ (empty), @#deadbeef@ (hex-encoded)
* Constructors: @0()@ (no args), @1(42 #cafe)@ (with args)
* Lists: @[]@ (empty), @[1 2 3]@ (with elements)
* Maps: @{}@ (empty), @{1:42 #key:#value}@ (key:value pairs)

= Examples

@
dataToCompactText (I 42) == "42"
dataToCompactText (B "test") == "#74657374"
dataToCompactText (Constr 1 [I 42, B "test"]) == "1(42 #74657374)"
dataToCompactText (List [I 1, I 2, I 3]) == "[1 2 3]"
dataToCompactText (Map [(I 1, I 42)]) == "{1:42}"
@
-}
module PlutusCore.Data.Compact.Printer (
  dataToCompactText,
) where

import Prelude

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Numeric (showHex)
import PlutusCore.Data (Data (..))

{- | Convert PlutusCore.Data to compact BuiltinData text representation

This function produces a compact text format that can be parsed back
to the original Data structure using the Parser module.
-}
dataToCompactText :: Data -> Text.Text
dataToCompactText = \case
  I integer -> Text.pack (show integer)
  B bytestring ->
    "#"
      <> Text.pack (concatMap (printf "%02x" . fromIntegral) (BS.unpack bytestring))
  Constr index datas ->
    Text.pack (show index)
      <> "("
      <> Text.unwords (map dataToCompactText datas)
      <> ")"
  List datas -> "[" <> Text.unwords (map dataToCompactText datas) <> "]"
  Map pairs ->
    "{"
      <> Text.unwords
        (map (\(k, v) -> dataToCompactText k <> ":" <> dataToCompactText v) pairs)
      <> "}"
  where
    -- Helper function for formatting bytes as hex
    printf :: String -> Word8 -> String
    printf "%02x" w =
      case showHex w "" of
        [c] -> ['0', c]
        cs -> cs
    printf _ _ = ""
