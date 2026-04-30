{- | UPLC text Data syntax parser and printer.

The grammar is the one used inside @(con data ...)@ constants in UPLC textual
programs (see Plutus Core specification, section on built-in constants):

@
data ::= 'Constr' int '[' data,* ']'
       | 'Map'   '[' '(' data ',' data ')' ,* ']'
       | 'List'  '[' data,* ']'
       | 'I'     int
       | 'B'     '#' hexbyte*
@

Examples:

* @I 42@, @I -7@
* @B #deadbeef@, @B #@ (empty bytestring)
* @Constr 0 []@, @Constr 1 [I 42, B #cafe]@
* @List [I 1, I 2, I 3]@
* @Map [(I 1, B #ab), (I 2, B #cd)]@

Whitespace is permissive between tokens.
-}
module Cape.Data.UplcText (
  parseUplcDataText,
  renderUplcDataText,
) where

import Prelude

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import PlutusCore.Data (Data (..))
import PlutusCore.Error (ParserErrorBundle)
import PlutusCore.Parser (ExpectParens (..), conData, parseGen)
import PlutusCore.Quote (runQuote)
import Text.Printf (printf)

{- | Parse a 'Data' value from the UPLC text Data syntax. The input is the bare
data expression (no surrounding @(con data ...)@ wrapper).
-}
parseUplcDataText :: Text -> Either Text Data
parseUplcDataText input =
  case runQuote (runExceptT (parseGen (conData ExpectParensNo) input)) of
    Left (bundle :: ParserErrorBundle) -> Left (Text.pack (show bundle))
    Right d -> Right d

{- | Render a 'Data' value back into the UPLC text Data syntax.

The output is round-trippable through 'parseUplcDataText'.
-}
renderUplcDataText :: Data -> Text
renderUplcDataText = \case
  I n -> "I " <> Text.pack (show n)
  B bs -> "B #" <> hexEncode bs
  Constr ix args ->
    "Constr "
      <> Text.pack (show ix)
      <> " ["
      <> Text.intercalate ", " (map renderUplcDataText args)
      <> "]"
  List ds ->
    "List ["
      <> Text.intercalate ", " (map renderUplcDataText ds)
      <> "]"
  Map kvs ->
    "Map ["
      <> Text.intercalate
        ", "
        [ "(" <> renderUplcDataText k <> ", " <> renderUplcDataText v <> ")"
        | (k, v) <- kvs
        ]
      <> "]"

hexEncode :: BS.ByteString -> Text
hexEncode = Text.concat . map (Text.pack . printf "%02x") . BS.unpack
