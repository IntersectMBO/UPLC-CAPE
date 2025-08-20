{-# LANGUAGE OverloadedStrings #-}

-- | Parser for compact BuiltinData text representation
--
-- = Grammar (BNF)
--
-- @
-- data         ::= integer | bytestring | constructor | list | map
-- integer      ::= ('-')? digit+
-- bytestring   ::= '#' hexdigit*
-- constructor  ::= integer '(' data* ')'
-- list         ::= '[' data* ']'
-- map          ::= '{' pair* '}'
-- pair         ::= data ':' data
-- digit        ::= '0'..'9'
-- hexdigit     ::= '0'..'9' | 'a'..'f' | 'A'..'F'
-- whitespace   ::= (' ' | '\t' | '\n' | '\r')*
-- @
--
-- Whitespace is ignored between tokens. Examples:
--
-- * Integers: @42@, @-123@
-- * ByteStrings: @#@, @#deadbeef@, @#A1B2C3@
-- * Constructors: @0()@, @1(42 #cafe)@, @0(1(2()))@
-- * Lists: @[]@, @[1 2 3]@, @[42 #beef 0()]@
-- * Maps: @{}@, @{1:42}@, @{#key:0() 42:#value}@
module App.BuiltinDataParser (
  parseBuiltinDataText,
  parseData,
  ParseError (..),
  renderParseError,
) where

import Prelude hiding (many)

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isHexDigit)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Void (Void)
import Numeric (readHex)
import PlutusCore.Data (Data (..))
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

newtype ParseError = ParseError (ParseErrorBundle Text Void)
  deriving (Show, Eq)

-- | Parse BuiltinData from compact text format
parseBuiltinDataText :: Text -> Either ParseError Data
parseBuiltinDataText input = case runParser (sc *> parseData <* eof) "" input of
  Left err -> Left (ParseError err)
  Right result -> Right result

-- | Render parse error for display
renderParseError :: ParseError -> Text
renderParseError (ParseError err) = Text.pack (errorBundlePretty err)

-- | Space consumer that ignores whitespace, newlines, and tabs
sc :: Parser ()
sc = L.space space1 empty empty

-- | Lexeme wrapper that consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser that consumes trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Main data parser
parseData :: Parser Data
parseData =
  choice
    [ parseByteString
    , try parseConstructor
    , parseList
    , parseMap
    , parseInteger
    ]

-- | Parse integer (positive or negative)
parseInteger :: Parser Data
parseInteger = I <$> lexeme (L.signed sc L.decimal)

-- | Parse hex-encoded bytestring with # prefix
parseByteString :: Parser Data
parseByteString = do
  void $ char '#'
  hexText <- takeWhileP (Just "hex digit") isHexDigit
  let hexString = Text.unpack hexText
  case decodeHex hexString of
    Left err -> fail err
    Right bs -> lexeme $ pure (B bs)
  where
    decodeHex :: String -> Either String ByteString
    decodeHex "" = Right BS.empty
    decodeHex hex
      | odd (length hex) = Left "Hex string must have even number of digits"
      | otherwise = case sequence $ parseHexPair <$> pairs hex of
          Just bytes -> Right (BS.pack bytes)
          Nothing -> Left "Invalid hex digits"
      where
        pairs [] = []
        pairs [_] = error "Impossible: checked for odd length"
        pairs (a : b : rest) = (a, b) : pairs rest

        parseHexPair (c1, c2) = do
          let hex = [c1, c2]
          case readHex hex of
            [(n, "")] -> Just (fromIntegral n)
            _ -> Nothing

-- | Parse constructor: tag(args...)
parseConstructor :: Parser Data
parseConstructor = do
  tag <- L.decimal
  sc
  void $ char '('
  sc
  args <- many parseData
  void $ symbol ")"
  pure $ Constr tag args

-- | Parse list: [items...]
parseList :: Parser Data
parseList = do
  void $ symbol "["
  items <- many parseData
  void $ symbol "]"
  pure $ List items

-- | Parse map: {key:value...}
parseMap :: Parser Data
parseMap = do
  void $ symbol "{"
  pairs <- many parsePair
  void $ symbol "}"
  pure $ Map pairs
  where
    parsePair :: Parser (Data, Data)
    parsePair = do
      key <- parseData
      void $ symbol ":"
      value <- parseData
      pure (key, value)
