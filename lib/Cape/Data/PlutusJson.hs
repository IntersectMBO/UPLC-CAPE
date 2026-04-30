{- | Plutus Data JSON schema (the @ScriptDataJsonDetailedSchema@ used by
@cardano-cli@, Aiken, Plu-ts, and other Cardano tooling).

The grammar is:

@
data ::= '{' '"int'      ':' integer '}'
       | '{' '"bytes'    ':' string  '}'                           -- hex-encoded
       | '{' '"list'     ':' '[' data,* ']' '}'
       | '{' '"map'      ':' '[' '{' '"k' ':' data ',' '"v' ':' data '}' ,* ']' '}'
       | '{' '"constructor' ':' integer ',' '"fields' ':' '[' data,* ']' '}'
@

References: cardano-cli @ScriptDataJsonDetailedSchema@ in
@Cardano.Api.SerialiseLedgerCddl@ / CIP-related conventions.
-}
module Cape.Data.PlutusJson (
  parsePlutusJsonData,
  encodePlutusJsonData,
) where

import Prelude

import Data.Aeson (Value)
import Data.Aeson qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import PlutusCore.Data (Data (..))

-- | Parse a 'Data' value from the Plutus JSON detailed schema.
parsePlutusJsonData :: Value -> Either Text Data
parsePlutusJsonData input = case AesonTypes.parseEither parseData input of
  Left err -> Left (Text.pack err)
  Right d -> Right d
  where
    parseData :: Value -> AesonTypes.Parser Data
    parseData val = case val of
      Json.Object o
        | KeyMap.member "int" o -> do
            n <- o Json..: "int"
            pure (I n)
        | KeyMap.member "bytes" o -> do
            hex :: Text <- o Json..: "bytes"
            case Base16.decode (TE.encodeUtf8 hex) of
              Left err -> fail $ "invalid hex in 'bytes': " <> err
              Right bs -> pure (B bs)
        | KeyMap.member "list" o -> do
            xs <- o Json..: "list"
            List <$> traverse parseData xs
        | KeyMap.member "map" o -> do
            entries <- o Json..: "map"
            Map <$> traverse parseEntry entries
        | KeyMap.member "constructor" o -> do
            ix <- o Json..: "constructor"
            fields <- o Json..: "fields"
            Constr ix <$> traverse parseData fields
        | otherwise ->
            fail $
              "Plutus JSON Data object must contain one of "
                <> "'int', 'bytes', 'list', 'map', 'constructor'; got keys: "
                <> show (KeyMap.keys o)
      other -> AesonTypes.typeMismatch "Plutus JSON Data object" other

    parseEntry :: Value -> AesonTypes.Parser (Data, Data)
    parseEntry = Json.withObject "Plutus JSON map entry" \o -> do
      key <- o Json..: "k" >>= parseData
      value <- o Json..: "v" >>= parseData
      pure (key, value)

-- | Encode a 'Data' value as Plutus JSON detailed schema.
encodePlutusJsonData :: Data -> Value
encodePlutusJsonData = \case
  I n -> Json.object ["int" Json..= n]
  B bs -> Json.object ["bytes" Json..= TE.decodeLatin1 (Base16.encode bs)]
  List ds -> Json.object ["list" Json..= map encodePlutusJsonData ds]
  Map kvs ->
    Json.object
      [ "map"
          Json..= [ Json.object
                      [ "k" Json..= encodePlutusJsonData k
                      , "v" Json..= encodePlutusJsonData v
                      ]
                  | (k, v) <- kvs
                  ]
      ]
  Constr ix args ->
    Json.object
      [ "constructor" Json..= ix
      , "fields" Json..= map encodePlutusJsonData args
      ]

