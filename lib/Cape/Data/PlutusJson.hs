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
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified as List
import Data.List (sort)
import Data.Set qualified as Set
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
      Json.Object o -> case sort (KeyMap.keys o) of
        ["int"] -> I <$> o Json..: "int"
        ["bytes"] -> do
          hex :: Text <- o Json..: "bytes"
          case Base16.decode (TE.encodeUtf8 hex) of
            Left err -> fail $ "invalid hex in 'bytes': " <> err
            Right bs -> pure (B bs)
        ["list"] -> List <$> (traverse parseData =<< o Json..: "list")
        ["map"] -> Map <$> (traverse parseEntry =<< o Json..: "map")
        ["constructor", "fields"] -> do
          ix <- o Json..: "constructor"
          fields <- o Json..: "fields"
          Constr ix <$> traverse parseData fields
        keys -> fail (badKeys keys)
      other -> AesonTypes.typeMismatch "Plutus JSON Data object" other

    badKeys :: [Key.Key] -> String
    badKeys keys =
      let known = Set.fromList ["int", "bytes", "list", "map", "constructor", "fields"]
          present = Set.fromList keys
          unknown = Set.toList (Set.difference present known)
          shape = List.unwords (map (show . Key.toText) keys)
       in case unknown of
            [] ->
              "Plutus JSON Data object has invalid key combination "
                <> shape
                <> " (expected exactly one of: {int}, {bytes}, {list}, {map}, "
                <> "or {constructor, fields})"
            extras ->
              "Plutus JSON Data object has unknown key(s): "
                <> List.unwords (map (show . Key.toText) extras)

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

