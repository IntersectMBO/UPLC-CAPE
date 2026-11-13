module Cape.TestsSpec (spec) where

import Prelude

import Cape.Metrics (AggregationPolicy (..))
import Cape.Tests
import Data.Aeson qualified as Json
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import PlutusCore.Data (Data (B, I, Map))
import PlutusLedgerApi.Data.V3 qualified as V3
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Data.List qualified as DataList
import System.Exit (ExitCode (..))
import Test.Hspec

-- | Helper: a fully populated TestSuite with optional shared data structures.
mkSuite :: Maybe (Map Text DataStructureEntry) -> TestSuite
mkSuite ds =
  TestSuite
    { tsVersion = "3.0.0"
    , tsDescription = Just "test"
    , tsMeasurements = []
    , tsChecks = []
    , tsDataStructures = ds
    }

-- | Helper: a TestInput for the given input type with no value/file/context.
emptyInput :: InputType -> TestInput
emptyInput t =
  TestInput
    { tiType = t
    , tiValue = Nothing
    , tiFile = Nothing
    , tiScriptContext = Nothing
    }

-- | Helper: the Data encoding of the Value built from a ValueSpec.
builtValueData :: ValueSpec -> IO Data
builtValueData = fmap (Builtins.builtinDataToData . V3.toBuiltinData) . buildValue mempty

-- | Helper: a public key address without a staking credential.
pubkeyAddress :: Text -> AddressSpec
pubkeyAddress hash = AddressSpec (PubkeyCredentialSpec hash) Nothing

-- | Helper: the Data encoding of the mint field built from its assets.
builtMintData :: [AssetSpec] -> IO Data
builtMintData =
  fmap (Builtins.builtinDataToData . V3.toBuiltinData) . buildMintValue mempty

{- | Helper: the ScriptContext that the spending baseline gives after the
patches, which are a JSON array in the cape-tests.json format.
-}
patchedContext ::
  Map Text DataStructureEntry -> LByteString -> IO V3.ScriptContext
patchedContext dataStructures patchesJson = do
  patches <- case Json.eitherDecode patchesJson of
    Left err -> error ("Failed to parse patches: " <> toText err)
    Right parsed -> pure parsed
  let testInput =
        (emptyInput ScriptContext)
          { tiScriptContext =
              Just (ScriptContextSpec (DirectBaseline SpendingBaseline) patches)
          }
  resolveTestInput "" (mkSuite (Just dataStructures)) testInput >>= \case
    ResolvedBuiltinData contextData -> pure (V3.unsafeFromBuiltinData contextData)
    ResolvedUplc t -> error ("expected ResolvedBuiltinData, got ResolvedUplc " <> t)

-- | Helper: the inputs of a context, in their order in the transaction.
contextInputs :: V3.ScriptContext -> [V3.TxInInfo]
contextInputs = DataList.toSOP . V3.txInfoInputs . V3.scriptContextTxInfo

-- | Helper: the outputs of a context, in their order in the transaction.
contextOutputs :: V3.ScriptContext -> [V3.TxOut]
contextOutputs = DataList.toSOP . V3.txInfoOutputs . V3.scriptContextTxInfo

-- | Helper: shared data with a 28-byte script hash and a 28-byte key hash.
hashes :: Map Text DataStructureEntry
hashes =
  Map.fromList
    [
      ( "escrow_script_hash"
      , BuiltinDataEntry
          (Json.String "B #5c1f4e9a02b7d3681fa40c5e9b2d7a6083f1c4e5d6a7b8c9d0e1f203")
      )
    ,
      ( "stake_key"
      , BuiltinDataEntry
          (Json.String "B #e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1")
      )
    , ("token_name", BuiltinDataEntry (Json.String "B #657363726f77"))
    , ("not_bytes", BuiltinDataEntry (Json.String "I 42"))
    ]

escrowScriptHash :: V3.ScriptHash
escrowScriptHash =
  V3.ScriptHash
    ( Builtins.toBuiltin @ByteString
        "\x5c\x1f\x4e\x9a\x02\xb7\xd3\x68\x1f\xa4\x0c\x5e\x9b\x2d\x7a\x60\x83\xf1\xc4\xe5\xd6\xa7\xb8\xc9\xd0\xe1\xf2\x03"
    )

stakeKey :: V3.PubKeyHash
stakeKey =
  V3.PubKeyHash
    (Builtins.toBuiltin @ByteString (fromString (replicate 28 '\xe1')))

shouldResolveBuiltinData :: ResolvedInput -> Expectation
shouldResolveBuiltinData = \case
  ResolvedBuiltinData _ -> pass
  ResolvedUplc t ->
    expectationFailure $ "expected ResolvedBuiltinData, got ResolvedUplc " <> show t

spec :: Spec
spec = do
  describe "resolveTestInput" do
    context "script_context input type" do
      it "resolves simple ScriptContext with AddSignature patch" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [AddSignatureSpec "deadbeef"]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with SetRedeemer patch (UPLC text Data)" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [SetRedeemerSpec (Json.String "I 42")]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with SetRedeemer patch (Plutus JSON object)" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches =
                            [ SetRedeemerSpec
                                (Json.object ["int" Json..= (42 :: Integer)])
                            ]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with multiple patches" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches =
                            [ AddSignatureSpec "cafe0001"
                            , AddSignatureSpec "cafe0002"
                            , SetRedeemerSpec (Json.String "List [I 1, I 2, I 3]")
                            , SetValidRangeSpec (Just 1000) (Just 2000)
                            ]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with data_structures references" do
        let dataStructures =
              Map.singleton
                "test_signature"
                (BuiltinDataEntry (Json.String "B #cafe1234567890abcdef"))
            testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [AddSignatureSpec "@test_signature"]
                        }
                }
        result <- resolveTestInput "" (mkSuite (Just dataStructures)) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with data_structures references (Plutus JSON form)" do
        let dataStructures =
              Map.singleton
                "test_signature"
                ( BuiltinDataEntry
                    (Json.object ["bytes" Json..= ("cafe1234567890abcdef" :: Text)])
                )
            testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [AddSignatureSpec "@test_signature"]
                        }
                }
        result <- resolveTestInput "" (mkSuite (Just dataStructures)) testInput
        shouldResolveBuiltinData result

      it "resolves minimal ScriptContext (no patches)" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = []
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

    context "builtin_data input type" do
      it "decodes UPLC text Data syntax string" do
        let testInput =
              (emptyInput BuiltinData)
                { tiValue = Just (Json.String "Constr 0 [I 42]")
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "decodes Plutus JSON detailed schema object" do
        let testInput =
              (emptyInput BuiltinData)
                { tiValue =
                    Just
                      ( Json.object
                          [ "constructor" Json..= (0 :: Integer)
                          , "fields" Json..= [Json.object ["int" Json..= (42 :: Integer)]]
                          ]
                      )
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves @reference into BuiltinData" do
        let dataStructures =
              Map.singleton
                "answer"
                (BuiltinDataEntry (Json.String "I 42"))
            testInput =
              (emptyInput BuiltinData)
                { tiValue = Just (Json.String "@answer")
                }
        result <- resolveTestInput "" (mkSuite (Just dataStructures)) testInput
        shouldResolveBuiltinData result

    context "uplc input type" do
      -- UPLC inputs are *terms*: applyInputsToProgram wraps them as
      -- "(program 1.1.0 <term>)" before parsing, so the value here must not
      -- itself include a (program ...) header.
      it "passes through inline UPLC term" do
        let uplcTerm = "(con integer 42)"
            testInput =
              (emptyInput UPLC)
                { tiValue = Just (Json.String uplcTerm)
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        case result of
          ResolvedUplc t -> t `shouldBe` uplcTerm
          ResolvedBuiltinData _ ->
            expectationFailure "expected ResolvedUplc"

  describe "PatchOperationSpec JSON completeness" do
    it "has JSON support for all constructors (exhaustive)" do
      let testJSONRoundTrip jsonValue expectedSpec = do
            case Json.eitherDecode jsonValue of
              Left err -> expectationFailure $ "Failed to parse JSON: " <> err
              Right parsedSpec -> parsedSpec `shouldBe` expectedSpec

          testAllConstructors patchSpec = case patchSpec of
            AddSignatureSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"add_signature\", \"pubkey_hash\": \"deadbeef\"}"
                (AddSignatureSpec "deadbeef")
            RemoveSignatureSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"remove_signature\", \"pubkey_hash\": \"cafe0001\"}"
                (RemoveSignatureSpec "cafe0001")
            SetRedeemerSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"set_redeemer\", \"redeemer\": \"I 42\"}"
                (SetRedeemerSpec (Json.String "I 42"))
            AddInputUTXOSpec {} ->
              testJSONRoundTrip
                [__i|{
                  "op": "add_input_utxo",
                  "utxo_ref": "txid:0",
                  "value": {
                    "lovelace": 1000000
                  },
                  "is_own_input": true
                }|]
                (AddInputUTXOSpec "txid:0" Nothing (ValueSpec 1000000 []) True Nothing)
            SetValidRangeSpec _ _ ->
              testJSONRoundTrip
                "{\"op\": \"set_valid_range\", \"from_time\": 1000, \"to_time\": 2000}"
                (SetValidRangeSpec (Just 1000) (Just 2000))
            AddOutputUTXOSpec {} ->
              testJSONRoundTrip
                [__i|{
                  "op": "add_output_utxo",
                  "address": {
                    "type": "pubkey",
                    "pubkey_hash": "deadbeef"
                  },
                  "value": {
                    "lovelace": 500000
                  }
                }|]
                (AddOutputUTXOSpec (pubkeyAddress "deadbeef") (ValueSpec 500000 []))
            RemoveOutputUTXOSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"remove_output_utxo\", \"index\": 0}"
                (RemoveOutputUTXOSpec 0)
            SetScriptDatumSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"set_script_datum\", \"datum\": \"I 42\"}"
                (SetScriptDatumSpec (Json.String "I 42"))
            AddOutputUTXOWithDatumSpec {} ->
              testJSONRoundTrip
                [__i|{
                  "op": "add_output_utxo",
                  "address": {
                    "type": "pubkey",
                    "pubkey_hash": "deadbeef"
                  },
                  "value": {
                    "lovelace": 500000
                  },
                  "datum": "I 42"
                }|]
                ( AddOutputUTXOWithDatumSpec
                    (pubkeyAddress "deadbeef")
                    (ValueSpec 500000 [])
                    (Json.String "I 42")
                )
            SetScriptInfoSpec _ ->
              testJSONRoundTrip
                [__i|{
                  "op": "set_script_info",
                  "script_info": {
                    "type": "minting",
                    "policy_id": "@policy"
                  }
                }|]
                (SetScriptInfoSpec (MintingScriptInfoSpec "@policy"))
            SetMintSpec _ ->
              testJSONRoundTrip
                [__i|{
                  "op": "set_mint",
                  "assets": [{
                    "currency_symbol": "@policy",
                    "token_name": "@token",
                    "quantity": -1
                  }]
                }|]
                (SetMintSpec [AssetSpec "@policy" "@token" (-1)])

      mapM_
        testAllConstructors
        [ AddSignatureSpec "test"
        , RemoveSignatureSpec "test"
        , SetRedeemerSpec (Json.String "I 42")
        , AddInputUTXOSpec "test:0" Nothing (ValueSpec 1000000 []) True Nothing
        , SetValidRangeSpec (Just 100) (Just 200)
        , AddOutputUTXOSpec (pubkeyAddress "test") (ValueSpec 1000000 [])
        , RemoveOutputUTXOSpec 0
        , SetScriptDatumSpec (Json.String "I 42")
        , AddOutputUTXOWithDatumSpec
            (pubkeyAddress "test")
            (ValueSpec 1000000 [])
            (Json.String "I 42")
        , SetScriptInfoSpec (MintingScriptInfoSpec "@policy")
        , SetMintSpec []
        ]

    context "value with assets and datum" do
      it "parses add_input_utxo with assets and datum" do
        let expected :: PatchOperationSpec
            expected =
              AddInputUTXOSpec
                "txid:0"
                Nothing
                (ValueSpec 2000000 [AssetSpec "@cs" "@tn" 1000])
                True
                (Just (Json.String "I 42"))
        case Json.eitherDecode
          [__i|{
            "op": "add_input_utxo",
            "utxo_ref": "txid:0",
            "value": {
              "lovelace": 2000000,
              "assets": [{
                "currency_symbol": "@cs",
                "token_name": "@tn",
                "quantity": 1000
              }]
            },
            "is_own_input": true,
            "datum": "I 42"
          }|] of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right parsedSpec -> parsedSpec `shouldBe` expected

      it "parses add_output_utxo with assets" do
        let expected :: PatchOperationSpec
            expected =
              AddOutputUTXOSpec
                (AddressSpec (ScriptCredentialSpec "1111") Nothing)
                (ValueSpec 2000000 [AssetSpec "@cs" "@tn" 500])
        case Json.eitherDecode
          [__i|{
            "op": "add_output_utxo",
            "address": {
              "type": "script",
              "script_hash": "1111"
            },
            "value": {
              "lovelace": 2000000,
              "assets": [{
                "currency_symbol": "@cs",
                "token_name": "@tn",
                "quantity": 500
              }]
            }
          }|] of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right parsedSpec -> parsedSpec `shouldBe` expected

    context "addresses and script purposes" do
      it "parses add_input_utxo with an address" do
        Json.eitherDecode
          [__i|{
            "op": "add_input_utxo",
            "utxo_ref": "\#33:0",
            "address": {
              "type": "script",
              "script_hash": "@escrow_script_hash"
            },
            "value": {
              "lovelace": 75000000
            },
            "is_own_input": true
          }|]
          `shouldBe` Right
            ( AddInputUTXOSpec
                "#33:0"
                (Just (AddressSpec (ScriptCredentialSpec "@escrow_script_hash") Nothing))
                (ValueSpec 75000000 [])
                True
                Nothing
            )

      it "parses an address with a staking credential" do
        Json.eitherDecode
          [__i|{
            "type": "pubkey",
            "pubkey_hash": "@seller_pubkey",
            "staking": {
              "type": "script",
              "script_hash": "@stake_script"
            }
          }|]
          `shouldBe` Right
            ( AddressSpec
                (PubkeyCredentialSpec "@seller_pubkey")
                (Just (ScriptCredentialSpec "@stake_script"))
            )

      it "rejects an unknown credential type" do
        Json.eitherDecode @AddressSpec
          [__i|{
            "type": "pointer",
            "pubkey_hash": "@seller_pubkey"
          }|]
          `shouldSatisfy` isLeft

      it "parses set_script_info with a rewarding purpose" do
        Json.eitherDecode
          [__i|{
            "op": "set_script_info",
            "script_info": {
              "type": "rewarding",
              "credential": {
                "type": "script",
                "script_hash": "@escrow_script_hash"
              }
            }
          }|]
          `shouldBe` Right
            ( SetScriptInfoSpec
                (RewardingScriptInfoSpec (ScriptCredentialSpec "@escrow_script_hash"))
            )

      it "rejects an unknown script info type" do
        Json.eitherDecode @PatchOperationSpec
          [__i|{
            "op": "set_script_info",
            "script_info": {
              "type": "spending"
            }
          }|]
          `shouldSatisfy` isLeft

  describe "transaction ids in utxo_ref" do
    let ownRef ctx = case V3.scriptContextScriptInfo ctx of
          V3.SpendingScript (V3.TxOutRef (V3.TxId txId) index) _ ->
            (Builtins.fromBuiltin txId :: ByteString, index)
          _ -> error "expected SpendingScript"
        ownInput :: Text -> LByteString
        ownInput utxoRef =
          [__i|[{
            "op": "add_input_utxo",
            "utxo_ref": "#{utxoRef}",
            "value": {
              "lovelace": 75000000
            },
            "is_own_input": true
          }]|]

    it "decodes a #-prefixed transaction id from hex" do
      ctx <- patchedContext mempty (ownInput ("#" <> Text.replicate 32 "3a" <> ":1"))
      ownRef ctx `shouldBe` (fromString (replicate 32 '\x3a'), 1)

    it "keeps an unprefixed transaction id as its raw characters" do
      ctx <- patchedContext mempty (ownInput (Text.replicate 32 "3a" <> ":1"))
      ownRef ctx `shouldBe` (fromString (concat (replicate 32 "3a")), 1)

    it "fails on malformed hex" do
      patchedContext mempty (ownInput "#3g:0") `shouldThrow` (== ExitFailure 1)

    it "resolves a reference to the text of a utxo_ref, in both forms" do
      let references =
            Map.fromList
              [
                ( "hex_ref"
                , BuiltinDataEntry (Json.String ("#" <> Text.replicate 32 "3a" <> ":1"))
                )
              , ("legacy_ref", BuiltinDataEntry (Json.String "3a3a:2"))
              ]
      hexContext <- patchedContext references (ownInput "@hex_ref")
      ownRef hexContext `shouldBe` (fromString (replicate 32 '\x3a'), 1)
      legacyContext <- patchedContext references (ownInput "@legacy_ref")
      ownRef legacyContext `shouldBe` ("3a3a", 2)

  describe "credentials" do
    let scriptOutput :: Text -> LByteString
        scriptOutput scriptHash =
          [__i|[{
            "op": "add_output_utxo",
            "address": {
              "type": "script",
              "script_hash": "#{scriptHash}"
            },
            "value": {
              "lovelace": 75000000
            }
          }]|]
        outputAddresses = fmap V3.txOutAddress . contextOutputs

    it "resolves a script_hash reference to the bytes it points to" do
      ctx <- patchedContext hashes (scriptOutput "@escrow_script_hash")
      outputAddresses ctx
        `shouldBe` [V3.Address (V3.ScriptCredential escrowScriptHash) Nothing]

    it "keeps a literal script_hash as its raw characters" do
      ctx <- patchedContext hashes (scriptOutput "1111")
      outputAddresses ctx
        `shouldBe` [ V3.Address
                       (V3.ScriptCredential (V3.ScriptHash (Builtins.toBuiltin @ByteString "1111")))
                       Nothing
                   ]

    it "fails when a script_hash reference is not a bytestring" do
      patchedContext hashes (scriptOutput "@not_bytes")
        `shouldThrow` (== ExitFailure 1)

    it "fails when a script_hash reference does not exist" do
      patchedContext hashes (scriptOutput "@missing")
        `shouldThrow` (== ExitFailure 1)

    it "builds an address with a staking credential" do
      ctx <-
        patchedContext
          hashes
          [__i|[{
            "op": "add_output_utxo",
            "address": {
              "type": "script",
              "script_hash": "@escrow_script_hash",
              "staking": {
                "type": "pubkey",
                "pubkey_hash": "@stake_key"
              }
            },
            "value": {
              "lovelace": 75000000
            }
          }]|]
      outputAddresses ctx
        `shouldBe` [ V3.Address
                       (V3.ScriptCredential escrowScriptHash)
                       (Just (V3.StakingHash (V3.PubKeyCredential stakeKey)))
                   ]

    it "puts an input at the given address" do
      ctx <-
        patchedContext
          hashes
          [__i|[{
            "op": "add_input_utxo",
            "utxo_ref": "\#33:0",
            "address": {
              "type": "script",
              "script_hash": "@escrow_script_hash"
            },
            "value": {
              "lovelace": 75000000
            },
            "is_own_input": false
          }]|]
      fmap (V3.txOutAddress . V3.txInInfoResolved) (contextInputs ctx)
        `shouldBe` [V3.Address (V3.ScriptCredential escrowScriptHash) Nothing]

  describe "set_script_info" do
    it "switches a spending context to minting under the policy" do
      ctx <-
        patchedContext
          hashes
          [__i|[{
            "op": "set_script_info",
            "script_info": {
              "type": "minting",
              "policy_id": "@escrow_script_hash"
            }
          }]|]
      case V3.scriptContextScriptInfo ctx of
        V3.MintingScript (V3.CurrencySymbol policyId) ->
          policyId `shouldBe` V3.getScriptHash escrowScriptHash
        _ -> expectationFailure "Expected MintingScript"

    it "switches a spending context to rewarding for the credential" do
      ctx <-
        patchedContext
          hashes
          [__i|[{
            "op": "set_script_info",
            "script_info": {
              "type": "rewarding",
              "credential": {
                "type": "script",
                "script_hash": "@escrow_script_hash"
              }
            }
          }]|]
      case V3.scriptContextScriptInfo ctx of
        V3.RewardingScript credential ->
          credential `shouldBe` V3.ScriptCredential escrowScriptHash
        _ -> expectationFailure "Expected RewardingScript"

  describe "buildMintValue" do
    it "mints and burns with signed quantities and no ADA entry" do
      builtMintData [AssetSpec "B #dd" "B #01" 1]
        `shouldReturn` Map [(B "\xdd", Map [(B "\x01", I 1)])]
      builtMintData [AssetSpec "B #dd" "B #01" (-1)]
        `shouldReturn` Map [(B "\xdd", Map [(B "\x01", I (-1))])]

    it "sorts policies and token names in ascending byte order" do
      builtMintData
        [ AssetSpec "B #ee" "B #02" 1
        , AssetSpec "B #dd" "B #02" (-5)
        , AssetSpec "B #dd" "B #01" 7
        ]
        `shouldReturn` Map
          [ (B "\xdd", Map [(B "\x01", I 7), (B "\x02", I (-5))])
          , (B "\xee", Map [(B "\x02", I 1)])
          ]

    it "sums duplicate coins and drops the ones that cancel" do
      builtMintData
        [ AssetSpec "B #dd" "B #01" 3
        , AssetSpec "B #dd" "B #01" (-1)
        , AssetSpec "B #ee" "B #01" 1
        , AssetSpec "B #ee" "B #01" (-1)
        ]
        `shouldReturn` Map [(B "\xdd", Map [(B "\x01", I 2)])]

    it "gives an empty map for no assets" do
      builtMintData [] `shouldReturn` Map []

    it "refuses to mint ADA" do
      builtMintData [AssetSpec "B #" "B #" 1] `shouldThrow` (== ExitFailure 1)

    it "sets the mint field of the context from set_mint" do
      ctx <-
        patchedContext
          hashes
          [__i|[{
            "op": "set_mint",
            "assets": [{
              "currency_symbol": "@escrow_script_hash",
              "token_name": "@token_name",
              "quantity": -1
            }]
          }]|]
      Builtins.builtinDataToData
        (V3.toBuiltinData (V3.txInfoMint (V3.scriptContextTxInfo ctx)))
        `shouldBe` Map
          [
            ( B (Builtins.fromBuiltin (V3.getScriptHash escrowScriptHash))
            , Map [(B "escrow", I (-1))]
            )
          ]

  describe "buildValue" do
    let ada lovelace = (B "", Map [(B "", I lovelace)])

    it "puts ADA's empty currency symbol before asset policies" do
      builtValueData (ValueSpec 2000000 [AssetSpec "B #dddd" "B #76657374" 1000])
        `shouldReturn` Map [ada 2000000, (B "\xdd\xdd", Map [(B "vest", I 1000)])]

    it "sorts currency symbols and token names in ascending byte order" do
      builtValueData
        ( ValueSpec
            2000000
            [ AssetSpec "B #ee" "B #02" 1
            , AssetSpec "B #dd" "B #02" 5
            , AssetSpec "B #dd" "B #01" 7
            ]
        )
        `shouldReturn` Map
          [ ada 2000000
          , (B "\xdd", Map [(B "\x01", I 7), (B "\x02", I 5)])
          , (B "\xee", Map [(B "\x02", I 1)])
          ]

    it "sums duplicate coins" do
      builtValueData
        (ValueSpec 2000000 [AssetSpec "B #dd" "B #01" 3, AssetSpec "B #dd" "B #01" 4])
        `shouldReturn` Map [ada 2000000, (B "\xdd", Map [(B "\x01", I 7)])]

    it "drops zero quantities, as the ledger does" do
      builtValueData (ValueSpec 2000000 [AssetSpec "B #dd" "B #01" 0])
        `shouldReturn` Map [ada 2000000]

  describe "Multiple inputs support" do
    context "JSON parsing with inputs array" do
      it "parses single input in inputs array" do
        let jsonText =
              [__i|{
              "name": "test_single",
              "inputs": [{"type": "uplc", "value": "(con integer 42)"}],
              "expected": {"type": "value", "content": "(con integer 42)"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_single"
            length (tcInputs testCase) `shouldBe` 1

      it "parses multiple UPLC inputs in inputs array" do
        let jsonText =
              [__i|{
              "name": "test_multi",
              "inputs": [
                {"type": "uplc", "value": "(con integer 48)"},
                {"type": "uplc", "value": "(con integer 18)"}
              ],
              "expected": {"type": "value", "content": "(con integer 6)"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_multi"
            length (tcInputs testCase) `shouldBe` 2
            tiType (tcInputs testCase !! 0) `shouldBe` UPLC
            tiType (tcInputs testCase !! 1) `shouldBe` UPLC

      it "parses empty inputs array for error-only tests" do
        let jsonText =
              [__i|{
              "name": "test_empty",
              "inputs": [],
              "expected": {"type": "error"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_empty"
            length (tcInputs testCase) `shouldBe` 0

      it "parses BuiltinData inputs in both UPLC text and Plutus JSON forms" do
        let jsonText =
              [__i|{
              "name": "test_builtin_multi",
              "inputs": [
                {"type": "builtin_data", "value": "I 42"},
                {"type": "builtin_data", "value": {"int": 100}}
              ],
              "expected": {"type": "value", "content": "(con integer 142)"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_builtin_multi"
            length (tcInputs testCase) `shouldBe` 2
            tiType (tcInputs testCase !! 0) `shouldBe` BuiltinData
            tiType (tcInputs testCase !! 1) `shouldBe` BuiltinData

  describe "Aggregation policy" do
    let parseSuite :: LByteString -> (TestSuite -> Expectation) -> Expectation
        parseSuite jsonText check =
          case Json.eitherDecode jsonText of
            Left err -> expectationFailure $ "Failed to parse: " <> err
            Right suite -> check suite

    it "tags measurements as included and checks as excluded" do
      parseSuite
        [__i|{
          "version": "3.0.0",
          "measurements": [
            {"name": "happy", "inputs": [], "expected": {"type": "value", "content": "(con unit ())"}}
          ],
          "checks": [
            {"name": "attack", "inputs": [], "expected": {"type": "error"}}
          ]
        }|]
        \suite ->
          [(p, tcName tc) | (p, tc) <- suiteTests suite]
            `shouldBe` [(IncludedInAggregates, "happy"), (ExcludedFromAggregates, "attack")]

    it "defaults checks to empty when the key is absent" do
      parseSuite
        [__i|{
          "version": "3.0.0",
          "measurements": [
            {"name": "happy", "inputs": [], "expected": {"type": "value", "content": "(con unit ())"}}
          ]
        }|]
        \suite -> do
          map (tcName . snd) (suiteTests suite) `shouldBe` ["happy"]
          map tcName (tsChecks suite) `shouldBe` []

    it "allows a measurement to expect an error" do
      parseSuite
        [__i|{
          "version": "3.0.0",
          "measurements": [
            {"name": "costed_rejection", "inputs": [], "expected": {"type": "error"}}
          ]
        }|]
        \suite ->
          [(p, tcName tc) | (p, tc) <- suiteTests suite]
            `shouldBe` [(IncludedInAggregates, "costed_rejection")]

    it "excludes pending measurements from aggregates" do
      parseSuite
        [__i|{
          "version": "3.0.0",
          "measurements": [
            {"name": "broken", "pending": true, "inputs": [], "expected": {"type": "value", "content": "(con unit ())"}}
          ]
        }|]
        \suite ->
          [effectiveAggregationPolicy p tc | (p, tc) <- suiteTests suite]
            `shouldBe` [ExcludedFromAggregates]
