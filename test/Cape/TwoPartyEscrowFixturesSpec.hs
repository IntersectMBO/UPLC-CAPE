{- | Checks on the fixtures of the @two_party_escrow@ scenario themselves.

A fixture can be wrong in two ways that no submission would reveal: it can
expect a result the specification does not give, and it can describe a
transaction the ledger never builds. This module guards against both.

* 'violations' is the rule table of
  @scenarios/two_party_escrow/two_party_escrow.md@ as a plain function.
  Every test must expect exactly what it says, and every rejected test
  must break exactly the rules its description names.
* 'problems' lists the ways a context differs from one the ledger builds.
  Only the tests in 'synthetic' may differ, each in the declared way.
* 'restrictions' lists conditions that the rules do not demand. For each
  one a successful test must break it, or an implementation that demands
  more than the rules would pass the suite.
-}
module Cape.TwoPartyEscrowFixturesSpec (spec) where

import Prelude

import Cape.Tests
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import Test.Hspec

-- * The rule table

-- | The rules of the scenario, named as in its specification.
data Rule
  = D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | B1
  | S1
  | S2
  | S3
  | A1
  | A2
  | R1
  | R2
  | R3
  | X1
  | X2
  | X3
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

buyer, seller :: V3.PubKeyHash
buyer = V3.PubKeyHash (Builtins.toBuiltin (BS.replicate 28 0xaa))
seller = V3.PubKeyHash (Builtins.toBuiltin (BS.replicate 28 0xbb))

price, refundDelay :: Integer
price = 75_000_000
refundDelay = 1_800_000

tokenName :: V3.TokenName
tokenName = V3.TokenName (Builtins.toBuiltin @ByteString "escrow")

-- | The rules whose condition does not hold.
broken :: [(Rule, Bool)] -> Set Rule
broken conditions = Set.fromList [rule | (rule, False) <- conditions]

-- | The rules a context breaks; empty when the validator must return unit.
violations :: V3.ScriptContext -> Set Rule
violations (V3.ScriptContext txInfo (V3.Redeemer redeemer) scriptInfo) =
  case (scriptInfo, V3.fromBuiltinData @Integer redeemer) of
    (V3.MintingScript policy, Just 0) -> deposit txInfo policy
    (V3.MintingScript policy, Just 1) -> burn txInfo policy
    (V3.MintingScript policy, Just 2) -> burn txInfo policy
    (V3.SpendingScript ownRef _, Just 1) ->
      spend
        txInfo
        ownRef
        [ (A1, seller `elem` V3.txInfoSignatories txInfo)
        , (A2, paidTo seller txInfo >= price)
        ]
    (V3.SpendingScript ownRef datum, Just 2) ->
      spend
        txInfo
        ownRef
        [ (R1, buyer `elem` V3.txInfoSignatories txInfo)
        , (R2, deadlinePassed datum txInfo)
        , (R3, paidTo buyer txInfo >= price)
        ]
    (V3.MintingScript _, _) -> one X1
    (V3.SpendingScript _ _, _) -> one X1
    _ -> one X2

deposit :: V3.TxInfo -> V3.CurrencySymbol -> Set Rule
deposit txInfo policy =
  broken $
    [ (D1, buyer `elem` V3.txInfoSignatories txInfo)
    , (D2, isJust upperBound)
    , (D3, ownMint policy txInfo == [(tokenName, 1)])
    , (D4, length escrowOutputs == 1)
    ]
      <> case escrowOutputs of
        [output] ->
          [ (D5, lovelaceOf output == price)
          , (D6, Value.valueOf (V3.txOutValue output) policy tokenName == 1)
          ]
            <> [ (D7, V3.txOutDatum output == V3.OutputDatum (escrowDatum (upper - 1)))
               | Just upper <- [upperBound]
               ]
        _ -> []
  where
    upperBound = case V3.ivTo (V3.txInfoValidRange txInfo) of
      V3.UpperBound (V3.Finite (V3.POSIXTime upper)) _ -> Just upper
      _ -> Nothing
    escrowOutputs =
      filter (paysToScript (scriptHashOf policy)) (V3.txInfoOutputs txInfo)

burn :: V3.TxInfo -> V3.CurrencySymbol -> Set Rule
burn txInfo policy = broken [(B1, ownMint policy txInfo == [(tokenName, -1)])]

{- | The rules that Accept and Refund share, followed by the rules of the
one action. Without the script's own input nothing else can be judged.
-}
spend :: V3.TxInfo -> V3.TxOutRef -> [(Rule, Bool)] -> Set Rule
spend txInfo ownRef actionRules =
  case ownScriptHash ownRef txInfo of
    Nothing -> one S1
    Just ownHash ->
      broken $
        [ (S2, length (filter (paysToScript ownHash) spentOutputs) == 1)
        ,
          ( S3
          , lookup tokenName (ownMint (policyOf ownHash) txInfo) == Just (-1)
          )
        ]
          <> actionRules
  where
    spentOutputs = V3.txInInfoResolved <$> V3.txInfoInputs txInfo

deadlinePassed :: Maybe V3.Datum -> V3.TxInfo -> Bool
deadlinePassed datum txInfo =
  case (datum >>= depositTimeOf, V3.ivFrom (V3.txInfoValidRange txInfo)) of
    (Just depositTime, V3.LowerBound (V3.Finite (V3.POSIXTime lower)) _) ->
      lower > depositTime + refundDelay
    _ -> False
  where
    depositTimeOf (V3.Datum datumData) =
      case Builtins.builtinDataToData datumData of
        PLC.Constr 0 [PLC.I depositTime] -> Just depositTime
        _ -> Nothing

escrowDatum :: Integer -> V3.Datum
escrowDatum depositTime =
  V3.Datum (Builtins.dataToBuiltinData (PLC.Constr 0 [PLC.I depositTime]))

-- | The script hash at the address of the script's own input.
ownScriptHash :: V3.TxOutRef -> V3.TxInfo -> Maybe V3.ScriptHash
ownScriptHash ownRef txInfo =
  listToMaybe
    [ scriptHash
    | V3.TxInInfo ref output <- V3.txInfoInputs txInfo
    , ref == ownRef
    , V3.ScriptCredential scriptHash <- [paymentCredential output]
    ]

-- | The mint entry of one policy: its token names and quantities.
ownMint :: V3.CurrencySymbol -> V3.TxInfo -> [(V3.TokenName, Integer)]
ownMint policy txInfo =
  maybe [] AssocMap.toList $
    AssocMap.lookup policy (V3.mintValueToMap (V3.txInfoMint txInfo))

-- | Lovelace in all outputs at the payment credential of a public key.
paidTo :: V3.PubKeyHash -> V3.TxInfo -> Integer
paidTo key txInfo =
  sum
    [ lovelaceOf output
    | output <- V3.txInfoOutputs txInfo
    , paymentCredential output == V3.PubKeyCredential key
    ]

paysToScript :: V3.ScriptHash -> V3.TxOut -> Bool
paysToScript scriptHash output =
  paymentCredential output == V3.ScriptCredential scriptHash

paymentCredential :: V3.TxOut -> V3.Credential
paymentCredential = V3.addressCredential . V3.txOutAddress

lovelaceOf :: V3.TxOut -> Integer
lovelaceOf = V3.getLovelace . Value.lovelaceValueOf . V3.txOutValue

scriptHashOf :: V3.CurrencySymbol -> V3.ScriptHash
scriptHashOf = V3.ScriptHash . V3.unCurrencySymbol

policyOf :: V3.ScriptHash -> V3.CurrencySymbol
policyOf = V3.CurrencySymbol . V3.getScriptHash

-- * What the rules leave open

-- | The action a context asks for, as the rule table selects it.
data Action = Deposit | Burn Integer | Accept | Refund
  deriving stock (Show, Eq)

actionOf :: V3.ScriptContext -> Maybe Action
actionOf (V3.ScriptContext _ (V3.Redeemer redeemer) scriptInfo) =
  case (scriptInfo, V3.fromBuiltinData @Integer redeemer) of
    (V3.MintingScript _, Just 0) -> Just Deposit
    (V3.MintingScript _, Just 1) -> Just (Burn 1)
    (V3.MintingScript _, Just 2) -> Just (Burn 2)
    (V3.SpendingScript _ _, Just 1) -> Just Accept
    (V3.SpendingScript _ _, Just 2) -> Just Refund
    _ -> Nothing

{- | Conditions that hold in the plain lifecycle and that no rule demands.
Each one names the actions it could be added to and holds when a context
satisfies it.
-}
restrictions :: [(String, [Action], V3.ScriptContext -> Bool)]
restrictions =
  [ ("a finite lower bound", [Deposit, Accept], finite . lowerBound)
  , ("a finite upper bound", [Accept, Refund], finite . upperBound)
  ,
    ( "a lower bound before the refund deadline"
    , [Accept]
    , \ctx -> case lowerBound ctx of
        V3.Finite (V3.POSIXTime lower) -> lower < 1_767_228_000_000
        _ -> True
    )
  ,
    ( "one signatory"
    , everyAction
    , (== 1) . length . V3.txInfoSignatories . txInfoOf
    )
  ,
    ( "the seller's signature"
    , [Burn 1]
    , elem seller . V3.txInfoSignatories . txInfoOf
    )
  ,
    ( "the buyer's signature"
    , [Burn 2]
    , elem buyer . V3.txInfoSignatories . txInfoOf
    )
  ,
    ( "the same redeemer for the burn and for the escrow input"
    , [Burn 1, Burn 2, Accept, Refund]
    , (<= 1)
        . Set.size
        . Set.fromList
        . AssocMap.elems
        . V3.txInfoRedeemers
        . txInfoOf
    )
  , ("no mint of another policy", everyAction, (== 1) . length . mintedPolicies)
  ,
    ( "no input of another script"
    , [Deposit, Accept, Refund]
    , \ctx -> all ((`elem` [Nothing, ownHash ctx]) . scriptHashOfOutput) (spent ctx)
    )
  ,
    ( "no output at another script"
    , [Deposit]
    , \ctx -> all ((`elem` [Nothing, ownHash ctx]) . scriptHashOfOutput) (created ctx)
    )
  , ("no output at the escrow script", [Accept, Refund], null . atEscrow created)
  ,
    ( "the escrow output first"
    , [Deposit]
    , atEscrowFirst created
    )
  ,
    ( "the escrow input first"
    , [Accept, Refund]
    , atEscrowFirst spent
    )
  ,
    ( "the escrow input last"
    , [Accept, Refund]
    , atEscrowFirst (reverse . spent)
    )
  ,
    ( "no staking credential on the escrow output"
    , [Deposit]
    , all unstaked . atEscrow created
    )
  ,
    ( "no reference script on the escrow output"
    , [Deposit]
    , all scriptless . atEscrow created
    )
  ,
    ( "no other asset in the escrow output"
    , [Deposit]
    , all ((== 2) . coinCount) . atEscrow created
    )
  ,
    ( "no staking credential on the escrow input"
    , [Accept, Refund]
    , all unstaked . atEscrow spent
    )
  ,
    ( "no reference script on the escrow input"
    , [Accept, Refund]
    , all scriptless . atEscrow spent
    )
  ,
    ( "no other asset in the escrow input"
    , [Accept, Refund]
    , all ((== 2) . coinCount) . atEscrow spent
    )
  ,
    ( "no staking credential on the payment"
    , [Accept, Refund]
    , all unstaked . payments
    )
  ,
    ( "no datum on the payment"
    , [Accept, Refund]
    , all ((== V3.NoOutputDatum) . V3.txOutDatum) . payments
    )
  ,
    ( "no reference script on the payment"
    , [Accept, Refund]
    , all scriptless . payments
    )
  ,
    ( "no other asset in the payment"
    , [Accept, Refund]
    , all ((== 1) . coinCount) . payments
    )
  ,
    ( "one output that holds the whole payment"
    , [Accept, Refund]
    , any ((>= price) . lovelaceOf) . payments
    )
  ,
    ( "a payment of exactly 75 ADA"
    , [Accept, Refund]
    , (== price) . sum . map lovelaceOf . payments
    )
  ]
  where
    everyAction = [Deposit, Burn 1, Burn 2, Accept, Refund]
    txInfoOf = V3.scriptContextTxInfo
    spent = fmap V3.txInInfoResolved . V3.txInfoInputs . txInfoOf
    created = V3.txInfoOutputs . txInfoOf
    lowerBound =
      (\(V3.LowerBound bound _) -> bound) . V3.ivFrom . V3.txInfoValidRange . txInfoOf
    upperBound =
      (\(V3.UpperBound bound _) -> bound) . V3.ivTo . V3.txInfoValidRange . txInfoOf
    finite = \case
      V3.Finite _ -> True
      _ -> False
    mintedPolicies = AssocMap.keys . V3.mintValueToMap . V3.txInfoMint . txInfoOf
    ownHash ctx = case V3.scriptContextScriptInfo ctx of
      V3.MintingScript policy -> Just (scriptHashOf policy)
      V3.SpendingScript ownRef _ -> ownScriptHash ownRef (txInfoOf ctx)
      _ -> Nothing
    scriptHashOfOutput output = case paymentCredential output of
      V3.ScriptCredential scriptHash -> Just scriptHash
      V3.PubKeyCredential _ -> Nothing
    atEscrow outputsOf ctx =
      filter ((== ownHash ctx) . scriptHashOfOutput) (outputsOf ctx)
    -- The outputs of the party that the action pays.
    payments ctx =
      [ output
      | output <- created ctx
      , Just (paymentCredential output)
          == (V3.PubKeyCredential <$> receiver ctx)
      ]
    receiver ctx = case actionOf ctx of
      Just Accept -> Just seller
      Just Refund -> Just buyer
      _ -> Nothing
    unstaked = isNothing . V3.addressStakingCredential . V3.txOutAddress
    scriptless = isNothing . V3.txOutReferenceScript
    coinCount = length . Value.flattenValue . V3.txOutValue
    atEscrowFirst outputsOf ctx =
      (listToMaybe (outputsOf ctx) >>= scriptHashOfOutput) == ownHash ctx

-- * The shape of a ledger-built context

-- | A way in which a context differs from one the ledger builds.
data Problem
  = HashNot28Bytes
  | TxIdNot32Bytes
  | InputsOutOfLedgerOrder
  | SignatoriesOutOfLedgerOrder
  | ValueNotConserved
  | FeeIsZero
  | OutputBelowOneAda
  | BoundNotSlotAligned
  | OwnInputNotSpent
  | OwnDatumDiffersFromInput
  | OwnPolicyNotMinted
  | RedeemersDifferFromScripts
  deriving stock (Show, Eq, Ord)

{- | The tests that are synthetic on purpose, with the exact way each one
differs from a ledger-built context. The specification lists them too.
-}
synthetic :: Map Text (Set Problem)
synthetic =
  Map.fromList $
    ("refund_at_exact_deadline", one BoundNotSlotAligned)
      : [ ( "rewarding_purpose_redeemer_" <> show redeemer
          , one RedeemersDifferFromScripts
          )
        | redeemer <- [0 :: Int, 1, 2]
        ]

problems :: V3.ScriptContext -> Set Problem
problems (V3.ScriptContext txInfo redeemer scriptInfo) =
  Set.fromList . map fst . filter (not . snd) $
    [ (HashNot28Bytes, all ((== 28) . BS.length) hashes)
    , (TxIdNot32Bytes, all ((== 32) . BS.length) txIds)
    , (InputsOutOfLedgerOrder, strictlyAscending inputRefs)
    ,
      ( SignatoriesOutOfLedgerOrder
      , strictlyAscending (V3.getPubKeyHash <$> V3.txInfoSignatories txInfo)
      )
    , (ValueNotConserved, consumed == produced)
    , (FeeIsZero, V3.txInfoFee txInfo > 0)
    , (OutputBelowOneAda, all ((>= 1_000_000) . lovelaceOf) outputs)
    , (BoundNotSlotAligned, all ((== 0) . (`mod` 1000)) finiteBounds)
    , (RedeemersDifferFromScripts, redeemersMatch)
    ]
      <> case scriptInfo of
        V3.SpendingScript ownRef ownDatum ->
          let ownInputs = [out | V3.TxInInfo ref out <- inputs, ref == ownRef]
           in [ (OwnInputNotSpent, length ownInputs == 1)
              ,
                ( OwnDatumDiffersFromInput
                , fmap V3.txOutDatum ownInputs
                    == [maybe V3.NoOutputDatum V3.OutputDatum ownDatum]
                )
              ]
        V3.MintingScript policy ->
          [(OwnPolicyNotMinted, not (null (ownMint policy txInfo)))]
        _ -> []
  where
    inputs = V3.txInfoInputs txInfo
    outputs = V3.txInfoOutputs txInfo
    spentOutputs = V3.txInInfoResolved <$> inputs
    inputRefs = [(txIdBytes ref, V3.txOutRefIdx ref) | V3.TxInInfo ref _ <- inputs]
    mintedPolicies = AssocMap.keys (V3.mintValueToMap (V3.txInfoMint txInfo))
    recordedRedeemers = AssocMap.toList (V3.txInfoRedeemers txInfo)

    txIdBytes = Builtins.fromBuiltin . V3.getTxId . V3.txOutRefId
    txIds =
      Builtins.fromBuiltin (V3.getTxId (V3.txInfoId txInfo))
        : (fst <$> inputRefs)
          <> [txIdBytes ref | (V3.Spending ref, _) <- recordedRedeemers]
          <> [txIdBytes ref | V3.SpendingScript ref _ <- [scriptInfo]]

    hashes =
      concatMap outputHashes (spentOutputs <> outputs)
        <> (Builtins.fromBuiltin . V3.getPubKeyHash <$> V3.txInfoSignatories txInfo)
        <> (Builtins.fromBuiltin . V3.unCurrencySymbol <$> mintedPolicies)
        <> [credentialBytes credential | Just credential <- [ownCredential]]
    outputHashes output =
      credentialBytes (paymentCredential output)
        : [ credentialBytes credential
          | Just (V3.StakingHash credential) <-
              [V3.addressStakingCredential (V3.txOutAddress output)]
          ]
          <> [ Builtins.fromBuiltin (V3.unCurrencySymbol policy)
             | (policy, _, _) <- Value.flattenValue (V3.txOutValue output)
             , policy /= V3.adaSymbol
             ]
          <> [ Builtins.fromBuiltin (V3.getScriptHash scriptHash)
             | Just scriptHash <- [V3.txOutReferenceScript output]
             ]
    credentialBytes = \case
      V3.PubKeyCredential (V3.PubKeyHash bytes) -> Builtins.fromBuiltin bytes
      V3.ScriptCredential (V3.ScriptHash bytes) -> Builtins.fromBuiltin bytes

    coins value =
      Map.fromListWith
        (+)
        [((policy, token), quantity) | (policy, token, quantity) <- value]
    minted =
      [ (policy, token, quantity)
      | (policy, tokens) <- AssocMap.toList (V3.mintValueToMap (V3.txInfoMint txInfo))
      , (token, quantity) <- AssocMap.toList tokens
      ]
    valueOfOutputs = concatMap (Value.flattenValue . V3.txOutValue)
    consumed =
      Map.filter (/= 0) (coins (valueOfOutputs spentOutputs <> minted))
    produced =
      coins $
        (V3.adaSymbol, V3.adaToken, V3.getLovelace (V3.txInfoFee txInfo))
          : valueOfOutputs outputs

    finiteBounds =
      [ time
      | V3.Finite (V3.POSIXTime time) <-
          [ (\(V3.LowerBound bound _) -> bound) (V3.ivFrom range)
          , (\(V3.UpperBound bound _) -> bound) (V3.ivTo range)
          ]
      ]
    range = V3.txInfoValidRange txInfo

    -- The script under test: its credential and its purpose in this run.
    (ownCredential, ownPurpose) = case scriptInfo of
      V3.MintingScript policy ->
        (Just (V3.ScriptCredential (scriptHashOf policy)), Just (V3.Minting policy))
      V3.SpendingScript ownRef _ ->
        ( V3.ScriptCredential <$> ownScriptHash ownRef txInfo
        , Just (V3.Spending ownRef)
        )
      V3.RewardingScript credential ->
        (Just credential, Just (V3.Rewarding credential))
      _ -> (Nothing, Nothing)
    -- The escrow script runs once for each input at its credential and
    -- once for its policy. Other scripts in the fixtures are native
    -- scripts, which have no redeemer.
    escrowPurposes = case ownCredential of
      Just credential@(V3.ScriptCredential scriptHash) ->
        [ V3.Spending ref
        | V3.TxInInfo ref output <- inputs
        , paymentCredential output == credential
        ]
          <> [ V3.Minting (policyOf scriptHash)
             | policyOf scriptHash `elem` mintedPolicies
             ]
      _ -> []
    redeemersMatch =
      (fst <$> recordedRedeemers) == escrowPurposes
        && (ownPurpose >>= (`lookup` recordedRedeemers)) == Just redeemer

strictlyAscending :: Ord a => [a] -> Bool
strictlyAscending xs = and (zipWith (<) xs (drop 1 xs))

-- * The suite

suiteFile :: FilePath
suiteFile = "scenarios/two_party_escrow/cape-tests.json"

-- | A test of the suite with its context, when its input is one.
data Fixture = Fixture
  { fixtureName :: Text
  , fixtureDescription :: Text
  , fixtureExpectsUnit :: Bool
  , fixtureContext :: Maybe V3.ScriptContext
  }

loadFixtures :: IO (TestSuite, [Fixture])
loadFixtures = do
  suite <- loadTestSuite suiteFile
  fixtures <- forM (snd <$> suiteTests suite) \testCase -> do
    contexts <- forM (tcInputs testCase) \input ->
      case tiType input of
        ScriptContext ->
          resolveTestInput (getTestBaseDir suiteFile) suite input <&> \case
            ResolvedBuiltinData contextData ->
              Just (V3.unsafeFromBuiltinData contextData)
            ResolvedUplc _ -> Nothing
        _ -> pure Nothing
    pure
      Fixture
        { fixtureName = tcName testCase
        , fixtureDescription = fromMaybe "" (tcDescription testCase)
        , fixtureExpectsUnit = case erType (tcExpected testCase) of
            ExpectedValue -> True
            ExpectedError -> False
        , fixtureContext = join (listToMaybe contexts)
        }
  pure (suite, fixtures)

{- | The rules a description names in its closing @(rule D4)@ or
@(rules S2, S3)@; none when it has no such closing.
-}
namedRules :: Text -> Either Text (Set Rule)
namedRules description =
  case Text.breakOnEnd "(rule" description of
    ("", _) -> Right mempty
    (_, closing) ->
      fmap Set.fromList
        . traverse parseRule
        . Text.splitOn ","
        . Text.dropWhileEnd (== ')')
        . Text.dropWhile (== 's')
        $ closing
  where
    parseRule name =
      maybeToRight ("unknown rule: " <> name) (readMaybe (toString (Text.strip name)))

spec :: Spec
spec = do
  (suite, fixtures) <- runIO loadFixtures
  let contextOf name =
        fixtures
          & find ((== name) . fixtureName)
          >>= fixtureContext
            & fromMaybe (error ("no context for " <> name))
      sameTransaction spending minting =
        it (toString minting <> " runs on the transaction of " <> toString spending) $
          V3.scriptContextTxInfo (contextOf minting)
            `shouldBe` V3.scriptContextTxInfo (contextOf spending)

  describe "two_party_escrow fixtures" do
    it "measures the five executions of the lifecycle and nothing else" $
      (tcName <$> tsMeasurements suite)
        `shouldBe` [ "deposit_successful"
                   , "accept_successful"
                   , "accept_burn"
                   , "refund_successful"
                   , "refund_burn"
                   ]

    describe "follow the rule table of the specification" do
      forM_ fixtures \Fixture {fixtureName, fixtureDescription, fixtureExpectsUnit, fixtureContext} ->
        it (toString fixtureName) do
          let broke = maybe (one X3) violations fixtureContext
          namedRules fixtureDescription `shouldBe` Right broke
          fixtureExpectsUnit `shouldBe` Set.null broke

    -- S1 is how the script finds its own hash. No ledger-built context
    -- breaks it: the spending purpose always refers to a script input.
    it "break every rule except S1 in a test that breaks nothing else" do
      let brokenAlone =
            Set.unions
              [ broke
              | Fixture {fixtureContext} <- fixtures
              , let broke = maybe (one X3) violations fixtureContext
              , Set.size broke == 1
              ]
      Set.fromList [minBound .. maxBound] `Set.difference` brokenAlone
        `shouldBe` one S1

    describe "show that the rules do not demand" do
      forM_ restrictions \(name, actions, holds) ->
        forM_ actions \action ->
          it (name <> " for " <> show action) $
            [ fixtureName
            | Fixture
                { fixtureName
                , fixtureExpectsUnit = True
                , fixtureContext = Just scriptContext
                } <-
                fixtures
            , actionOf scriptContext == Just action
            , not (holds scriptContext)
            ]
              `shouldNotBe` []

    describe "have the shape of a ledger-built context" do
      forM_ fixtures \Fixture {fixtureName, fixtureContext} ->
        forM_ fixtureContext \scriptContext ->
          it (toString fixtureName) $
            problems scriptContext
              `shouldBe` Map.findWithDefault mempty fixtureName synthetic

      it "declares only existing tests as synthetic" $
        Map.keysSet synthetic
          `shouldSatisfy` (`Set.isSubsetOf` Set.fromList (fixtureName <$> fixtures))

    describe "evaluate the burn on the transaction that spends" do
      sameTransaction "accept_successful" "accept_burn"
      sameTransaction "refund_successful" "refund_burn"
      sameTransaction "accept_successful_other_hash" "accept_burn_other_hash"
      sameTransaction "refund_successful_other_hash" "refund_burn_other_hash"
