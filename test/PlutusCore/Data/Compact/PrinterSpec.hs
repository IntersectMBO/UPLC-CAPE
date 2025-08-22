module PlutusCore.Data.Compact.PrinterSpec (spec) where

import Prelude

import Data.ByteString qualified as BS
import PlutusCore.Data (Data (..))
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText)
import PlutusCore.Data.Compact.Printer (dataToCompactText)
import Test.Hspec

spec :: Spec
spec = describe "PlutusCore.Data.Compact.Printer" do
  describe "dataToCompactText" do
    it "converts integer data" do
      dataToCompactText (I 42) `shouldBe` "42"

    it "converts negative integer data" do
      dataToCompactText (I (-123)) `shouldBe` "-123"

    it "converts zero" do
      dataToCompactText (I 0) `shouldBe` "0"

    it "converts large integers" do
      dataToCompactText (I 123456789) `shouldBe` "123456789"
      dataToCompactText (I (-987654321)) `shouldBe` "-987654321"

    it "converts empty bytestring data" do
      dataToCompactText (B "") `shouldBe` "#"

    it "converts bytestring data" do
      dataToCompactText (B "test") `shouldBe` "#74657374"

    it "converts various hex patterns" do
      dataToCompactText (B (BS.pack [0xde, 0xad, 0xbe, 0xef])) `shouldBe` "#deadbeef"
      dataToCompactText (B (BS.pack [0xca, 0xfe])) `shouldBe` "#cafe"
      dataToCompactText (B (BS.pack [0x01, 0x02, 0x03])) `shouldBe` "#010203"

    it "converts constructor with no args" do
      dataToCompactText (Constr 0 []) `shouldBe` "0()"
      dataToCompactText (Constr 42 []) `shouldBe` "42()"

    it "converts constructor with args" do
      dataToCompactText (Constr 1 [I 42, B "test"]) `shouldBe` "1(42 #74657374)"
      dataToCompactText (Constr 0 [I (-1), B (BS.pack [0xab, 0xcd])])
        `shouldBe` "0(-1 #abcd)"

    it "converts nested constructors" do
      dataToCompactText (Constr 0 [Constr 1 []]) `shouldBe` "0(1())"
      dataToCompactText (Constr 0 [Constr 1 [Constr 2 []]]) `shouldBe` "0(1(2()))"

    it "converts empty list" do
      dataToCompactText (List []) `shouldBe` "[]"

    it "converts list with items" do
      dataToCompactText (List [I 1, I 2, I 3]) `shouldBe` "[1 2 3]"
      dataToCompactText (List [I 42, B "test", Constr 0 []])
        `shouldBe` "[42 #74657374 0()]"

    it "converts nested lists" do
      dataToCompactText (List [List [I 1], List [I 2, I 3]]) `shouldBe` "[[1] [2 3]]"

    it "converts empty map" do
      dataToCompactText (Map []) `shouldBe` "{}"

    it "converts map with pairs" do
      dataToCompactText (Map [(I 1, I 42), (B "key", B "val")])
        `shouldBe` "{1:42 #6b6579:#76616c}"

    it "converts complex map structures" do
      dataToCompactText
        (Map [(I 42, Constr 0 []), (B (BS.pack [0xab, 0xcd]), List [I 1, I 2])])
        `shouldBe` "{42:0() #abcd:[1 2]}"

    it "converts deeply nested structures" do
      let complexData =
            Constr
              0
              [ List [I 1, I 2]
              , Map [(I 42, B "test")]
              , Constr 1 [B (BS.pack [0xca, 0xfe])]
              ]
      dataToCompactText complexData `shouldBe` "0([1 2] {42:#74657374} 1(#cafe))"

  describe "round-trip property tests" do
    it "round-trips integers" do
      let testCases = [I 0, I 42, I (-1), I 123456789, I (-987654321)]
      forM_ testCases \original -> do
        let printed = dataToCompactText original
        let parsed = parseBuiltinDataText printed
        parsed `shouldBe` Right original

    it "round-trips bytestrings" do
      let testCases =
            [ B ""
            , B "test"
            , B (BS.pack [0x00])
            , B (BS.pack [0xff])
            , B (BS.pack [0xde, 0xad, 0xbe, 0xef])
            , B (BS.pack [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef])
            ]
      forM_ testCases \original -> do
        let printed = dataToCompactText original
        let parsed = parseBuiltinDataText printed
        parsed `shouldBe` Right original

    it "round-trips constructors" do
      let testCases =
            [ Constr 0 []
            , Constr 42 []
            , Constr 1 [I 42]
            , Constr 0 [I 1, I 2, I 3]
            , Constr 5 [B "test", I (-123)]
            , Constr 0 [Constr 1 [Constr 2 []]]
            ]
      forM_ testCases \original -> do
        let printed = dataToCompactText original
        let parsed = parseBuiltinDataText printed
        parsed `shouldBe` Right original

    it "round-trips lists" do
      let testCases =
            [ List []
            , List [I 1]
            , List [I 1, I 2, I 3]
            , List [B "test", Constr 0 []]
            , List [List [I 1], List [I 2, I 3]]
            , List [I 42, B (BS.pack [0xbe, 0xef]), Constr 0 []]
            ]
      forM_ testCases \original -> do
        let printed = dataToCompactText original
        let parsed = parseBuiltinDataText printed
        parsed `shouldBe` Right original

    it "round-trips maps" do
      let testCases =
            [ Map []
            , Map [(I 1, I 42)]
            , Map [(I 1, I 42), (I 2, B "test")]
            , Map [(B "key", Constr 0 []), (I 42, List [I 1, I 2])]
            ]
      forM_ testCases \original -> do
        let printed = dataToCompactText original
        let parsed = parseBuiltinDataText printed
        parsed `shouldBe` Right original

    it "round-trips complex structures" do
      let complexData =
            Constr
              0
              [ List [I 1, I 2, I 3]
              , Map
                  [ (I 42, B "value")
                  , (B "key", Constr 1 [I (-456)])
                  ]
              , Constr
                  2
                  [ B (BS.pack [0xa1, 0xb2, 0xc3, 0xd4])
                  , List []
                  , Map []
                  ]
              ]
      let printed = dataToCompactText complexData
      let parsed = parseBuiltinDataText printed
      parsed `shouldBe` Right complexData

    it "round-trips escrow-like structures" do
      let pubkey = BS.pack $ replicate 32 0xa1
      let escrowData = Constr 0 [B pubkey, I 1000, Constr 0 []]
      let printed = dataToCompactText escrowData
      let parsed = parseBuiltinDataText printed
      parsed `shouldBe` Right escrowData

  describe "format compliance" do
    it "produces parseable output for all data types" do
      let testCases =
            [ I 42
            , B "test"
            , Constr 1 [I 2, B "data"]
            , List [I 1, I 2, I 3]
            , Map [(I 1, B "one"), (I 2, B "two")]
            ]
      forM_ testCases \original -> do
        let printed = dataToCompactText original
        parseBuiltinDataText printed `shouldSatisfy` isRight

    it "handles edge cases correctly" do
      let edgeCases =
            [ I 0
            , I (-1)
            , B ""
            , Constr 0 []
            , List []
            , Map []
            ]
      forM_ edgeCases \original -> do
        let printed = dataToCompactText original
        let parsed = parseBuiltinDataText printed
        parsed `shouldBe` Right original
