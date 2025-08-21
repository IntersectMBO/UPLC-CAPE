module PlutusCore.Data.Compact.ParserSpec (spec) where

import Prelude

import Data.ByteString qualified as BS
import PlutusCore.Data (Data (..))
import PlutusCore.Data.Compact.Parser
import Test.Hspec

spec :: Spec
spec = describe "PlutusCore.Data.Compact.Parser" do
  describe "parseBuiltinDataText" do
    context "integers" do
      it "parses positive integers" do
        parseBuiltinDataText "42" `shouldBe` Right (I 42)
        parseBuiltinDataText "0" `shouldBe` Right (I 0)
        parseBuiltinDataText "123456" `shouldBe` Right (I 123456)

      it "parses negative integers" do
        parseBuiltinDataText "-42" `shouldBe` Right (I (-42))
        parseBuiltinDataText "-1" `shouldBe` Right (I (-1))

      it "handles whitespace around integers" do
        parseBuiltinDataText "  42  " `shouldBe` Right (I 42)
        parseBuiltinDataText "\n-123\t" `shouldBe` Right (I (-123))

    context "bytestrings" do
      it "parses empty bytestring" do
        parseBuiltinDataText "#" `shouldBe` Right (B BS.empty)

      it "parses hex bytestrings" do
        parseBuiltinDataText "#deadbeef"
          `shouldBe` Right (B (BS.pack [0xde, 0xad, 0xbe, 0xef]))
        parseBuiltinDataText "#a1b2c3"
          `shouldBe` Right (B (BS.pack [0xa1, 0xb2, 0xc3]))

      it "parses long hex bytestrings" do
        let longHex = "#a1b2c3d4e5f6789012345678abcdef0123456789abcdef0123456789abcdef01"
        let expected =
              [ 0xa1
              , 0xb2
              , 0xc3
              , 0xd4
              , 0xe5
              , 0xf6
              , 0x78
              , 0x90
              , 0x12
              , 0x34
              , 0x56
              , 0x78
              , 0xab
              , 0xcd
              , 0xef
              , 0x01
              , 0x23
              , 0x45
              , 0x67
              , 0x89
              , 0xab
              , 0xcd
              , 0xef
              , 0x01
              , 0x23
              , 0x45
              , 0x67
              , 0x89
              , 0xab
              , 0xcd
              , 0xef
              , 0x01
              ]
        parseBuiltinDataText longHex `shouldBe` Right (B (BS.pack expected))

      it "handles uppercase and lowercase hex" do
        parseBuiltinDataText "#DeAdBeEf"
          `shouldBe` Right (B (BS.pack [0xde, 0xad, 0xbe, 0xef]))

      it "handles whitespace around bytestrings" do
        parseBuiltinDataText "  #cafe  "
          `shouldBe` Right (B (BS.pack [0xca, 0xfe]))

      it "rejects odd-length hex" do
        parseBuiltinDataText "#abc" `shouldSatisfy` isLeft

      it "rejects invalid hex characters" do
        parseBuiltinDataText "#xyz" `shouldSatisfy` isLeft

    context "constructors" do
      it "parses empty constructor" do
        parseBuiltinDataText "0()" `shouldBe` Right (Constr 0 [])
        parseBuiltinDataText "42()" `shouldBe` Right (Constr 42 [])

      it "parses constructor with single argument" do
        parseBuiltinDataText "0(42)" `shouldBe` Right (Constr 0 [I 42])
        parseBuiltinDataText "1(#cafe)"
          `shouldBe` Right (Constr 1 [B (BS.pack [0xca, 0xfe])])

      it "parses constructor with multiple arguments" do
        parseBuiltinDataText "0(42 #beef)"
          `shouldBe` Right (Constr 0 [I 42, B (BS.pack [0xbe, 0xef])])
        parseBuiltinDataText "2(1 2 3)"
          `shouldBe` Right (Constr 2 [I 1, I 2, I 3])

      it "parses nested constructors" do
        parseBuiltinDataText "0(1())"
          `shouldBe` Right (Constr 0 [Constr 1 []])
        parseBuiltinDataText "0(1(2()))"
          `shouldBe` Right (Constr 0 [Constr 1 [Constr 2 []]])

      it "handles whitespace in constructors" do
        parseBuiltinDataText " 0 ( 42   #cafe ) "
          `shouldBe` Right (Constr 0 [I 42, B (BS.pack [0xca, 0xfe])])

      it "parses complex escrow examples" do
        let pubkey = BS.pack $ replicate 32 0xa1
        parseBuiltinDataText
          "0(#a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1 1000 0())"
          `shouldBe` Right (Constr 0 [B pubkey, I 1000, Constr 0 []])

    context "lists" do
      it "parses empty list" do
        parseBuiltinDataText "[]" `shouldBe` Right (List [])

      it "parses list with single element" do
        parseBuiltinDataText "[42]" `shouldBe` Right (List [I 42])
        parseBuiltinDataText "[#cafe]"
          `shouldBe` Right (List [B (BS.pack [0xca, 0xfe])])

      it "parses list with multiple elements" do
        parseBuiltinDataText "[1 2 3]" `shouldBe` Right (List [I 1, I 2, I 3])
        parseBuiltinDataText "[42 #beef 0()]"
          `shouldBe` Right (List [I 42, B (BS.pack [0xbe, 0xef]), Constr 0 []])

      it "parses nested lists" do
        parseBuiltinDataText "[[1] [2 3]]"
          `shouldBe` Right (List [List [I 1], List [I 2, I 3]])

      it "handles whitespace in lists" do
        parseBuiltinDataText " [ 1   2   3 ] "
          `shouldBe` Right (List [I 1, I 2, I 3])

    context "maps" do
      it "parses empty map" do
        parseBuiltinDataText "{}" `shouldBe` Right (Map [])

      it "parses map with single pair" do
        parseBuiltinDataText "{42:#76616c7565}"
          `shouldBe` Right (Map [(I 42, B (BS.pack [0x76, 0x61, 0x6c, 0x75, 0x65]))])

      it "parses map with multiple pairs" do
        parseBuiltinDataText "{1:42 #6b6579:0()}"
          `shouldBe` Right
            ( Map
                [ (I 1, I 42)
                , (B (BS.pack [0x6b, 0x65, 0x79]), Constr 0 [])
                ]
            )

      it "handles whitespace in maps" do
        parseBuiltinDataText " { 1 : 42   2 : #beef } "
          `shouldBe` Right
            ( Map
                [ (I 1, I 42)
                , (I 2, B (BS.pack [0xbe, 0xef]))
                ]
            )

    context "complex structures" do
      it "parses deeply nested structures" do
        parseBuiltinDataText "0([1 2] {42:#74657374} 1(#cafe))"
          `shouldBe` Right
            ( Constr
                0
                [ List [I 1, I 2]
                , Map [(I 42, B (BS.pack [0x74, 0x65, 0x73, 0x74]))]
                , Constr 1 [B (BS.pack [0xca, 0xfe])]
                ]
            )

      it "handles multiline formatting" do
        let multiline = "0(\n  #a1b2c3d4\n  1000\n  0()\n)"
        parseBuiltinDataText multiline
          `shouldBe` Right
            ( Constr
                0
                [ B (BS.pack [0xa1, 0xb2, 0xc3, 0xd4])
                , I 1000
                , Constr 0 []
                ]
            )

    context "error cases" do
      it "rejects malformed input" do
        parseBuiltinDataText "(" `shouldSatisfy` isLeft
        parseBuiltinDataText ")" `shouldSatisfy` isLeft
        parseBuiltinDataText "[" `shouldSatisfy` isLeft
        parseBuiltinDataText "]" `shouldSatisfy` isLeft
        parseBuiltinDataText "{" `shouldSatisfy` isLeft
        parseBuiltinDataText "}" `shouldSatisfy` isLeft

      it "rejects incomplete constructs" do
        parseBuiltinDataText "0(" `shouldSatisfy` isLeft
        parseBuiltinDataText "0(42" `shouldSatisfy` isLeft
        parseBuiltinDataText "[1 2" `shouldSatisfy` isLeft
        parseBuiltinDataText "{1:" `shouldSatisfy` isLeft

      it "rejects invalid syntax" do
        parseBuiltinDataText "abc" `shouldSatisfy` isLeft
        parseBuiltinDataText "0[42]" `shouldSatisfy` isLeft
        parseBuiltinDataText "{1 2}" `shouldSatisfy` isLeft

-- Use isLeft from Relude/Prelude
