module Cape.Data.UplcTextSpec (spec) where

import Prelude

import Cape.Data.UplcText (parseUplcDataText, renderUplcDataText)
import Data.ByteString qualified as BS
import PlutusCore.Data (Data (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseUplcDataText" do
    it "parses integers" do
      parseUplcDataText "I 42" `shouldBe` Right (I 42)
      parseUplcDataText "I 0" `shouldBe` Right (I 0)
      parseUplcDataText "I -7" `shouldBe` Right (I (-7))

    it "parses bytestrings" do
      parseUplcDataText "B #" `shouldBe` Right (B BS.empty)
      parseUplcDataText "B #deadbeef"
        `shouldBe` Right (B (BS.pack [0xde, 0xad, 0xbe, 0xef]))
      parseUplcDataText "B #A1B2C3"
        `shouldBe` Right (B (BS.pack [0xa1, 0xb2, 0xc3]))

    it "parses constructors" do
      parseUplcDataText "Constr 0 []" `shouldBe` Right (Constr 0 [])
      parseUplcDataText "Constr 1 [I 42]" `shouldBe` Right (Constr 1 [I 42])
      parseUplcDataText "Constr 0 [I 1, B #cafe]"
        `shouldBe` Right (Constr 0 [I 1, B (BS.pack [0xca, 0xfe])])

    it "parses lists" do
      parseUplcDataText "List []" `shouldBe` Right (List [])
      parseUplcDataText "List [I 1, I 2, I 3]"
        `shouldBe` Right (List [I 1, I 2, I 3])

    it "parses maps" do
      parseUplcDataText "Map []" `shouldBe` Right (Map [])
      parseUplcDataText "Map [(I 1, B #ab), (I 2, B #cd)]"
        `shouldBe` Right (Map [(I 1, B (BS.pack [0xab])), (I 2, B (BS.pack [0xcd]))])

    it "parses nested constructors" do
      parseUplcDataText "Constr 0 [Constr 1 [I 2]]"
        `shouldBe` Right (Constr 0 [Constr 1 [I 2]])

    it "rejects junk input" do
      parseUplcDataText "" `shouldSatisfy` isLeft
      parseUplcDataText "0(1 2)" `shouldSatisfy` isLeft
      parseUplcDataText "[I 1]" `shouldSatisfy` isLeft

  describe "renderUplcDataText" do
    it "round-trips through parseUplcDataText" do
      let samples =
            [ I 0
            , I 42
            , I (-1)
            , B BS.empty
            , B (BS.pack [0xde, 0xad, 0xbe, 0xef])
            , Constr 0 []
            , Constr 1 [I 42, B (BS.pack [0xab])]
            , List []
            , List [I 1, I 2, I 3]
            , Map []
            , Map [(I 1, B (BS.pack [0xab])), (I 2, B (BS.pack [0xcd]))]
            , Constr 0
                [ List [I 1, I 2]
                , Map [(I 1, B (BS.pack [0xab]))]
                , Constr 2 []
                ]
            ]
      mapM_
        ( \d ->
            parseUplcDataText (renderUplcDataText d) `shouldBe` Right d
        )
        samples
