module Cape.Data.PlutusJsonSpec (spec) where

import Prelude

import Cape.Data.PlutusJson (encodePlutusJsonData, parsePlutusJsonData)
import Data.Aeson qualified as Json
import Data.ByteString qualified as BS
import PlutusCore.Data (Data (..))
import Test.Hspec

parseString :: ByteString -> Either Text Data
parseString s = case Json.eitherDecodeStrict s of
  Left err -> Left ("decode error: " <> toText err)
  Right v -> parsePlutusJsonData v

spec :: Spec
spec = do
  describe "parsePlutusJsonData" do
    it "parses integers" do
      parseString "{\"int\": 42}" `shouldBe` Right (I 42)
      parseString "{\"int\": -7}" `shouldBe` Right (I (-7))

    it "parses bytestrings" do
      parseString "{\"bytes\": \"\"}" `shouldBe` Right (B BS.empty)
      parseString "{\"bytes\": \"deadbeef\"}"
        `shouldBe` Right (B (BS.pack [0xde, 0xad, 0xbe, 0xef]))

    it "rejects odd-length hex in bytes" do
      parseString "{\"bytes\": \"abc\"}" `shouldSatisfy` isLeft

    it "parses constructors" do
      parseString "{\"constructor\": 0, \"fields\": []}"
        `shouldBe` Right (Constr 0 [])
      parseString
        "{\"constructor\": 1, \"fields\": [{\"int\": 42}, {\"bytes\": \"ab\"}]}"
        `shouldBe` Right (Constr 1 [I 42, B (BS.pack [0xab])])

    it "parses lists" do
      parseString "{\"list\": []}" `shouldBe` Right (List [])
      parseString "{\"list\": [{\"int\": 1}, {\"int\": 2}]}"
        `shouldBe` Right (List [I 1, I 2])

    it "parses maps" do
      parseString "{\"map\": []}" `shouldBe` Right (Map [])
      parseString
        "{\"map\": [{\"k\": {\"int\": 1}, \"v\": {\"bytes\": \"ab\"}}]}"
        `shouldBe` Right (Map [(I 1, B (BS.pack [0xab]))])

    it "rejects objects with no recognised key" do
      parseString "{\"foo\": 42}" `shouldSatisfy` isLeft

    it "rejects objects with multiple discriminator keys" do
      parseString "{\"int\": 42, \"bytes\": \"ab\"}" `shouldSatisfy` isLeft
      parseString "{\"list\": [], \"map\": []}" `shouldSatisfy` isLeft

    it "rejects valid discriminator alongside an unknown key" do
      parseString "{\"int\": 42, \"extra\": true}" `shouldSatisfy` isLeft
      parseString "{\"bytes\": \"ab\", \"junk\": null}" `shouldSatisfy` isLeft

    it "rejects constructor without fields and vice versa" do
      parseString "{\"constructor\": 0}" `shouldSatisfy` isLeft
      parseString "{\"fields\": []}" `shouldSatisfy` isLeft

    it "rejects constructor + fields with extra keys" do
      parseString "{\"constructor\": 0, \"fields\": [], \"extra\": null}"
        `shouldSatisfy` isLeft

    it "rejects non-object inputs" do
      parseString "42" `shouldSatisfy` isLeft
      parseString "\"hello\"" `shouldSatisfy` isLeft
      parseString "[1, 2, 3]" `shouldSatisfy` isLeft

  describe "encodePlutusJsonData" do
    it "round-trips through parsePlutusJsonData" do
      let samples =
            [ I 0
            , I 42
            , I (-1)
            , B BS.empty
            , B (BS.pack [0xde, 0xad])
            , Constr 0 []
            , Constr 1 [I 42, B (BS.pack [0xab])]
            , List []
            , List [I 1, I 2]
            , Map [(I 1, B (BS.pack [0xab]))]
            , Constr 0
                [ List [I 1, I 2]
                , Map [(I 1, B (BS.pack [0xab]))]
                , Constr 2 []
                ]
            ]
      mapM_
        ( \d ->
            parsePlutusJsonData (encodePlutusJsonData d) `shouldBe` Right d
        )
        samples
