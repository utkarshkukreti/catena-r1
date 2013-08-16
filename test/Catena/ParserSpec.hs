module Catena.ParserSpec (main, spec) where

import Test.Hspec
import Catena.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" $ do
    context "Literals" $ do
      it "parses positive integers" $ do
        parse "0" `shouldBe` Right (Integer 0)
        parse "14159" `shouldBe` Right (Integer 14159)

      it "parses negative integers" $ do
        parse "-0" `shouldBe` Right (Integer 0)
        parse "-26535" `shouldBe` Right (Integer $ -26535)

    context "Strings" $ do
      it "parses simple strings" $ do
        parse "\"\"" `shouldBe` Right (String "")
        parse "\"hello\"" `shouldBe` Right (String "hello")
        parse "\" world ! \"" `shouldBe` Right (String " world ! ")
