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
        parse "0" `shouldBe` Right (IntegerT 0)
        parse "14159" `shouldBe` Right (IntegerT 14159)

      it "parses negative integers" $ do
        parse "-0" `shouldBe` Right (IntegerT 0)
        parse "-26535" `shouldBe` Right (IntegerT $ -26535)

    context "Strings" $ do
      it "parses simple strings" $ do
        parse "\"\"" `shouldBe` Right (StringT "")
        parse "\"hello\"" `shouldBe` Right (StringT "hello")
        parse "\" world ! \"" `shouldBe` Right (StringT " world ! ")
