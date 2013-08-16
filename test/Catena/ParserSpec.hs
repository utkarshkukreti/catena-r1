module Catena.ParserSpec (main, spec) where

import Test.Hspec
import Catena.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" $ do
    context "Integers" $ do
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

    context "Atoms" $ do
      it "parses everything except square brackets and whitespace" $ do
        parse "+" `shouldBe` Right (Atom "+")
        parse "++--" `shouldBe` Right (Atom "++--")
        parse "add!!?" `shouldBe` Right (Atom "add!!?")

    context "Blocks" $ do
      it "parses empty blocks" $ do
        parse "[]" `shouldBe` Right (Block [])
        parse "[  ]" `shouldBe` Right (Block [])

      it "parses non-empty blocks" $ do
        parse "[1 2]" `shouldBe` Right (Block [Integer 1, Integer 2])
        parse "[ 1  2  ]" `shouldBe` Right (Block [Integer 1, Integer 2])

      it "parses recursive blocks" $ do
        parse "[1 [[2]] [\"abc\"]]" `shouldBe`
          Right (Block [Integer 1,
                        Block [Block [Integer 2]],
                        Block [String "abc"]])
