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
        parse1 "0" `shouldBe` Right (Integer 0)
        parse1 "14159" `shouldBe` Right (Integer 14159)

      it "parses negative integers" $ do
        parse1 "-0" `shouldBe` Right (Integer 0)
        parse1 "-26535" `shouldBe` Right (Integer $ -26535)

    context "Strings" $ do
      it "parses simple strings" $ do
        parse1 "\"\"" `shouldBe` Right (String "")
        parse1 "\"hello\"" `shouldBe` Right (String "hello")
        parse1 "\" world ! \"" `shouldBe` Right (String " world ! ")

    context "Atoms" $ do
      it "parses everything except square brackets and whitespace" $ do
        parse1 "+" `shouldBe` Right (Atom "+")
        parse1 "++--" `shouldBe` Right (Atom "++--")
        parse1 "add!!?" `shouldBe` Right (Atom "add!!?")

    context "Blocks" $ do
      it "parses empty blocks" $ do
        parse1 "[]" `shouldBe` Right (Block [])
        parse1 "[  ]" `shouldBe` Right (Block [])

      it "parses non-empty blocks" $ do
        parse1 "[1 2]" `shouldBe` Right (Block [Integer 1, Integer 2])
        parse1 "[ 1  2  ]" `shouldBe` Right (Block [Integer 1, Integer 2])

      it "parses recursive blocks" $ do
        parse1 "[1 [[2]] [\"abc\"]]" `shouldBe`
          Right (Block [Integer 1,
                        Block [Block [Integer 2]],
                        Block [String "abc"]])

    context "Root" $ do
      it "parses multiple expressions delimited by whitespace" $ do
        parse "1 2 + [!] ^_^" `shouldBe`
          Right [Integer 1, Integer 2, Atom "+", Block [Atom "!"], Atom "^_^"]
