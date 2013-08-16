module Catena.EvaluatorSpec (main, spec) where

import Test.Hspec
import Catena.Evaluator
import Catena.Parser (Token(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Evaluator" $ do
    it "pushes Strings, Integers, and Lists to the stack" $ do
      evalString "\"abc\" 1 [two]" `shouldBe`
        Right State { stack = [List [Atom "two"], Integer 1, String "abc"] }

    describe "evalString" $ do
      context "errors" $ do
        it "returns a Left on parse error" $ do
          evalString "[" `shouldBe` Left "Parse Error: not enough input!"

        it "returns a Left on invalid Atom access" $ do
          evalString "abc" `shouldBe` Left "Atom \"abc\" not found!"

        it "returns a Left on invalid arguments to builtins" $ do
          evalString "1 \"2\" +" `shouldBe` Left "Invalid Arguments"
          evalString "1 \"2\" *" `shouldBe` Left "Invalid Arguments"
          evalString "+" `shouldBe` Left "Invalid Arguments"
          evalString "2 +" `shouldBe` Left "Invalid Arguments"
