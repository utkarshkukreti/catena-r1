module Catena.EvaluatorSpec (main, spec) where

import Test.Hspec
import Catena
import Catena.Evaluator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Evaluator" $ do
    it "pushes Strings, Integers, and Lists to the stack" $ do
      evalString "\"b@\" 1 [a!]" `shouldBe`
        Right State { stack = [List [Atom "a!"], Integer 1, String "b@"]
                    , queue = [] }

    describe "evalString" $ do
      context "errors" $ do
        it "returns ParseError on parse error" $ do
          evalString "[" `shouldBe` (Left $ ParseError "not enough input")

        it "returns a Left on invalid Atom access" $ do
          evalString "abc" `shouldBe` (Left $ NotFoundError "abc")
