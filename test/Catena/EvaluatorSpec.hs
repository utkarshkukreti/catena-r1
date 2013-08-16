module Catena.EvaluatorSpec (main, spec) where

import Test.Hspec
import Catena.Evaluator
import Catena.Parser (Token(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Evaluator" $ do
    it "pushes Strings, Integers, and Blocks to the stack" $ do
      evalString "\"abc\" 1 [two]" `shouldBe`
        Right State { stack = [Block [Atom "two"], Integer 1, String "abc"] }

    it "evaluates arithmetic builtins" $ do
      evalString "1 2 +" `shouldBe` Right State { stack = [Integer 3] }
      evalString "0 1 2 3 4 + - -" `shouldBe`
        Right State { stack = [Integer 6, Integer 0] }
      evalString "2 3 ^ 6 - 10 * 3 /" `shouldBe`
        Right State { stack = [Integer 6] }
