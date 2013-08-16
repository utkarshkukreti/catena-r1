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
