module Catena.StdlibSpec (main, spec) where

import Test.Hspec
import Catena.Evaluator
import Catena.Parser (Token(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Stdlib" $ do
    describe "+ - * / ^" $ do
      it "evaluates them correctly" $ do
        evalString "1 2 +" `shouldBe` Right State { stack = [Integer 3] }
        evalString "0 1 2 3 4 + - -" `shouldBe`
          Right State { stack = [Integer 6, Integer 0] }
        evalString "2 3 ^ 6 - 10 * 3 / 17 13 %" `shouldBe`
          Right State { stack = [Integer 4, Integer 6] }
