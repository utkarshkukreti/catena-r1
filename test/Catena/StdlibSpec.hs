module Catena.StdlibSpec (main, spec) where

import Test.Hspec
import Catena
import Catena.Evaluator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Stdlib" $ do
    describe "+ - * / ^" $ do
      it "evaluates them correctly" $ do
        "1 2 +" `shouldSetStackTo` Right "[3]"
        "0 1 2 3 4 + - -" `shouldSetStackTo` Right "[0, 6]"
        "2 3 ^ 6 - 10 * 3 / 17 13 %" `shouldSetStackTo` Right "[6, 4]"

shouldSetStackTo x y = show' (evalString x) `shouldBe` y
                       where
                         show' (Left err) = Left err
                         show' (Right state) = Right $ show $ stack state
