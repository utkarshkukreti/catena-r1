module Catena.StdlibSpec (main, spec) where

import Test.Hspec
import Catena
import Catena.Evaluator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Stdlib" $ do
    describe "+ - * / % ^" $ do
      it "evaluates them correctly" $ do
        "1 2 +" `shouldSetStackTo` Right "[3]"
        "0 1 2 3 4 + - -" `shouldSetStackTo` Right "[0, 6]"
        "2 3 ^ 6 - 10 * 3 / 17 13 %" `shouldSetStackTo` Right "[6, 4]"

    describe "++" $ do
      it "evaluates it correctly" $ do
        "[1 2] [] [] [] ++ ++" `shouldSetStackTo` Right "[[1, 2], []]"
        "[1 2] [] [3 4] ++ ++" `shouldSetStackTo` Right "[[1, 2, 3, 4]]"

    describe "pop" $ do
      it "pops a value from the stack" $ do
        "1 2 3 pop pop 5 pop" `shouldSetStackTo` Right "[1]"

    describe "dup" $ do
      it "duplicates value on the top of the stack" $ do
        "1 2 3 dup dup 5 dup" `shouldSetStackTo` Right "[1, 2, 3, 3, 3, 5, 5]"

shouldSetStackTo :: String -> Either String String -> Expectation
shouldSetStackTo x y = show' (evalString x) `shouldBe` y
                       where
                         show' (Left err) = Left err
                         show' (Right state) = Right $ show $ stack state
