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
      it "works for Integer Integer" $ do
        "1 2 +" `shouldEvaluateTo` Right "[3]"
        "0 1 2 3 4 + - -" `shouldEvaluateTo` Right "[0, 6]"
        "2 3 ^ 6 - 10 * 3 / 17 13 %" `shouldEvaluateTo` Right "[6, 4]"

      it "returns ArgumentError when either argument is not an Integer" $ do
        "2 +" `shouldEvaluateTo` Left ArgumentError
        "2 \"3\" +" `shouldEvaluateTo` Left ArgumentError

    describe "++" $ do
      it "works for List List" $ do
        "[1 2] [] [] [] ++ ++" `shouldEvaluateTo` Right "[[1, 2], []]"
        "[1 2] [] [3 4] ++ ++" `shouldEvaluateTo` Right "[[1, 2, 3, 4]]"

      it "otherwise returns ArgumentError" $ do
        "[] ++" `shouldEvaluateTo` Left ArgumentError
        "[1] 2 ++" `shouldEvaluateTo` Left ArgumentError

    describe "pop" $ do
      it "works when atleast one value is in stack" $ do
        "1 2 3 pop pop 5 pop" `shouldEvaluateTo` Right "[1]"

      it "otherwise returns ArgumentError" $ do
        "pop" `shouldEvaluateTo` Left ArgumentError
        "1 pop pop" `shouldEvaluateTo` Left ArgumentError

    describe "dup" $ do
      it "works when atleast one value is in stack" $ do
        "1 2 3 dup dup 5 dup" `shouldEvaluateTo` Right "[1, 2, 3, 3, 3, 5, 5]"

      it "otherwise returns ArgumentError" $ do
        "dup" `shouldEvaluateTo` Left ArgumentError

    describe "apply" $ do
      it "works for List" $ do
        "1 2 [+] apply" `shouldEvaluateTo` Right "[3]"
        "1 1 [dup + dup [+] apply] apply" `shouldEvaluateTo` Right "[1, 4]"

      it "otherwise returns ArgumentError" $ do
        "apply" `shouldEvaluateTo` Left ArgumentError
        "1 apply" `shouldEvaluateTo` Left ArgumentError

    describe "swap" $ do
      it "works when atleast two values are in the stack" $ do
        "1 4 7 swap - swap -" `shouldEvaluateTo` Right "[2]"

      it "otherwise returns ArgumentError" $ do
        "swap" `shouldEvaluateTo` Left ArgumentError
        "1 swap" `shouldEvaluateTo` Left ArgumentError

shouldEvaluateTo :: String -> Either EvalError String -> Expectation
shouldEvaluateTo x y = show' (evalString x) `shouldBe` y
                       where
                         show' (Left err) = Left err
                         show' (Right state) = Right $ showStack $ stack state
