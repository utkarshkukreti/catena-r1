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

      it "otherwise returns an error" $ do
        "2 +" `shouldEvaluateTo` Left (NotEnoughArgumentsError 2 1)
        "2 \"3\" +" `shouldEvaluateTo` Left ArgumentError

    describe "++" $ do
      it "works for List List" $ do
        "[1 2] [] [] [] ++ ++" `shouldEvaluateTo` Right "[[1, 2], []]"
        "[1 2] [] [3 4] ++ ++" `shouldEvaluateTo` Right "[[1, 2, 3, 4]]"

      it "otherwise returns an error" $ do
        "++" `shouldEvaluateTo` Left (NotEnoughArgumentsError 2 0)
        "[] ++" `shouldEvaluateTo` Left (NotEnoughArgumentsError 2 1)
        "[1] 2 ++" `shouldEvaluateTo` Left ArgumentError

    describe "pop" $ do
      it "works when atleast one value is in stack" $ do
        "1 2 3 pop pop 5 pop" `shouldEvaluateTo` Right "[1]"

      it "otherwise returns an error" $ do
        "pop" `shouldEvaluateTo` Left (NotEnoughArgumentsError 1 0)
        "1 pop pop" `shouldEvaluateTo` Left (NotEnoughArgumentsError 1 0)

    describe "dup" $ do
      it "works when atleast one value is in stack" $ do
        "1 2 3 dup dup 5 dup" `shouldEvaluateTo` Right "[1, 2, 3, 3, 3, 5, 5]"

      it "otherwise returns an error" $ do
        "dup" `shouldEvaluateTo` Left (NotEnoughArgumentsError 1 0)

    describe "apply" $ do
      it "works for List" $ do
        "1 2 [+] apply" `shouldEvaluateTo` Right "[3]"
        "1 1 [dup + dup [+] apply] apply" `shouldEvaluateTo` Right "[1, 4]"

      it "otherwise returns an error" $ do
        "apply" `shouldEvaluateTo` Left (NotEnoughArgumentsError 1 0)
        "1 apply" `shouldEvaluateTo` Left ArgumentError

    describe "swap" $ do
      it "works when atleast two values are in the stack" $ do
        "1 4 7 swap - swap -" `shouldEvaluateTo` Right "[2]"

      it "otherwise returns an error" $ do
        "swap" `shouldEvaluateTo` Left (NotEnoughArgumentsError 2 0)
        "1 swap" `shouldEvaluateTo` Left (NotEnoughArgumentsError 2 1)

shouldEvaluateTo :: String -> Either Error String -> Expectation
shouldEvaluateTo x y = show' (evalString x) `shouldBe` y
                       where
                         show' (Left err) = Left err
                         show' (Right state) = Right $ showStack $ stack state
