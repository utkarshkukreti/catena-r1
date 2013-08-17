module Catena.Evaluator (
  State(..),
  defaultState,
  eval,
  eval1,
  evalString
) where

import Catena
import Catena.Parser
import Catena.Stdlib
import qualified Data.Map as Map

defaultState :: State
defaultState = State { stack = Stack [] }

evalString :: String -> EvalResult
evalString s = case parse s of
                 Left err -> Left $ ParseError err
                 Right tokens -> eval defaultState tokens

eval :: State -> [Token] -> EvalResult
eval state [] = Right state
eval state (x:xs) = case eval1 state x of
                      Left err -> Left err
                      Right newState -> eval newState xs

eval1 :: State -> Token -> EvalResult
eval1 state (Atom "apply") = eval (state { stack = Stack xs }) x
                            where Stack ((List x):xs) = stack state
eval1 state (Atom name) = case Map.lookup name stdlib of
                            Just f -> f state
                            Nothing -> Left $ NotFoundError name
eval1 state token = Right state { stack = Stack (token:xs) }
                    where
                      Stack xs = stack state
