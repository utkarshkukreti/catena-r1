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

evalString :: String -> Either String State
evalString s = case parse s of
                 Left message -> Left $ "Parse Error: " ++ message ++ "!"
                 Right ts -> eval defaultState ts

eval :: State -> [Token] -> Either String State
eval state [] = Right state
eval state (x:xs) = case eval1 state x of
                      Left message -> Left message
                      Right newState -> eval newState xs

eval1 :: State -> Token -> Either String State
eval1 state (Atom name) = case Map.lookup name stdlib of
                            Just f -> f state
                            Nothing -> Left $
                                         "Atom \"" ++ name ++ "\" not found!"
eval1 state token = Right state { stack = Stack (token:xs) }
                    where
                      Stack xs = stack state
