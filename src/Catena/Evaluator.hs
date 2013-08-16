module Catena.Evaluator (
  State(..),
  evalString
) where

import Catena.Parser
import Data.Map (Map)
import qualified Data.Map as Map

data State = State { stack :: [Token] }
             deriving (Eq, Show)

type Result = Either String State

defaultState :: State
defaultState = State { stack = [] }

evalString :: String -> Result
evalString s = case parse s of
                 Left message -> Left $ "Parse Error: " ++ message ++ "!"
                 Right ts -> eval defaultState ts

eval :: State -> [Token] -> Result
eval state [] = Right state
eval state (x:xs) = case eval1 state x of
                      Left message -> Left message
                      Right newState -> eval newState xs


eval1 :: State -> Token -> Result
eval1 state (Atom a) = case Map.lookup a builtins of
                         Just f -> f state
                         Nothing -> Left $ "Atom \"" ++ a ++ "\" not found!"
eval1 state token = Right state { stack = token:(stack state) }

builtins :: Map String (State -> Result)
builtins = Map.fromList [
    ("+", iii "+" (+)),
    ("-", iii "-" (-)),
    ("*", iii "*" (*)),
    ("/", iii "/" div),
    ("^", iii "^" (^))
  ]

  where
    iii name f state = case stack state of
      (Integer y:Integer x:xs) -> Right state { stack = (Integer $ f x y): xs}
      (_:_:_)                  -> Left $ "Invalid stack for function \"" ++
                                       name ++ "\"!"
      _                        -> Left $ "Stack must have atleast 2 values!"
