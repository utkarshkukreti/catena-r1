module Catena.Evaluator (
  State(..),
  evalString
) where

import Catena.Parser
import Data.Map (Map)
import qualified Data.Map as Map

data State = State { stack :: [Token] }
             deriving (Eq, Show)

defaultState :: State
defaultState = State { stack = [] }

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
eval1 state (Atom a) = case Map.lookup a builtins of
                         Just f -> Right $ f state
                         Nothing -> Left $ "Atom \"" ++ a ++ "\" not found!"
eval1 state token = Right state { stack = token:(stack state) }

builtins :: Map String (State -> State)
builtins = Map.fromList [
    ("+", iii (+)),
    ("-", iii (-)),
    ("*", iii (*)),
    ("/", iii div),
    ("^", iii (^))
  ]

  where
    iii f state@State{stack = y:x:xs} = case (x, y) of
      (Integer x, Integer y) -> state { stack = (Integer $ f x y): xs}
      _                      -> error "Can only apply this to two Integers"
