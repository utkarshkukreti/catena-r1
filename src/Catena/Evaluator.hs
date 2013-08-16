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
                 Left message -> Left $ "Parse Error: " ++ message
                 Right ts -> Right $ foldl eval1 defaultState ts

eval1 :: State -> Token -> State
eval1 state token = case token of
                      String _ -> state { stack = token:(stack state) }
                      Integer _ -> state { stack = token:(stack state) }
                      Block _ -> state { stack = token:(stack state) }
                      Atom a -> case Map.lookup a builtins of
                                  Just f -> f state
                                  Nothing -> error $
                                               "Atom \"" ++ a ++ "\" not found!"

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
