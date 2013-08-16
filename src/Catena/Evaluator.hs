module Catena.Evaluator (
  State(..),
  evalString
) where

import Catena.Parser

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
                      Atom _ -> error "cannot evaluate Atoms yet"
