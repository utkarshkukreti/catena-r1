module Catena.Evaluator (
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
defaultState = State { stack = [], queue = [] }

evalString :: String -> Result
evalString s = case parse s of
                 Left err     -> Left $ ParseError err
                 Right tokens -> eval defaultState { queue = tokens }

eval :: State -> Result
eval state@State {queue = []} = Right state
eval state = case eval1 state of
               Left err       -> Left err
               Right newState -> eval newState

eval1 :: State -> Result
eval1 state@State{queue = []} = Right state
eval1 state@State{queue = (q:qs), stack = _stack} = case q of
  Atom name    -> case Map.lookup name stdlib of
                    Just f  -> f state { queue = qs }
                    Nothing -> Left $ NotFoundError name
  _            -> Right $ state { queue = qs, stack = q:_stack }
