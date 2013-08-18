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
import Prelude hiding (head, tail)

defaultState :: State
defaultState = State { stack = [], queue = [] }

evalString :: String -> EvalResult
evalString s = case parse s of
                 Left err     -> Left $ ParseError err
                 Right tokens -> eval defaultState { queue = tokens }

eval :: State -> EvalResult
eval state@State {queue = []} = Right state
eval state = case eval1 state of
               Left err       -> Left err
               Right newState -> eval newState

eval1 :: State -> EvalResult
eval1 state@State{queue = (head:tail), stack = _stack} = case head of
  Atom name    -> case Map.lookup name stdlib of
                    Just f  -> f state { queue = tail }
                    Nothing -> Left $ NotFoundError name
  _            -> Right $ state { queue = tail, stack = head:_stack }
