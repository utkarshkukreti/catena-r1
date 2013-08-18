module Catena.Stdlib(
  stdlib
) where

import Catena
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding(head)

stdlib :: Map String (State -> EvalResult)
stdlib = Map.fromList [
    ("+", iii (+)),
    ("-", iii (-)),
    ("*", iii (*)),
    ("/", iii div),
    ("%", iii rem),
    ("^", iii (^)),
    ("++", lll (++)),
    ("pop", pop),
    ("dup", dup),
    ("swap", swap),
    ("apply", apply)
  ]

iii :: (Integer -> Integer -> Integer) -> State -> EvalResult
iii f = onlyStack 2 f'
          where
            f' [Integer y, Integer x] = Just [Integer $ f x y]
            f' _                      = Nothing

lll :: ([Token] -> [Token] -> [Token]) -> State -> EvalResult
lll f = onlyStack 2 f'
          where
            f' [List y, List x] = Just [List $ f x y]
            f' _                = Nothing

pop :: State -> EvalResult
pop = onlyStack 1 $ \[_] -> Just []

dup :: State -> EvalResult
dup = onlyStack 1 $ \[x] -> Just [x, x]

swap :: State -> EvalResult
swap = onlyStack 2 $ \[y, x] -> Just [x, y]

apply :: State -> EvalResult
apply state@State{queue = _queue, stack = _stack} = case take 1 _stack of
               [List x] -> Right state { queue = x ++ _queue, stack = drop 1 _stack }
               []       -> Left $ NotEnoughArgumentsError 1 0
               _        -> Left ArgumentError

onlyStack :: Int -> ([Token] -> Maybe [Token]) -> State -> EvalResult
onlyStack i f state@State{stack = _stack}
  | length _stack < i = Left $ NotEnoughArgumentsError i $ length _stack
  | otherwise         = case f head of
                          Just xs -> Right state { stack = xs ++ rest }
                          Nothing -> Left ArgumentError
                        where
                          (head, rest) = splitAt i _stack
