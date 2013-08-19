module Catena.Stdlib(
  stdlib
) where

import Catena
import Data.Map (Map)
import qualified Data.Map as Map

stdlib :: Map String (State -> Result)
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

iii :: (Integer -> Integer -> Integer) -> State -> Result
iii f = onlyStack 2 f'
          where
            f' [Integer y, Integer x] = Just [Integer $ f x y]
            f' _                      = Nothing

lll :: ([Token] -> [Token] -> [Token]) -> State -> Result
lll f = onlyStack 2 f'
          where
            f' [List y, List x] = Just [List $ f x y]
            f' _                = Nothing

pop :: State -> Result
pop = onlyStack 1 $ \[_] -> Just []

dup :: State -> Result
dup = onlyStack 1 $ \[x] -> Just [x, x]

swap :: State -> Result
swap = onlyStack 2 $ \[y, x] -> Just [x, y]

apply :: State -> Result
apply state@State{queue = _queue, stack = _stack} = case left of
        [List x] -> Right state { queue = x ++ _queue, stack = right }
        []       -> Left $ NotEnoughArgumentsError 1 0
        _        -> Left ArgumentError
        where
          (left, right) = splitAt 1 _stack

onlyStack :: Int -> ([Token] -> Maybe [Token]) -> State -> Result
onlyStack i f state@State{stack = _stack}
  | length _stack < i = Left $ NotEnoughArgumentsError i $ length _stack
  | otherwise         = case f left of
                          Just xs -> Right state { stack = xs ++ right }
                          Nothing -> Left ArgumentError
                        where
                          (left, right) = splitAt i _stack
