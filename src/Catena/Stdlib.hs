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
    ("swap", swap)
  ]

iii :: (Integer -> Integer -> Integer) -> State -> EvalResult
iii f state = onlyStack 2 state f'
                where
                  f' [Integer y, Integer x] = Just [Integer $ f x y]
                  f' _                      = Nothing

lll :: ([Token] -> [Token] -> [Token]) -> State -> EvalResult
lll f state = onlyStack 2 state f'
                where
                  f' [List y, List x] = Just [List $ f x y]
                  f' _                = Nothing

pop :: State -> EvalResult
pop state = onlyStack 1 state f'
            where
              f' [_] = Just []
              f' _   = Nothing

dup :: State -> EvalResult
dup state = onlyStack 1 state f'
            where
              f' [x] = Just [x, x]
              f' _   = Nothing

swap :: State -> EvalResult
swap state = onlyStack 2 state f'
             where
               f' [y, x] = Just [x, y]
               f' _      = Nothing

onlyStack :: Int -> State -> ([Token] -> Maybe [Token]) -> EvalResult
onlyStack i state f = case f head of
                        Just xs -> Right state { stack = Stack (xs ++ rest) }
                        Nothing -> Left ArgumentError
                      where
                        (head, rest) = splitAt i xs
                          where Stack xs = stack state
