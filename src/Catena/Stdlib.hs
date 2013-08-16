module Catena.Stdlib(
  stdlib
) where

import Catena
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding(head)

stdlib :: Map String (State -> Either String State)
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

iii :: (Integer -> Integer -> Integer) -> State -> Either String State
iii f state = onlyStack 2 state f'
                where
                  f' [Integer y, Integer x] = Just [Integer $ f x y]
                  f' _                      = Nothing

lll :: ([Token] -> [Token] -> [Token]) -> State -> Either String State
lll f state = onlyStack 2 state f'
                where
                  f' [List y, List x] = Just [List $ f x y]
                  f' _                = Nothing

pop :: State -> Either String State
pop state = onlyStack 1 state f'
            where
              f' [_] = Just []
              f' _   = Nothing

dup :: State -> Either String State
dup state = onlyStack 1 state f'
            where
              f' [x] = Just [x, x]
              f' _   = Nothing

swap :: State -> Either String State
swap state = onlyStack 2 state f'
             where
               f' [y, x] = Just [x, y]
               f' _      = Nothing

onlyStack :: Int -> State -> ([Token] -> Maybe [Token]) -> Either String State
onlyStack i state f = case f head of
                        Just xs -> Right state { stack = Stack (xs ++ rest) }
                        Nothing -> Left "Invalid Arguments"
                      where
                        (head, rest) = splitAt i xs
                          where Stack xs = stack state
