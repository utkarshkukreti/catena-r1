module Catena.Stdlib(
  stdlib
) where

import Catena
import Data.Map (Map)
import qualified Data.Map as Map

stdlib :: Map String (State -> Either String State)
stdlib = Map.fromList [
    ("+", iii (+)),
    ("-", iii (-)),
    ("*", iii (*)),
    ("/", iii div),
    ("^", iii (^))
  ]

iii :: (Integer -> Integer -> Integer) -> State -> Either String State
iii f state = case stack state of
                (Integer y:Integer x:xs) -> Right state {
                                              stack = (Integer $ f x y):xs }
                _                        -> Left "Invalid Arguments"
