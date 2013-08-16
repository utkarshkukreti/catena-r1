module Catena (
  Token(..),
  Stack(..),
  State(..)
) where

import Data.List (intercalate)

data Token = Integer Integer
           | String String
           | Atom String
           | List [Token]
             deriving (Eq)

data Stack = Stack [Token]
             deriving (Eq)

data State = State { stack :: Stack }
             deriving (Eq, Show)

instance Show (Token) where
  show (Integer x) = show x
  show (String x) = show x
  show (Atom x) = x
  show (List xs) = "[" ++ (intercalate ", " (map show xs)) ++ "]"

instance Show (Stack) where
  show (Stack xs) = "[" ++ (intercalate ", " (map show $ reverse xs)) ++ "]"
