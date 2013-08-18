module Catena (
  Token(..),
  State(..),
  Error(..),
  Result,
  showStack,
  showQueue
) where

import Data.List (intercalate)

data Token = Integer Integer
           | String String
           | Atom String
           | List [Token]
             deriving (Eq)

data State = State { stack :: [Token], queue :: [Token] }
             deriving (Eq, Show)

data Error = NotFoundError String
           | ParseError String
           | ArgumentError
           | NotEnoughArgumentsError Int Int
             deriving (Eq, Show)

type Result = Either Error State

instance Show (Token) where
  show (Integer x) = show x
  show (String x)  = show x
  show (Atom x)    = x
  show (List xs)   = "[" ++ intercalate ", " (map show xs) ++ "]"

showStack :: [Token] -> String
showStack = show . List . reverse

showQueue :: [Token] -> String
showQueue = showStack
