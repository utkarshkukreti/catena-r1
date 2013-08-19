module Catena (
  AST(..),
  State(..),
  Error(..),
  Result,
  showStack,
  showQueue
) where

import Data.List (intercalate)

data AST = Integer Integer
           | String String
           | Atom String
           | List [AST]
             deriving (Eq)

data State = State { stack :: [AST], queue :: [AST] }
             deriving (Eq, Show)

data Error = NotFoundError String
           | ParseError String
           | ArgumentError
           | NotEnoughArgumentsError Int Int
             deriving (Eq, Show)

type Result = Either Error State

instance Show (AST) where
  show (Integer x) = show x
  show (String x)  = show x
  show (Atom x)    = x
  show (List xs)   = "[" ++ intercalate ", " (map show xs) ++ "]"

showStack :: [AST] -> String
showStack = show . List . reverse

showQueue :: [AST] -> String
showQueue = showStack
