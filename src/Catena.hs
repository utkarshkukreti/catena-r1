module Catena (
  State(..),
  Token(..)
) where

data Token = Integer Integer
           | String String
           | Atom String
           | List [Token]
             deriving (Eq, Show)

data State = State { stack :: [Token] }
             deriving (Eq, Show)
