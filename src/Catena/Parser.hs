module Catena.Parser (
  Token(..),
  parse
) where

import Data.Attoparsec.Text (Parser, decimal, parseOnly, signed)
import Data.String (fromString)

data Token = IntegerT Integer
  deriving (Eq, Show)

parse :: String -> Either String Token
parse = parseOnly root . fromString

root :: Parser Token
root = integer

integer :: Parser Token
integer = fmap IntegerT $ signed decimal
