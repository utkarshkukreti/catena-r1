module Catena.Parser (
  Token(..),
  parse
) where

import Data.Attoparsec.Text (Parser, char, decimal, notChar, parseOnly, signed)
import Control.Applicative (many, (<|>))
import Data.String (fromString)

data Token = IntegerT Integer
           | StringT String
             deriving (Eq, Show)

parse :: String -> Either String Token
parse = parseOnly root . fromString

root :: Parser Token
root = integer <|> string

integer :: Parser Token
integer = fmap IntegerT $ signed decimal

string :: Parser Token
string = do
  char '"'
  s <- many $ notChar '"'
  char '"'
  return $ StringT s
