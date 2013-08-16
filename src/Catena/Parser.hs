module Catena.Parser (
  Token(..),
  parse
) where

import Data.Attoparsec.Text (Parser, char, decimal, notChar, parseOnly, signed)
import Control.Applicative (many, (<|>))
import Data.String (fromString)

data Token = Integer Integer
           | String String
             deriving (Eq, Show)

parse :: String -> Either String Token
parse = parseOnly root . fromString

root :: Parser Token
root = integer <|> string

integer :: Parser Token
integer = fmap Integer $ signed decimal

string :: Parser Token
string = do
  _ <- char '"'
  s <- many $ notChar '"'
  _ <- char '"'
  return $ String s
