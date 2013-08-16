module Catena.Parser (
  Token(..),
  parse
) where

import Data.Attoparsec.Text (Parser, char, decimal, notChar, parseOnly, signed,
                             skipSpace)
import Control.Applicative (many, (<|>))
import Data.String (fromString)

data Token = Integer Integer
           | String String
           | Block [Token]
             deriving (Eq, Show)

parse :: String -> Either String Token
parse = parseOnly root . fromString

root :: Parser Token
root = integer <|> string <|> block

integer :: Parser Token
integer = fmap Integer $ signed decimal

string :: Parser Token
string = do
  _ <- char '"'
  s <- many $ notChar '"'
  _ <- char '"'
  return $ String s

block :: Parser Token
block = do
  _ <- char '['
  skipSpace
  xs <- many $ do
    x <- root
    skipSpace
    return x
  skipSpace
  _ <- char ']'
  return $ Block xs
