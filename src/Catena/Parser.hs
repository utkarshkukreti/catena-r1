module Catena.Parser (
  Token(..),
  parse,
  parse1
) where

import Control.Applicative (many, (<|>), (*>), (<*))
import Data.Attoparsec.Text (Parser, char, decimal, many1, notChar, notInClass,
                             parseOnly, satisfy, signed, skipSpace)
import Data.String (fromString)

data Token = Integer Integer
           | String String
           | Atom String
           | List [Token]
             deriving (Eq, Show)

parse :: String -> Either String [Token]
parse = parseOnly (many1 (root <* skipSpace)) . fromString

parse1 :: String -> Either String Token
parse1 = parseOnly root . fromString

root :: Parser Token
root = integer <|> string <|> atom <|> list

integer :: Parser Token
integer = fmap Integer $ signed decimal

string :: Parser Token
string = fmap String $ char '"' *> (many $ notChar '"') <* char '"'

atom :: Parser Token
atom = fmap Atom $ many1 $ satisfy $ notInClass " \t\n\r[]"

list :: Parser Token
list = fmap List $ open *> inside <* close
  where
    open = char '[' >> skipSpace
    inside = many $ root <* skipSpace
    close = char ']'
