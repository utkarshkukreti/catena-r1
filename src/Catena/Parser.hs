module Catena.Parser (
  Token(..),
  parse
) where

import Data.Attoparsec.Text (Parser, char, decimal, many1, notChar, notInClass,
                             parseOnly, satisfy, signed, skipSpace)
import Control.Applicative (many, (<|>), (*>), (<*))
import Data.String (fromString)

data Token = Integer Integer
           | String String
           | Atom String
           | Block [Token]
             deriving (Eq, Show)

parse :: String -> Either String Token
parse = parseOnly root . fromString

root :: Parser Token
root = integer <|> string <|> atom <|> block

integer :: Parser Token
integer = fmap Integer $ signed decimal

string :: Parser Token
string = fmap String $ char '"' *> (many $ notChar '"') <* char '"'

atom :: Parser Token
atom = fmap Atom $ many1 $ satisfy $ notInClass " \t\n\r[]"

block :: Parser Token
block = fmap Block $ open *> inside <* close
  where
    open = char '[' >> skipSpace
    inside = many $ root <* skipSpace
    close = char ']'
