module Catena.Parser (
  AST(..),
  parse,
  parse1
) where

import Catena
import Control.Applicative (many, (<|>), (*>), (<*))
import Data.Attoparsec.Text (Parser, char, decimal, many1, notChar, notInClass,
                             parseOnly, satisfy, signed, skipSpace)
import Data.String (fromString)

parse :: String -> Either String [AST]
parse = parseOnly (many1 (root <* skipSpace)) . fromString

parse1 :: String -> Either String AST
parse1 = parseOnly root . fromString

root :: Parser AST
root = integer <|> string <|> atom <|> list

integer :: Parser AST
integer = fmap Integer $ signed decimal

string :: Parser AST
string = fmap String $ char '"' *> many (notChar '"') <* char '"'

atom :: Parser AST
atom = fmap Atom $ many1 $ satisfy $ notInClass " \t\n\r[]"

list :: Parser AST
list = fmap List $ open *> inside <* close
  where
    open   = char '[' >> skipSpace
    inside = many $ root <* skipSpace
    close  = char ']'
