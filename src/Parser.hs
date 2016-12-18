-- http://www.program-transformation.org/Tools/ATermFormat
-- Does not support annotated terms, reals, blobs, or ints yet.

module Parser (parseAterm, ParseError, list) where

import Aterm
import Text.ParserCombinators.Parsec hiding (ParseError)
import qualified Text.ParserCombinators.Parsec

type ParseError = Text.ParserCombinators.Parsec.ParseError

parseAterm :: String -> String -> Either ParseError Aterm
parseAterm = parse (aterm <* (optional (char '\n')) <* eof)

aterm :: Parser Aterm
aterm = constructor <|> constant <|> tuple <|> list <|> quotedString

constant :: Parser Aterm
constant = do
  name <- many1 alphaNum
  return $ Constant name

constructor :: Parser Aterm
constructor = do
  Constant name <- constant
  Tuple args <- tuple
  return $ Constructor name args

tuple :: Parser Aterm
tuple = do
  contents <- between (char '(') (char ')') (aterm `sepBy` (char ','))
  return $ Tuple contents

list :: Parser Aterm
list = do
  contents <- between (char '[') (char ']') (aterm `sepBy` (char ','))
  return $ List contents

quotedString :: Parser Aterm
quotedString = do
  str <- between quote quote (many quotedStringChar)
  return $ QuotedString str
  where
    quote = char '"'
    quotedStringChar = escapedChar <|> normalChar
    escapedChar = (char '\\') *> (oneOf ['\\', '"'])
    normalChar = noneOf "\""
