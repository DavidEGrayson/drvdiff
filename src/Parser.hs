-- http://www.program-transformation.org/Tools/ATermFormat
-- Does not support annotated terms, reals, blobs, or ints yet.

module Parser (parseAterm, ParseError) where

import Aterm
import Text.ParserCombinators.Parsec hiding (ParseError)
import qualified Text.ParserCombinators.Parsec

type ParseError = Text.ParserCombinators.Parsec.ParseError

parseAterm :: String -> String -> Either ParseError Aterm
parseAterm = parse (aterm <* (optional (char '\n')) <* eof)

aterm = constructor <|> constant <|> tuple <|> list <|> quotedString

constant = do
  name <- many1 alphaNum
  return $ Constant name

constructor = do
  Constant name <- constant
  Tuple args <- tuple
  return $ Constructor name args

tuple = do
  contents <- between (char '(') (char ')') (aterm `sepBy` (char ','))
  return $ Tuple contents

list = do
  contents <- between (char '[') (char ']') (aterm `sepBy` (char ','))
  return $ List contents

quotedString = do
  string <- between (char '"') (char '"') (many quotedStringChar)
  return $ QuotedString string
  where
    quotedStringChar = escapedChar <|> normalChar
    escapedChar = (char '\\') *> (oneOf ['\\', '"'])
    normalChar = noneOf "\""
