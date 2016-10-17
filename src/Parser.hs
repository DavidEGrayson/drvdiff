-- http://www.program-transformation.org/Tools/ATermFormat
-- Does not support annotated terms, reals, or blobs yet.

module Parser where

import Text.ParserCombinators.Parsec hiding (ParseError)
import qualified Text.ParserCombinators.Parsec

type ParseError = Text.ParserCombinators.Parsec.ParseError

data ATerm = Constant String
           | Constructor String [ATerm]
           | Tuple [ATerm]
           | List [ATerm]
           | QuotedString String
           | Integer Int
  deriving (Show)

parseAterm :: String -> String -> Either ParseError ATerm
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
