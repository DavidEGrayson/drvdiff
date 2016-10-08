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

-- Each line contains 1 or more cells, separated by a comma
aterm :: GenParser Char st ATerm
aterm = constructor <|> constant

constant :: GenParser Char st ATerm
constant = do
  name <- many1 alphaNum
  return $ Constant name

constructor :: GenParser Char st ATerm
constructor = do
  Constant name <- constant
  Tuple args <- tuple
  return $ Constructor name args

tuple :: GenParser Char st ATerm
tuple = do
  contents <- between (char '(') (char ')') (aterm `sepBy` (char ','))
  return $ Tuple contents

parseAterm :: String -> String -> Either ParseError ATerm
parseAterm = parse (aterm <* eof)

