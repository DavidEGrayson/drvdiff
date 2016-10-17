module Aterm where

data Aterm = Constant String
           | Constructor String [Aterm]
           | Tuple [Aterm]
           | List [Aterm]
           | QuotedString String
           | Integer Int
  deriving (Show)

