module Data.Regex where

import Data.Parser (Parser(..))

data Regex = String
  | Many Regex
  | StringIn (Array Regex)
  | Optional Regex
  | OneOf Regex Regex

{--regexParser :: Regex -> Parser String--}
{--regexParser String = --}
