module Data.Parser where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Int (fromString) as Int
import Data.Maybe (fromMaybe, maybe)
import Data.String (length) as Str
import Data.String.CodeUnits (fromCharArray, singleton, splitAt, uncons)

data ParseError = UnexpectedChar Char
    | UnexpectedString String
    | UnexpectedEOI
    | Unexpected

derive instance genericPE :: Generic ParseError _
instance showPE :: Show ParseError where
  show = genericShow
newtype Parser a = Parser (String -> ParseError \/ (String /\ a))

runParser (Parser f) s = f s

parseChar :: Parser Char
parseChar = Parser
  (\s ->
    maybe
      (Left UnexpectedEOI)
      (\{head, tail} -> Right (tail /\ head))
      (uncons s))

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser (\s -> map (map f) (p s))

instance applyParser :: Apply Parser where
  apply (Parser ff) (Parser af) =
    Parser (\s ->
      either
        Left
        (\(s' /\ f) ->
          either
            Left
            (\(s'' /\ a) -> Right (s'' /\ f a))
            (af s'))
        (ff s))

instance applicativeParser :: Applicative Parser where
  pure a = Parser (\s -> Right (s /\ a))

instance bindParser :: Bind Parser where
  bind (Parser fa) ff =
    Parser (\s ->
      either
        Left
        (\(s' /\ a) ->
          (runParser (ff a) s'))
        (fa s))

instance altParser :: Alt Parser where
  alt p1 p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

failWith :: ∀ a. ParseError -> Parser a
failWith pe = Parser (\s -> Left pe)
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = (\c -> if f c then pure c else failWith (UnexpectedChar c)) =<< parseChar

space :: Parser Char
space = satisfy (_ == ' ')

many :: ∀ a. Parser a -> Parser (Array a)
many p = Parser (\s ->
  either
    (const $ Right (s /\ []))
    (\(s' /\ a) -> runParser ((a: _)<$> many p) s')
  (runParser p s))

many1 :: ∀ t63. Parser t63 → Parser (Array t63)
many1 p = (:) <$> p <*> many p

times :: ∀ a. Int -> Parser a -> Parser (Array a)
times 0 p = pure mempty
times n p = (:) <$> p <*> times (n-1) p

endWith :: ∀ a b. Parser a -> Parser a -> Parser (Array a)
endWith pa pb = (<>) <$> many pa <*> (pure <$> pb)

digit = (fromMaybe 0 <<< Int.fromString <<< singleton) <$> satisfy (\c -> c >= '0' && c <= '9')

parseNumber :: Parser Int
parseNumber = (foldl (\a x -> a * 10 + x) 0) <$> many digit

{--"Name   Age   email"--}
parseFormat :: Parser (String /\ Int)
parseFormat = do
  name <- (fromCharArray <<< join) <$> (endWith (many1 (satisfy (_ /= ' '))) (pure <$> space))
  void (many space)
  age <- parseNumber <* space
  pure (name /\ age)

parseStr :: String -> Parser String
parseStr s = Parser (\i -> case splitAt (Str.length s) i of
  {before, after} -> if before == s then Right (after /\ s) else Left (UnexpectedString i))

notParser :: ∀ a. Parser a -> Parser Unit
notParser p = Parser (\s -> either
  (const $ Right (s /\ unit))
  (const (Left Unexpected))
  (runParser p s))
