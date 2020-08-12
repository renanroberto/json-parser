{-# LANGUAGE InstanceSigs #-}

module Parser where

import Data.Char (isSpace)
import Data.Tuple (swap)
import Control.Applicative


type ParserError = String

newtype Parser a = Parser
  { runParser :: String -> Either ParserError (String, a) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    return (input', f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Right (input, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser p = Parser $ \input -> do
    (input', g) <- f input
    (input'', x) <- p input'
    return (input'', g x)

instance Monoid e => Alternative (Either e) where
  empty :: Either e a
  empty = Left mempty

  (<|>) :: Either e a -> Either e a -> Either e a
  a <|> b = a <> b

instance Alternative Parser where
  empty :: Parser a
  empty = (Parser . const) (Left "Parser error")

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser p <|> Parser q = Parser $ \input ->
    p input <|> q input

flatParser :: Parser (Parser a) -> Parser a
flatParser (Parser p) = Parser $ \input -> do
  (input', Parser q) <- p input
  (input'', x) <- q input'
  return (input'', x)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  x >>= f = flatParser (fmap f x)


char :: Char -> Parser Char
char c = Parser h
  where h :: String -> Either ParserError (String, Char)
        h (s:str)
          | s == c = Right (str, s)
          | s /= c = Left ("Unexpected " <> [s] <> ". Expecting " <> [c])
        h _ = Left ("Expecting " <> [c])

string :: String -> Parser String
string = traverse char

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser (Right . swap . span pred)

notNull :: String -> Parser [a] -> Parser [a]
notNull errMsg (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Left errMsg
    else Right (input', xs)

noneOf :: [Char] -> Parser Char
noneOf blacklist = Parser h
  where h :: String -> Either ParserError (String, Char)
        h (s:str)
          | s `notElem` blacklist = Right (str, s)
          | s `elem` blacklist = Left ("Unexpected " <> [s])
        h _ = Left "Expecting something to parse"

anyOf :: [Char] -> Parser Char
anyOf whitelist = Parser h
  where h :: String -> Either ParserError (String, Char)
        h (s:str)
          | s `elem` whitelist = Right (str, s)
          | s `notElem` whitelist = Left ("Unexpected " <> [s])
        h _ = Left "Expecting something to parse"

anyChar :: Parser Char
anyChar = Parser $ h
  where h :: String -> Either ParserError (String, Char)
        h (s:str) = Right (str, s)
        h _ = Left "Expecting something to parse"

space :: Parser String
space = spanP isSpace

token :: Char -> Parser Char
token t = space *> char t <* space

comma :: Parser Char
comma = token ','

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy element sep = liftA2 (:) element (many (sep *> element))
  <|> pure []

endBy :: Parser a -> Parser b -> Parser a
endBy element end = element <* end

eol :: Parser String
eol =
  string "\n\r"
  <|> string "\r\n"
  <|> string "\n"
  <|> string "\r"

eof :: Parser ()
eof = Parser h
  where h :: String -> Either ParserError (String, ())
        h "" = Right ("", ())
        h _  = Left "Expecting end of file"
