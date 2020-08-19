module JsonParser (JsonValue, parseJsonC) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Applicative

import Parser


data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Float
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving Show


parseNull :: Parser JsonValue
parseNull = const JsonNull <$> string "null"

parseBool :: Parser JsonValue
parseBool = (JsonBool . toBool) <$> (string "true" <|> string "false")
  where toBool :: String -> Bool
        toBool "true" = True
        toBool "false" = False

stringLiteral :: Parser String
stringLiteral =
  char '"' *> many text <* char '"'
  where text :: Parser Char
        text = noneOf ['\\', '"']
          <|> char '\\' *> anyChar

parseString :: Parser JsonValue
parseString = JsonString <$> stringLiteral

parseNumber :: Parser JsonValue
parseNumber = do
  let numbers = ['0' .. '9']
  let floats = '.' : numbers
  
  digit <- anyOf numbers
  digits <- (many . anyOf) floats

  let result = read (digit:digits)
  return (JsonNumber result)

parseArray :: Parser JsonValue
parseArray = JsonArray <$>
  (token '[' *> (parseValue `sepBy` comma) <* token ']')

parseElement :: Parser (String, JsonValue)
parseElement = do
  key <- stringLiteral
  token ':'
  value <- parseValue
  return (key, value)

parseObject :: Parser JsonValue
parseObject = do
  token '{'
  elements <- parseElement `sepBy` comma
  token '}'
  return (JsonObject elements)

parseValue :: Parser JsonValue
parseValue =
  parseNull
  <|> parseBool
  <|> parseString
  <|> parseNumber
  <|> parseArray
  <|> parseObject

parseJson :: Parser JsonValue
parseJson = (parseArray <|> parseObject) <* eof


parseSinglelineComment :: Parser Char
parseSinglelineComment =
  string "//" *> many (noneOf "\n\r") *> eol *> pure ' '

parseMultilineComment :: Parser Char
parseMultilineComment =
  string "/*" *> many comment *> string "*/" *> pure ' '
  where comment :: Parser Char
        comment = noneOf "*" <|> (char '*' *> noneOf "/")

parseComment :: Parser String
parseComment = many $
  parseSinglelineComment
  <|> parseMultilineComment
  <|> anyChar


parseJsonC :: String -> Either ParserError JsonValue
parseJsonC input = do
  (_, input') <- evalStateT initialState $ runParser parseComment input
  (_, json) <- evalStateT initialState $ runParser parseJson input'
  return json
