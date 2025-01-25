module JsonParser (
    JsonValue(..),
    json,
    jsonValue,
    jsonNull,
    jsonBool,
    jsonNumber,
    stringLiteral,
    jsonString,
    jsonArray,
    jsonObject
) where

import Parser
import Control.Applicative

data JsonValue = JsonNull
               | JsonBool Bool 
               | JsonNumber Double
               | JsonString String 
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Eq)

instance Show JsonValue where
  show JsonNull = "null"
  show (JsonBool value) = show value
  show (JsonNumber value) = show value
  show (JsonString value) = show value
  show (JsonArray values) = show values
  show (JsonObject fields) = "{\n" ++ (concatMap (\(f, v) -> "  " ++ f ++ ": " ++ show v ++ ",\n") fields) ++ "}"


json :: Parser JsonValue
json = ws *> jsonValue <* ws

jsonValue :: Parser JsonValue
jsonValue =     jsonNull 
            <|> jsonBool 
            <|> jsonNumber 
            <|> jsonString 
            <|> jsonArray 
            <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser JsonValue
jsonBool = strToJsonBool <$> (string "true" <|> string "false")
    where
    strToJsonBool "true" = JsonBool True
    strToJsonBool "false" = JsonBool False
    strToJsonBool _ = undefined

formDouble_ :: Integer  -- sign
            -> Integer  -- integral part
            -> Double   -- decimal part
            -> Integer  -- exponent
            -> Double
formDouble_ sign int dec expo =
    fromIntegral sign * (fromIntegral int + dec) * (10 ^^ expo)

doubleLiteral :: Parser Double
doubleLiteral =
  formDouble_
    <$> (sign <|> pure 1)
    <*> (read <$> number)
    <*> ((read <$> decimal) <|> pure 0)
    <*> ((e *> ((*) <$> (sign <|> pure 1) <*> (read <$> number))) <|> pure 0)
  where
    delimiter = char '.'
    decimal = ('0':) <$> ((:) <$> delimiter <*> number)
    sign = ((-1) <$ char '-') <|> (1 <$ char '+')
    e = char 'e' <|> char 'E'

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> doubleLiteral

simpleChar_ :: Parser Char
simpleChar_ = satisfy ((&&) <$> (/= '"') <*> (/= '\\'))

escapedChar_ :: Parser Char 
escapedChar_ =  ('"' <$ string "\\\"") 
            <|> ('\\' <$ string "\\\\")
            <|> ('\n' <$ string "\\n") 
            <|> ('\r' <$ string "\\r") 
            <|> ('\t' <$ string "\\t") 

stringLiteral :: Parser [Char]
stringLiteral = char '"' *>  many (simpleChar_ <|> escapedChar_) <* char '"'

jsonString :: Parser JsonValue
jsonString =  JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (char '[' *> ws *> elements <* ws <* char ']')
    where
    sep = ws *> char ',' <* ws
    elements = jsonValue `sepBy` sep

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (char '{' *> ws *> elements <* ws <* char '}')
    where
    sep = ws *> char ',' <* ws
    pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> char ':' <* ws) <*> jsonValue
    elements = pair `sepBy` sep
