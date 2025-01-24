module JsonParser (
    JsonValue(..),
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
import Data.Char
import Control.Applicative

data JsonValue = JsonNull
               | JsonBool Bool 
               | JsonNumber Integer -- todo: double
               | JsonString String 
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue =     jsonNull 
            <|> jsonBool 
            <|> jsonNumber 
            <|> jsonString 
            <|> jsonArray 
            <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> string "null"

jsonBool :: Parser JsonValue
jsonBool = strToJsonBool <$> (string "true" <|> string "false")
    where
    strToJsonBool "true" = JsonBool True
    strToJsonBool "false" = JsonBool False
    strToJsonBool _ = undefined

jsonNumber :: Parser JsonValue
jsonNumber = lstToInt <$> number 
    where
    lstToInt nms = JsonNumber $ read $ map intToDigit nms

stringLiteral :: Parser [Char]
stringLiteral = char '"' *> (many $ satisfy (/= '"')) <* char '"'

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
