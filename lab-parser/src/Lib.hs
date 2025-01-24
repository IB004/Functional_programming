module Lib
    ( someFunc
    ) where

import Data.Char
import Data.Either
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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

-- no escape support
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




newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  fmap k prs = Parser fun where
      fun s = case (runParser prs s) of
          (Left err) -> Left err
          (Right (a, s')) -> Right (k a, s')

instance Applicative Parser where
  pure x = Parser fun where
      fun s = Right (x, s)
  pf <*> pv = Parser fun where
      fun s = do
          (g, s') <- runParser pf s
          (x, s'') <- runParser pv s'
          return (g x, s'')

instance Alternative Parser where
  empty = Parser fun where
      fun _s = Left "empty"
  pa <|> pb = Parser fun where
      fun s = if (isRight a) then a else b 
          where
          a = runParser pa s
          b = runParser pb s  


satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser fun where
    fun "" = Left "unexpected end of input"
    fun (x:xs) | pr x = Right (x, xs)
               | otherwise = Left $ "unexpected '" ++ [x] ++ "'"
        

char_ :: Char -> Parser Char
char_ c = satisfy (== c)

char :: Char -> Parser Char
char c = Parser $ \inp ->
    case (runParser (char_ c) inp) of 
        (Left err) -> Left (err ++ ", expected '" ++ [c] ++ "'" ++ context inp )
        result -> result 


digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

ws :: Parser String
ws = many (satisfy isSpace)

string_ :: String -> Parser String
string_ str = sequenceA $ map char str 

string :: String -> Parser String
string str = Parser $ \inp ->
    case (runParser (string_ str) inp) of 
        (Left _) -> Left ("expected " ++ str ++ context inp )
        result -> result 

sepBy :: Parser e -> Parser s -> Parser [e] 
sepBy element sep = ((:) <$> element <*> many (sep *> element)) <|> pure []

number :: Parser [Int]
number = some digit 

context :: String -> String
context inp = " in " ++ take 10 inp