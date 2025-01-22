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

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> string "null"

jsonBool :: Parser JsonValue
jsonBool = strToJsonBool <$> (string "true" <|> string "false")
    where
    strToJsonBool "true" = JsonBool True
    strToJsonBool "false" = JsonBool False
    strToJsonBool _ = undefined

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
               | otherwise = Left $ "unexpected " ++ [x]
        

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

string :: String -> Parser String
string str = sequenceA $ map char str 

