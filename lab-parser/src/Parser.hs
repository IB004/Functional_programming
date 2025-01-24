module Parser (
  Parser (..),
  satisfy,
  char,
  digit,
  ws,
  string,
  sepBy,
  number
) where

import Data.Char
import Data.Either
import Control.Applicative

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
ws = many (satisfy (\c -> isSpace c || c == '\xfeff'))

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
context inp = ": " ++ take 20 inp