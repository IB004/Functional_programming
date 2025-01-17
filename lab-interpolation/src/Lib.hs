module Lib where

import Text.Read(readMaybe)
import Control.Monad (unless)

data Window a = Window [a] deriving (Show, Eq)

emptyWindow = Window []

addLeft x (Window win)  = Window $ x : win

readToWindow wind = do 
    putStrLn $ show wind
    input <- readDot
    unless (input == "exit") $ do
        let xy = extractNumbers input
        readToWindow $ addLeft xy wind

someFunc :: IO ()
someFunc = putStrLn "Hello!"

readNumbers :: IO [Float]
readNumbers = do
      n <- getLine
      case (n, readMaybe n :: Maybe Float) of
        ("end", _) -> return []
        (_, Just n) -> (n:) <$> readNumbers
        (_, Nothing) -> readNumbers

readDot = do
    putStr "Enter x y: "
    xy <- getLine
    return xy

extractNumbers :: String -> (Float, Float)
extractNumbers xy = (read x, read y)
    where 
    lst = words xy
    x = head lst
    y = head $ tail lst

hw :: IO ()
hw = do
    putStrLn "What is your name?"
    name <- getLine
    if name == "" 
        then hw
        else putStrLn $ "Hello, " ++ name ++ "!"


putStr' :: String -> IO ()
putStr' "" = return ()
putStr' (c:cs) = putChar c >> putStr' cs
