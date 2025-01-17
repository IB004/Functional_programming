module Lib where

import Text.Read(readMaybe)
import Control.Monad (unless)
import Window

someFunc :: IO ()
someFunc = putStrLn "Hello!"

readToWindow :: Window (Float, Float) -> IO ()
readToWindow wind = do 
    putStrLn $ show wind
    input <- readDot
    unless (isEnd input) $ do
        let xy = extractNumbers input
        readToWindow $ addLeft xy wind

readDot :: IO String
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


isEnd :: String -> Bool
isEnd "exit" = True 
isEnd "e" = True 
isEnd "end" = True 
isEnd "quit" = True 
isEnd "q" = True 
isEnd _ = False 