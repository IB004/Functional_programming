module Lib where

import Text.Read(readMaybe)
import Control.Monad (unless)
import Window
import Interpolation
import System.IO
import Data.Maybe (isJust)
import Text.Printf (printf)

someFunc :: IO ()
someFunc = mainLoop [linearInetrpolation, lagrangeInetrpolation]

readStdin = do 
    input' <- getLine
    let input = cutBom input'
    unless (input == "end") $ do
        putStrLn input 
        hFlush stdout 
        readStdin 

mainLoop interps = do 
    input <- readPoint
    unless (isEnd input) $ do
        if (isValid $ words input)
        then do 
            let step = 1
            let xy = extractNumbers input
            let results = map (\i -> process step i xy) interps
            mapM_ (\(res, int) -> putStr $ showRes (getName int) res) results
            mainLoop (snd $ unzip results)
        else do 
            putStrLn "Please, enter float x and float y"
            mainLoop interps



isValid :: [String] -> Bool
isValid [x,y] = isJust (readMaybe x :: Maybe Float) && isJust (readMaybe y :: Maybe Float)
isValid _ = False


readPoint :: IO String
readPoint = do
    putStr "Enter x y: "
    hFlush stdout
    xy <- getLine
    return $ cutBom xy

extractNumbers :: String -> Point
extractNumbers xy = (read x, read y)
    where 
    lst = words xy
    x = head lst
    y = head $ tail lst

isEnd :: String -> Bool
isEnd "exit" = True 
isEnd "e" = True 
isEnd "end" = True 
isEnd "quit" = True 
isEnd "q" = True 
isEnd _ = False 

showRes :: String -> [Point] -> String
showRes _ [] = ""
showRes header points = "\n" ++ (showHeader header) ++ (concat $ map showPoint points) ++ "\n"

showHeader :: String -> String
showHeader header = header ++ ":\n"

showPoint :: Point -> String
showPoint (x, y) = (printf "%.2f" x) ++ "  " ++ (printf "%.2f" y) ++ "\n"

cutBom :: String -> String
cutBom ('\xfeff':str) = str 
cutBom str = str 
