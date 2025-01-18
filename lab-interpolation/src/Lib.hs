module Lib (
    execute
) where

import Text.Read(readMaybe)
import Control.Monad (unless)
import Interpolation
import System.IO
import Data.Maybe (isJust)
import Text.Printf (printf)

import LinearInterpolation
import LagrangeInterpolation

execute :: IO ()
execute = mainLoop [linearInterpolation, lagrangeInterpolation]

mainLoop :: [Interpolation Point] -> IO ()
mainLoop interps = do 
    input <- readPoint
    unless (isEnd input) $ do
        if (isValid $ words input)
        then do 
            let step = 1
            let xy = extractNumbers $ words input
            let results = map (\i -> process step i xy) interps
            mapM_ (\(res, int) -> putStr $ showRes (getName int) res) results
            mainLoop (map snd results)
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
    cutBom <$> getLine

extractNumbers :: [String] -> Point
extractNumbers (x:y:_rest) = (read x, read y)
extractNumbers _ = error "Can not extract numbers"

isEnd :: String -> Bool
isEnd "exit" = True 
isEnd "e" = True 
isEnd "end" = True 
isEnd "quit" = True 
isEnd "q" = True 
isEnd _ = False 

showRes :: String -> [Point] -> String
showRes _ [] = ""
showRes header points = "\n" ++ (showHeader header) ++ (concatMap showPoint points) ++ "\n"

showHeader :: String -> String
showHeader header = header ++ ":\n"

showPoint :: Point -> String
showPoint (x, y) = (printf "%.2f" x) ++ "  " ++ (printf "%.2f" y) ++ "\n"

cutBom :: String -> String
cutBom ('\xfeff':str) = str 
cutBom str = str 
