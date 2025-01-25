module Lib ( 
    mainLoop
) where

import Parser
import JsonParser
import System.IO

mainLoop :: IO ()
mainLoop = do 
    inp <- getContents
    putStrLn inp
    case (runParser json inp) of
        (Right (ast, rest)) -> putStrLn $ show ast
        (Left err) -> putStrLn err
    hFlush stdout 

myLoop = do 
    done <- isEOF
    if done
      then do 
        putStrLn "Bye!"
        hFlush stdout   
      else do 
        inp <- getLine
        putStrLn inp
        hFlush stdout 
        myLoop

readStdin :: IO String
readStdin = readStdin_ ""

readStdin_ :: String -> IO String
readStdin_ acc = do
    done <- isEOF
    if done
      then do
        return acc
      else do 
        inp <- getLine
        readStdin_ $ acc ++ "\n" ++  inp