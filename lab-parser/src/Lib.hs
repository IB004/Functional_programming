module Lib ( 
    mainFunc
) where

import Parser
import JsonParser
import System.IO

mainFunc :: IO ()
mainFunc = do 
    inp <- readStdin
    showInput inp
    case (runParser json inp) of
        (Right (ast, rest)) -> showResult (ast, rest)
        (Left err) -> showError err

showInput :: String -> IO ()  
showInput inp = do
    putStr "Get input: "
    putStrLn inp
    hFlush stdout 

showResult :: Show a => (a, String) -> IO ()
showResult (ast, rest) = do 
    putStrLn $ show ast
    putStrLn "Rest input: "
    putStrLn rest
    hFlush stdout 

showError :: String -> IO ()
showError err = do 
    putStrLn err
    hFlush stdout 

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