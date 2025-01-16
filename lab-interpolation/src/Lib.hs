module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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


hw = do
    putStrLn "What is your name?"
    name <- getLine
    if name == "" 
        then hw
        else putStrLn $ "Hello, " ++ name ++ "!"


putStr' :: String -> IO ()
putStr' "" = return ()
putStr' (c:cs) = putChar c >> putStr' cs

