module Window where

data Window a = Window {
    getCapacity :: Int,
    getSize :: Int,
    getPoints :: [a]
} deriving (Show, Eq)

emptyWindow :: Int -> Window a
emptyWindow cap | cap < 0 = error "Window capacity should be positive."
                | otherwise = Window cap 0 []

addLeft :: a -> Window a -> Window a 
addLeft x (Window cap sz xs) 
    | sz > cap = error "Window size can't be greater than capacity."
    | sz < cap = Window cap (sz + 1) (x : xs)
    | otherwise = Window cap sz (x : (init xs))