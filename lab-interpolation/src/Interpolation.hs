module Interpolation where

import Window 

data Interpolation a = Interpolation {
    window :: Window a,
    func :: Float -> Window a -> [a] 
}

linear :: Float -> Window (Float, Float) -> [(Float, Float)]
linear step wind = case dots wind of
    [(x1, y1), (x0, y0)] -> [(x, linearCount (x0, y0) (x1, y1) x) | x <- takeWhile (<= x1) $ iterate (+step) x0]
    _ -> []

linearCount (x0, y0) (x1, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)