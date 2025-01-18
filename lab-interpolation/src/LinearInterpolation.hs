module LinearInterpolation (
    linearInterpolation
) where

import Interpolation
import Window 

linearInterpolation :: Interpolation Point
linearInterpolation = Interpolation (emptyWindow 2) linear "Linear"

linear :: Window Point -> Float -> [Point]
linear wind step = case getPoints wind of
    [(x1, y1), (x0, y0)] -> [(x, linearCount (x0, y0) (x1, y1) x) | x <- takeWhile (< x1+step) $ iterate (+step) x0]
    _ -> []

linearCount :: Point -> Point -> Float -> Float
linearCount (x0, y0) (x1, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)