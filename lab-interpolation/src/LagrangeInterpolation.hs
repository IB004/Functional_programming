module LagrangeInterpolation (
    lagrangeInterpolation
) where

import Interpolation
import Window 

lagrangeInterpolation :: Interpolation Point
lagrangeInterpolation = Interpolation (emptyWindow 4) lagrange "Lagrange"

lagrange :: Window Point -> Float -> [Point]
lagrange wind step = case getPoints wind of
    [(x3, y3), (x2, y2), (x1, y1), (x0, y0)] -> [(x, lagrangePolin [(x0, y0), (x1, y1), (x2, y2), (x3, y3)] x) | x <- takeWhile (< x3+step) $ iterate (+step) x0]
    _ -> []

lagrangePolin :: [Point] -> Float -> Float
lagrangePolin points x = sum $ map (\(ind, (xi, yi)) -> yi * (polin points 0 ind x) / (polin points 0 ind xi)) (zip [0..] points)

polin :: [Point] -> Int -> Int -> Float -> Float
polin [] _ind _n _x = 1
polin ((xi, _yi):points) ind n x
    | ind == n = polin points (ind+1) n x
    | otherwise = (x - xi) * (polin points (ind+1) n x)