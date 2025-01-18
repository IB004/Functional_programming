module Interpolation where

import Window 

type Point = (Float, Float)

data Interpolation p = Interpolation {
    getWindow :: Window p,
    getFunc :: Window p -> Float ->  [p],
    getName :: String
}

linearInetrpolation :: Interpolation Point
linearInetrpolation = Interpolation (emptyWindow 2) linear "Линейная"

linear :: Window Point -> Float -> [Point]
linear wind step = case getPoints wind of
    [(x1, y1), (x0, y0)] -> [(x, linearCount (x0, y0) (x1, y1) x) | x <- takeWhile (< x1+step) $ iterate (+step) x0]
    _ -> []

linearCount :: Point -> Point -> Float -> Float
linearCount (x0, y0) (x1, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)

process :: Float -> Interpolation Point -> Point -> ([Point], Interpolation Point)
process step (Interpolation wind func name) xy = (res, interp')
    where
    interp' = Interpolation (addLeft xy $ wind) func name
    res = func (getWindow interp') step