module Interpolation where

import Window 

type Point = (Float, Float)

data Interpolation p = Interpolation {
    getWindow :: Window p,
    getFunc :: Window p -> Float ->  [p],
    getName :: String
}

process :: Float -> Interpolation Point -> Point -> ([Point], Interpolation Point)
process step (Interpolation wind func name) xy = (res, interp')
    where
    interp' = Interpolation (addLeft xy $ wind) func name
    res = func (getWindow interp') step




linearInetrpolation :: Interpolation Point
linearInetrpolation = Interpolation (emptyWindow 2) linear "Linear"

linear :: Window Point -> Float -> [Point]
linear wind step = case getPoints wind of
    [(x1, y1), (x0, y0)] -> [(x, linearCount (x0, y0) (x1, y1) x) | x <- takeWhile (< x1+step) $ iterate (+step) x0]
    _ -> []

linearCount :: Point -> Point -> Float -> Float
linearCount (x0, y0) (x1, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)




lagrangeInetrpolation :: Interpolation Point
lagrangeInetrpolation = Interpolation (emptyWindow 4) lagrange "Lagrange"

lagrange :: Window Point -> Float -> [Point]
lagrange wind step = case getPoints wind of
    [(x3, y3), (x2, y2), (x1, y1), (x0, y0)] -> [(x, lagrangeCount (x0, y0) (x1, y1) (x2, y2) (x3, y3) x) | x <- takeWhile (< x3+step) $ iterate (+step) x0]
    _ -> []

lagrangeCount :: Point -> Point -> Point -> Point -> Float -> Float
lagrangeCount (x0, y0) (x1, y1) (x2, y2) (x3, y3) x = 
    y0 * (x - x1) *  (x - x2) *  (x - x3) / ( (x0 - x1) *  (x0 - x2) *  (x0 - x3) ) + 
    y1 * (x - x0) *  (x - x2) *  (x - x3) / ( (x1 - x0) *  (x1 - x2) *  (x1 - x3) ) + 
    y2 * (x - x0) *  (x - x1) *  (x - x3) / ( (x2 - x0) *  (x2 - x1) *  (x2 - x3) ) + 
    y3 * (x - x0) *  (x - x1) *  (x - x2) / ( (x3 - x0) *  (x3 - x1) *  (x3 - x2) )


