module Interpolation (
    Interpolation(..),
    Point,
    process
) where

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
    interp' = Interpolation (addLeft xy wind) func name
    res = func (getWindow interp') step
