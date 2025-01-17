module Interpolation where

import Window 

data Interpolation a = Interpolation {
    window :: Window a,
    func :: Window a -> Float ->  [a],
    name :: String
}

linearInetrpolation = Interpolation (emptyWindow 2) linear "Линейная"

linear :: Window (Float, Float) -> Float -> [(Float, Float)]
linear wind step = case dots wind of
    [(x1, y1), (x0, y0)] -> [(x, linearCount (x0, y0) (x1, y1) x) | x <- takeWhile (< x1+step) $ iterate (+step) x0]
    _ -> []

linearCount (x0, y0) (x1, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)

process :: Float -> Interpolation (Float, Float) -> (Float, Float) -> ([(Float, Float)], Interpolation (Float, Float))
process step (Interpolation wnd fnc name) xy = (res, interp')
    where
    interp' = Interpolation (addLeft xy $ wnd) fnc name
    res = fnc (window interp') step