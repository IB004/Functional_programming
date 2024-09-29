diagSum len = diagSum_r 1 0 (len^2) where
    diagSum_r x delta maxX | x > maxX   = 0
                           | isCorner x = x + diagSum_r (x + delta + 2) (delta + 2) maxX
                           | otherwise  = x + diagSum_r (x + delta) delta maxX


diagSumRecursion len = diagSumRecursion_r 1 (len^2) where
    diagSumRecursion_r x maxX | x > maxX   = 0
                              | isOnDiag x = x + diagSumRecursion_r (x + 1) maxX
                              | otherwise  = diagSumRecursion_r (x + 1) maxX

diagSumTailRecursion len = diagSumRecursion_r 1 0 (len^2) where
    diagSumRecursion_r x acc maxX | x > maxX   = acc
                                  | isOnDiag x = diagSumRecursion_r (x + 1) (acc + x) maxX
                                  | otherwise  = diagSumRecursion_r (x + 1) acc maxX

diagSumMap len = foldr (+) 0 . map (\x -> if isOnDiag x then x else 0) $ [1..(len^2)]

diagSumInfiniteList len = foldr (+) 0 . map (\x -> if isOnDiag x then x else 0) $ takeWhile (<=(len^2)) [1..]

isOnDiag :: Integer -> Bool
isOnDiag x | isCorner x = True
           | x == corner - 1 * delta = True
           | x == corner - 2 * delta = True
           | x == corner - 3 * delta = True
           | otherwise = False
    where
    corner = closesetCorner x
    delta = floorIntSqrt corner - 1
           


closesetCorner :: Integer -> Integer
closesetCorner x | isCorner x = x
                 | otherwise  = (xCut + 1 + xCut `mod` 2)^2 where
                    xCut = floorIntSqrt x


isCorner :: Integer -> Bool
isCorner x = x `mod` 2 == 1 && x == (floorIntSqrt x)^2


floorIntSqrt :: Integer -> Integer
floorIntSqrt = floor . sqrt . fromIntegral