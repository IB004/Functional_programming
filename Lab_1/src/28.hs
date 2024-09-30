diagSumRecursion :: Integer -> Integer
diagSumRecursion len = _diagSumRecursion 1 0 (len ^ 2)
  where
    _diagSumRecursion x delta maxX
      | x > maxX = 0
      | isCorner x = x + _diagSumRecursion (x + delta + 2) (delta + 2) maxX
      | otherwise = x + _diagSumRecursion (x + delta) delta maxX


diagSumTailRecursion :: Integer -> Integer
diagSumTailRecursion len = _diagSumTailRecursion 1 0 0 (len ^ 2)
  where
    _diagSumTailRecursion x delta acc maxX
      | x > maxX = acc
      | isCorner x = _diagSumTailRecursion (x + delta + 2) (delta + 2) (acc + x) maxX
      | otherwise = _diagSumTailRecursion (x + delta) delta (acc + x) maxX


diagSumWithSequence :: Integer -> Integer
diagSumWithSequence len = foldr (+) 0 $ filter isOnDiag [1 .. (len ^ 2)]


diagSumMap :: Integer -> Integer
diagSumMap len = sum $ map (\x -> if isOnDiag x then x else 0) [1 .. (len ^ 2)]


diagSumInfiniteList :: Int -> Integer
diagSumInfiniteList len = sum $ map fst $ take ((len `div` 2) * 4 + 1) $ iterate getNextDiag (1, 0)


getNextDiag :: (Integer, Integer) -> (Integer, Integer)
getNextDiag (x, delta)
  | isCorner x = (x + delta + 2, delta + 2)
  | otherwise = (x + delta, delta)


isOnDiag :: Integer -> Bool
isOnDiag x
  | isCorner x = True
  | x == corner - 1 * delta = True
  | x == corner - 2 * delta = True
  | x == corner - 3 * delta = True
  | otherwise = False
  where
    corner = closesetCorner x
    delta = floorIntSqrt corner - 1


closesetCorner :: Integer -> Integer
closesetCorner x
  | isCorner x = x
  | otherwise = (xCut + 1 + xCut `mod` 2) ^ 2
  where
    xCut = floorIntSqrt x


isCorner :: Integer -> Bool
isCorner x = x `mod` 2 == 1 && x == floorIntSqrt x ^ 2


floorIntSqrt :: Integer -> Integer
floorIntSqrt = floor . sqrt . fromIntegral
