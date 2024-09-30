largestPrimeFactorRecursion :: Integer -> Integer
largestPrimeFactorRecursion x = largestPrimeFactorRecursion_ x 2
  where
    largestPrimeFactorRecursion_ x divisor
      | x == 1 = divisor
      | x `mod` divisor == 0 = max divisor $ largestPrimeFactorRecursion_ (x `div` divisor) divisor
      | otherwise = largestPrimeFactorRecursion_ x (getNextPrime divisor)


largestPrimeFactorTailRecursion :: Integer -> Integer
largestPrimeFactorTailRecursion x = largestPrimeFactorTailRecursion_ x 2
  where
    largestPrimeFactorTailRecursion_ x divisor
      | x == 1 = divisor
      | x `mod` divisor == 0 = largestPrimeFactorTailRecursion_ (x `div` divisor) divisor
      | otherwise = largestPrimeFactorTailRecursion_ x (getNextPrime divisor)


largestPrimeFactorWithSequence :: Integer -> Integer
largestPrimeFactorWithSequence x = foldr1 (\_  a -> a) $ filter (\el -> x `mod` el == 0 && isPrime el) [2 .. ceilIntSqrt x]


largestPrimeFactorMap :: Integer -> Integer
largestPrimeFactorMap x = maximum $ map (\el -> if x `mod` el == 0 && isPrime el then el else 0) [2 .. ceilIntSqrt x]


infiniteList :: Integer -> [(Integer, Integer)]
infiniteList x = iterate (\(a, b) -> if b `mod` a == 0 then (a, b `div` a) else (getNextPrime a, b)) (2, x)


largestPrimeFactorInfiniteList :: Integer -> Integer
largestPrimeFactorInfiniteList x = fst . last $ takeWhile (\(a, b) -> b /= 1) $ infiniteList x


getNextPrime :: Integer -> Integer
getNextPrime x
  | isPrime $ x + 1 = x + 1
  | otherwise = getNextPrime (x + 1)


isPrime :: Integer -> Bool
isPrime x
  | x < 2 = False
  | otherwise = not $ hasOtherDivisors_r x 2
  where
    hasOtherDivisors_r x divisor
      | x == divisor = False
      | x `mod` divisor == 0 = True
      | divisor <= ceilIntSqrt x = hasOtherDivisors_r x (divisor + 1)
      | otherwise = False


ceilIntSqrt :: Integer -> Integer
ceilIntSqrt = ceiling . sqrt . fromIntegral
