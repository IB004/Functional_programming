largestPrimeFactor :: Integer -> Integer
largestPrimeFactor x = primeFactor x 2 where
    primeFactor x divisor 
        | isPrime x            = x
        | x `mod` divisor == 0 = primeFactor (x `div` divisor) divisor
        | otherwise            = primeFactor x (getNextPrime divisor)



getNextPrime :: Integer -> Integer
getNextPrime x | isPrime $ x + 1 = x + 1
               | otherwise       = getNextPrime (x + 1)


isPrime :: Integer -> Bool
isPrime x | x < 2     = False
          | otherwise = not $ hasOtherDivisors_r x 2 where
    hasOtherDivisors_r x divisor | x == divisor            = False
                                 | x `mod` divisor == 0    = True
                                 | divisor <= getIntSqrt x = hasOtherDivisors_r x (divisor + 1)
                                 | otherwise               = False


getIntSqrt :: Integer -> Integer
getIntSqrt = ceiling . sqrt . fromIntegral