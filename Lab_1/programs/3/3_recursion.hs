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
          | otherwise = not $ haveOtherDivisors x 2

haveOtherDivisors :: Integer -> Integer -> Bool
haveOtherDivisors x divisor | x == divisor         = False
                            | x `mod` divisor == 0 = True
                            | divisor <= getSqrt x = haveOtherDivisors x (divisor + 1)
                            | otherwise            = False

getSqrt :: Integer -> Integer
getSqrt = ceiling . sqrt . fromIntegral