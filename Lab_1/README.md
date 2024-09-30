# Лабораторная работа №1

`Буто Иван` | `P3317` | `3, 28`

## [3. Наибольший простой делитель](https://projecteuler.net/problem=3)

> ### Largest Prime Factor
>
> Problem 3
>
> The prime factors of **13195** are **5**, **7**, **13** and **29**.
> What is the largest prime factor of the number **600851475143**?

Решение на Haskell:

```Haskell
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
```

Решение на Python:

```Python
from math import sqrt

def largest_prime_factor(x):
    if is_prime(x):
        return x
    divisor = 2
    while divisor <= sqrt(x):
        if x == divisor:
            return divisor
        if x % divisor == 0:
            return largest_prime_factor(x // divisor)
        divisor = get_next_prime(divisor)

def get_next_prime(x):
    x += 1
    while not is_prime(x):
        x += 1
    return x

def is_prime(x):
    if x < 2:
        return False
    divisor = 2
    while divisor <= sqrt(x):
        if x % divisor == 0:
            return False
        divisor += 1
    return True

x = 600851475143
print(largest_prime_factor(x)) #6857
```

## [28. Сумма чисел на диагоналях](https://projecteuler.net/problem=28)

> ### Number Spiral Diagonals
>
> Problem 28
>
> Starting with the number **1** and moving to the right in a clockwise direction a **5** by **5** spiral is formed as follows:
>
> **21** 22 23 24 **25**
> 
> 20  **07**  08  **09** 10
> 
> 19  06  **01**  02 11
> 
> 18  **05**  04  **03** 12
> 
> **17** 16 15 14 **13**
> 
> It can be verified that the sum of the numbers on the diagonals is **101**.
> What is the sum of the numbers on the diagonals in a **1001** by **1001** spiral formed in the same way?

Решение на Haskell:

```Haskell
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
```

Решение на Python:

```Python
def diag_sum(lenght):
    dist = 2
    x = 1
    res = 1
    while dist < lenght:
        for i in range(4):
            x += dist
            res += x
        dist += 2
    return res


print(diag_sum(1001)) #669171001
```
## Выводы

1. Одним из наиболее удобных и эффективных способов решения считаю использование хвостовой рекурсии. В ней относительно легко контролировать процесс решения и определять исключительные ситуации. К сожалению, это не самый лаконичный способ решения.

2. Также мне понравилась генерировать бесконечные списки и обрабатывать их значения. Так решение получается коротким и ясным.

3. После изучения основ Haskell могу еще раз подтвердить, что этот язык мне приятен. В особенности своей лаконичностью и системой типов.