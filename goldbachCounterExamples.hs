
-- get an infinite list of all the odd numbers starting from 3
oddsFrom3 = [3, 5 ..]

-- define a function small prime divisors, that takes in a value of n and find elements of primes whose square is less than or equal to n that equally divide n.
-- we mention primes in our function before primes is defined, this is a form of recursion, we use the fact that we have computed the primes up to n-2 to determine whether n is prime
smallPrimeDivisors :: Integer -> [Integer]
smallPrimeDivisors n = [d | d <- takeWhile (\x -> x^2 <= n) primes, n `mod` d == 0]

-- our primes is an infinite list appended onto 2, where each element n is an element from oddFrom3 where there are no small prime divisors for n
primes = 2 : [n | n <- oddsFrom3, null (smallPrimeDivisors n)]

-- goldbach's other conjecture
-- Every odd composite number can be expressed as the sum of a prime and
-- twice a square. E.g., 35 = 17 + 2*(3^2); 99 = 67 + 2*(4^2). (Disproved. It is not true for 5777 or 5993.

-- function for getting a square root
iSqrt :: (Integral a, Integral b) => a -> b
iSqrt n = round (sqrt (fromIntegral n))

-- function for getting whether an Integral is a square
isASquare :: (Integral a) => a -> Bool
isASquare n = (iSqrt n) ^ 2 == n

--isPrime function
isPrime :: Integer -> Bool
isPrime n = n `elem` (takeWhile (<=n) primes)

-- define our list of odd non primes
oddNonPrimes = [n | n <- oddsFrom3, not $ isPrime n]

-- define what our goldbach pair is
goldbach :: Integer -> [(Integer, Integer)]
goldbach g = [(p, iSqrt k) | p <- takeWhile(<g) primes, let k = ((g - p) `div` 2), isASquare k]

-- define our goldbach counterExamples
goldbachCounterExamples = [g | g <- oddNonPrimes, null $ goldbach g]




