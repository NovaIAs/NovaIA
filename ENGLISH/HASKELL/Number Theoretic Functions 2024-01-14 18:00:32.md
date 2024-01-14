```haskell
import Data.List (nub, groupBy, sort)
import Data.Function (on)
import Control.Monad (join)
import qualified Data.Map as Map

-- Define a type alias for a list of integers
type IntList = [Int]

-- Define a function to calculate the greatest common divisor (GCD) of two integers
gcd :: Int -> Int -> Int
gcd a b | b == 0 = a
        | otherwise = gcd b (a `rem` b)

-- Define a function to calculate the least common multiple (LCM) of two integers
lcm :: Int -> Int -> Int
lcm a b = a * b `div` gcd a b

-- Define a function to find all the factors of an integer
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `rem` x == 0]

-- Define a function to find all the prime factors of an integer
primeFactors :: Int -> [Int]
primeFactors n = filter isPrime (factors n)
  where
    isPrime n = all (\p -> n `rem` p /= 0) (takeWhile (\p -> p * p <= n) primes)
    primes = 2 : filter isPrime [3,5..]

-- Define a function to calculate the Euler totient function (phi) of an integer
phi :: Int -> Int
phi n = product [p - 1 | p <- primeFactors n]

-- Define a function to find all the divisors of an integer
divisors :: Int -> [Int]
divisors n = nub $ concatMap factors (primeFactors n)

-- Define a function to find the sum of the divisors of an integer
sumOfDivisors :: Int -> Int
sumOfDivisors n = sum (divisors n)

-- Define a function to find the number of divisors of an integer
numberOfDivisors :: Int -> Int
numberOfDivisors n = length (divisors n)

-- Define a function to find the aliquot sum of an integer
aliquotSum :: Int -> Int
aliquotSum n = sum (divisors n) - n

-- Define a function to find the deficiency of an integer
deficiency :: Int -> Int
deficiency n = aliquotSum n - n

-- Define a function to find the abundance of an integer
abundance :: Int -> Int
abundance n = aliquotSum n - n

-- Define a function to find all the perfect numbers up to a given limit
perfectNumbers :: Int -> [Int]
perfectNumbers n = filter (\x -> aliquotSum x == 2 * x) [1..n]

-- Define a function to find all the amicable numbers up to a given limit
amicableNumbers :: Int -> [(Int, Int)]
amicableNumbers n = filter (\(x, y) -> aliquotSum x == y && aliquotSum y == x) [(x, y) | x <- [1..n], y <- [x+1..n]]

-- Define a function to find all the abundant numbers up to a given limit
abundantNumbers :: Int -> [Int]
abundantNumbers n = filter (\x -> abundance x > 0) [1..n]

-- Define a function to find all the deficient numbers up to a given limit
deficientNumbers :: Int -> [Int]
deficientNumbers n = filter (\x -> deficiency x > 0) [1..n]

-- Define a function to find all the semiperfect numbers up to a given limit
semiperfectNumbers :: Int -> [Int]
semiperfectNumbers n = filter (\x -> sumOfDivisors x == 2 * x) [1..n]

-- Define a function to find all the pronic numbers up to a given limit
pronicNumbers :: Int -> [Int]
pronicNumbers n = [x * (x + 1) | x <- [1..n]]

-- Define a function to find all the Catalan numbers up to a given limit
catalanNumbers :: Int -> [Int]
catalanNumbers n = [product [k, n - k + 1] `div` product [k + 1, n - k] | k <- [0..n]]

-- Define a function to find all the Fibonacci numbers up to a given limit
fibonacciNumbers :: Int -> [Int]
fibonacciNumbers n = 0 : 1 : [x + y | (x, y) <- zip (fibonacciNumbers (n - 1)) (fibonacciNumbers (n - 2))]

-- Define a function to find all the Lucas numbers up to a given limit
lucasNumbers :: Int -> [Int]
lucasNumbers n = 2 : 1 : [x + y | (x, y) <- zip (lucasNumbers (n - 1)) (lucasNumbers (n - 2))]
```

This code defines a number of functions for working with integers. These functions include:

* `gcd`: Calculates the greatest common divisor of two integers.
* `lcm`: Calculates the least common multiple of two integers.
* `factors`: Finds all the factors of an integer.
* `primeFactors`: Finds all the prime factors of an integer.
* `phi`: Calculates the Euler totient function (phi) of an integer.
* `divisors`: Finds all the divisors of an integer.
* `sumOfDivisors`: Calculates the sum of the divisors of an integer.
* `numberOfDivisors`: Finds the number of divisors of an integer.
* `aliquotSum`: Finds the aliquot sum of an integer.
* `deficiency`: Finds the deficiency of an integer.
* `abundance`: Finds the abundance of an integer.
* `perfectNumbers`: Finds all the perfect numbers up to a given limit.
* `amicableNumbers`: Finds all the amicable numbers up to a given limit.
* `abundantNumbers`: Finds all the abundant numbers up to a given limit.
* `deficientNumbers`: Finds all the deficient numbers up to a given limit.
* `semiperfectNumbers`: Finds all the semiperfect numbers up to a given limit.
* `pronicNumbers`: Finds all the pronic numbers up to a given limit.
* `catalanNumbers`: Finds all the Catalan numbers up to a given limit.
* `fibonacciNumbers`: Finds all the Fibonacci numbers up to a given limit.
* `lucasNumbers`: Finds all the Lucas numbers up to a given limit.

These functions can be used to solve a variety of problems involving integers, such as finding the greatest common divisor of two numbers, finding the factors of a number, or calculating the sum of the divisors of a number.