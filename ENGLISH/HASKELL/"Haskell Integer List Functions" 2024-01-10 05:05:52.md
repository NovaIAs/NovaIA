```haskell
import Data.List (nub, sort)
import Data.Maybe (fromJust)

-- This function takes a list of integers and returns a tuple containing the maximum and minimum values in the list.
getMaxMin :: [Int] -> (Int, Int)
getMaxMin xs = (maximum xs, minimum xs)

-- This function takes a list of integers and returns the average value of the list.
getAverage :: [Int] -> Double
getAverage xs = sum xs / fromJust (length xs)

-- This function takes a list of integers and returns a list of the unique values in the list, sorted in ascending order.
getUniqueSorted :: [Int] -> [Int]
getUniqueSorted xs = sort (nub xs)

-- This function takes a list of integers and returns a list of the prime numbers in the list.
isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [2 .. (truncate (sqrt (fromIntegral n)))]

getPrimes :: [Int] -> [Int]
getPrimes xs = filter isPrime xs

-- This function takes a list of integers and returns a list of the factors of the first integer in the list.
getFactors :: [Int] -> [Int]
getFactors (x:xs) = filter (\y -> x `mod` y == 0) [1 .. x]

-- This function takes a list of integers and returns a list of the perfect numbers in the list.
isPerfect :: Int -> Bool
isPerfect n = n == sum (getFactors n)

getPerfects :: [Int] -> [Int]
getPerfects xs = filter isPerfect xs

-- This function takes a list of integers and returns a list of the amicable numbers in the list.
isAmicable :: Int -> Int -> Bool
isAmicable x y = x /= y && x == sum (getFactors y) && y == sum (getFactors x)

getAmicables :: [Int] -> [Int]
getAmicables xs = filter (\(x, y) -> isAmicable x y) [(x, y) | x <- xs, y <- xs, x /= y]

-- This function takes a list of integers and returns a list of the happy numbers in the list.
isHappy :: Int -> Bool
isHappy n = n == 1 || (n `mod` 2 == 0 && isHappy (n `div` 2)) || (n `mod` 3 == 0 && isHappy (n `div` 3)) || (n `mod` 4 == 0 && isHappy (n `div` 4)) || (n `mod` 5 == 0 && isHappy (n `div` 5)) || (n `mod` 6 == 0 && isHappy (n `div` 6)) || (n `mod` 7 == 0 && isHappy (n `div` 7)) || (n `mod` 8 == 0 && isHappy (n `div` 8)) || (n `mod` 9 == 0 && isHappy (n `div` 9))

getHappies :: [Int] -> [Int]
getHappies xs = filter isHappy xs

-- This function takes a list of integers and returns a list of the Fibonacci numbers in the list.
isFibonacci :: Int -> Bool
isFibonacci n = n == 0 || n == 1 || (isFibonacci (n - 1) && isFibonacci (n - 2))

getFibonaccis :: [Int] -> [Int]
getFibonaccis xs = filter isFibonacci xs

-- This function takes a list of integers and returns a list of the Catalan numbers in the list.
isCatalan :: Int -> Bool
isCatalan n = n == 0 || n == 1 || (isCatalan (n - 1) && isCatalan (n - 2) && n * (n + 1) `mod` 2 == 0)

getCatalans :: [Int] -> [Int]
getCatalans xs = filter isCatalan xs

-- This function takes a list of integers and returns a list of the Bell numbers in the list.
isBell :: Int -> Bool
isBell n = n == 0 || n == 1 || (isBell (n - 1) && sum [isBell (n - i - 1) * isBell i | i <- [0 .. n - 1]]) == n)

getBells :: [Int] -> [Int]
getBells xs = filter isBell xs

-- This function takes a list of integers and returns a list of the Stirling numbers of the first kind in the list.
isStirling1 :: Int -> Bool