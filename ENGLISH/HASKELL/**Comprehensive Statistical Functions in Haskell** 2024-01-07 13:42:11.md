```haskell
module ComplexCode where

import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Tuple (swap)

-- Define a type alias for a function that takes two arguments of type `a`
-- and returns a result of type `b`.
type Function a b = a -> b

-- Define a function that takes a function `f` and a list `xs` and returns
-- a list of the results of applying `f` to each element of `xs`.
map' :: Function a b -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- Define a function that takes a list of numbers and returns the sum of the
-- numbers in the list.
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- Define a function that takes a list of numbers and returns the product of
-- the numbers in the list.
product' :: [Int] -> Int
product' = foldr (*) 1

-- Define a function that takes a list of numbers and returns the maximum
-- number in the list.
maximum' :: [Int] -> Int
maximum' = foldr max minBound

-- Define a function that takes a list of numbers and returns the minimum
-- number in the list.
minimum' :: [Int] -> Int
minimum' = foldr min maxBound

-- Define a function that takes a list of numbers and returns the average of
-- the numbers in the list.
average' :: [Int] -> Double
average' = (/ fromMaybe 0 (length xs)) . sum'
  where xs = nub . sort $ xs

-- Define a function that takes a list of numbers and returns the median of
-- the numbers in the list.
median' :: [Int] -> Double
median' xs = average' $ select ((length xs + 1) `div` 2) xs
  where select n xs = snd $ foldr select' ([], xs) xs
        select' x (ys, zs) = if n <= length ys then (x:ys, zs) else (ys, x:zs)

-- Define a function that takes a list of numbers and returns the mode of
-- the numbers in the list.
mode' :: [Int] -> [Int]
mode' xs = map fst $ filter ((== maximum') . snd) $ group $ sort xs
  where group = foldr step []
        step x [] = [(x, 1)]
        step x ((y, n):ys)
          | x == y = (x, n+1):ys
          | otherwise = (y, n):(x, 1):ys

-- Define a function that takes a list of numbers and returns the range of
-- the numbers in the list.
range' :: [Int] -> Int
range' xs = maximum' xs - minimum' xs

-- Define a function that takes a list of numbers and returns the variance of
-- the numbers in the list.
variance' :: [Int] -> Double
variance' xs = average' $ map (^2) $ map' subtract' average' xs
  where subtract' = flip (-)

-- Define a function that takes a list of numbers and returns the standard
-- deviation of the numbers in the list.
standardDeviation' :: [Int] -> Double
standardDeviation' = sqrt . variance'

-- Define a function that takes a list of numbers and returns the covariance
-- of the numbers in the list.
covariance' :: [Int] -> [Int] -> Double
covariance' xs ys = average' $ map' multiply' $ zipWith subtract' average' xs ys
  where multiply' = uncurry (*)

-- Define a function that takes a list of numbers and returns the correlation
-- coefficient of the numbers in the list.
correlationCoefficient' :: [Int] -> [Int] -> Double
correlationCoefficient' xs ys = covariance' xs ys / (standardDeviation' xs * standardDeviation' ys)

-- Define a function that takes two lists of numbers and returns the linear
-- regression line of the two lists.
linearRegression' :: [Int] -> [Int] -> (Double, Double)
linearRegression' xs ys = (gradient, intercept)
  where n = fromIntegral $ length xs
        sumX = sum' xs
        sumY = sum' ys
        sumX2 = sum' $ map (^2) xs
        sumY2 = sum' $ map (^2) ys
        sumXY = sum' $ map' multiply' $ zipWith subtract' average' xs ys
        gradient = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX^2)
        intercept = (sumY - gradient * sumX) / n

-- Define a function that takes a list of numbers and returns the exponential
-- regression line of the list.
exponentialRegression' :: [Int] -> [Int] -> (Double, Double)
exponentialRegression' xs ys = (gradient, intercept)
  where n = fromIntegral $ length xs
        sumX = sum' xs
        sumY = sum' ys
        sumX2 = sum' $ map (^2) xs
        sumLogY = sum' $ map log ys
        sumXLogY = sum' $ map' multiply' $ zipWith subtract' average' xs $ map log ys
        gradient = (n * sumXLogY - sumX * sumLogY) / (n * sumX2 - sumX^2)
        intercept = (sumLogY - gradient * sumX) / n

-- Define a function that takes a list of numbers and returns the logarithmic
-- regression line of the list.
logarithmicRegression' :: [Int] -> [Int] -> (Double, Double)
logarithmicRegression' xs ys = (gradient, intercept)
  where n = fromIntegral $ length xs
        sumX = sum' xs
        sumY = sum' ys
        sumLogX = sum' $ map log xs
        sumYLogX = sum' $ map' multiply' $ zipWith subtract' average' $ map log xs ys
        gradient = (n * sumYLogX - sumLogX * sumY) / (n * sumLogX^2 - sumLogX^2)
        intercept = (sumY - gradient * sumLogX) / n

-- Define a function that takes a list of numbers and returns the power
-- regression line of the list.
powerRegression' :: [Int] -> [Int] -> (Double, Double)
powerRegression' xs ys = (gradient, intercept)
  where n = fromIntegral $ length xs
        sumX = sum' xs
        sumY = sum' ys
        sumX2 = sum' $ map (^2) xs
        sumLogY = sum' $ map log ys
        sumXLogY = sum' $ map' multiply' $ zipWith subtract' average' xs $ map log ys
        gradient = (n * sumXLogY - sumX * sumLogY) / (n * sumX2 - sumX^2)
        intercept = (sumLogY - gradient * sumX) / n

-- Define a function that takes a list of numbers and returns the polynomial
-- regression line of the list.
polynomialRegression' :: [Int] -> [Int] -> Int -> [(Double, Double)]
polynomialRegression' xs ys degree = map (\i -> (fromIntegral i, coefficient)) [0..degree]
  where coefficients = polyfit xs ys degree
        polyfit :: [Int] -> [Int] -> Int -> [Double]
        polyfit xs ys degree =
          let A = moments xs degree
              b = moments' xs ys degree
          in solve A b
        moments :: [Int] -> Int -> [[Double]]
        moments xs degree = map (\i -> map (^i) xs) [0..degree]
        moments' :: [Int] -> [Int] -> Int -> [Double]
        moments' xs ys degree = map' sum' $ zipWith multiply' $ zipWith zip $ moments xs degree ys
        solve :: [[Double]] -> [Double] -> [Double]
        solve A b = foldr step b A
          where step (a:A) b = a - sum' $ zipWith multiply' A b
        multiply' :: [Double] -> [Double] -> [Double]
        multiply' xs ys = zipWith (*) xs ys
```

The code above is a comprehensive collection of statistical functions written in Haskell. It includes functions for calculating the sum, product, maximum, minimum, average, median, mode, range, variance, standard deviation, covariance, correlation coefficient, linear regression line, exponential regression line, logarithmic regression line, power regression line, and polynomial regression line. The code is well-documented and uses a functional programming style.

Here are some examples of how to use the code:

```haskell
-- Calculate the sum of the numbers in the list [1, 2, 3, 4, 5]
let sum = sum' [1, 2, 3, 4, 5]
-- Output: 15

-- Calculate the product of the numbers in the list [1, 2, 3, 4, 5]
let product = product' [1, 2