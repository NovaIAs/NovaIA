```haskell
import Data.List (foldr', groupBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)

-- Define a type alias for a list of integers.
type IntList = [Int]

-- Define a function to find the maximum value in a list of integers.
maximum' :: IntList -> Int
maximum' = foldr' max 0

-- Define a function to find the minimum value in a list of integers.
minimum' :: IntList -> Int
minimum' = foldr' min (maxBound :: Int)

-- Define a function to find the mean (average) value in a list of integers.
mean :: IntList -> Maybe Double
mean = fmap (fromIntegral . sum / fromIntegral . length)

-- Define a function to find the median value in a list of integers.
median :: IntList -> Maybe Double
median = fmap (fromJust . mean . groupBy compare) . sort

-- Define a function to find the mode (most frequently occurring value) in a list of integers.
mode :: IntList -> Maybe Int
mode = fmap (head . fromJust . maximumBy (comparing length)) . groupBy id . sort

-- Define a function to find the range (difference between maximum and minimum values) in a list of integers.
range :: IntList -> Maybe Int
range = fmap (maximum' - minimum')

-- Define a function to find the standard deviation in a list of integers.
standardDeviation :: IntList -> Maybe Double
standardDeviation xs = do
  m <- mean xs
  let diffs = map (\x -> (x - m) ^ 2) xs
  sqrt . fromIntegral $ sum diffs / fromIntegral (length xs - 1)

-- Define a function to find the variance in a list of integers.
variance :: IntList -> Maybe Double
variance = fmap ((standardDeviation)^2)

-- Define a function to find the covariance between two lists of integers.
covariance :: IntList -> IntList -> Maybe Double
covariance xs ys = do
  mx <- mean xs
  my <- mean ys
  let diffsX = zipWith (-) xs (repeat mx)
      diffsY = zipWith (-) ys (repeat my)
      products = zipWith (*) diffsX diffsY
  pure $ sum products / fromIntegral (length xs - 1)

-- Define a function to find the correlation coefficient between two lists of integers.
correlationCoefficient :: IntList -> IntList -> Maybe Double
correlationCoefficient xs ys = do
  cov <- covariance xs ys
  sdevX <- standardDeviation xs
  sdevY <- standardDeviation ys
  pure $ cov / (sdevX * sdevY)

-- Example usage:
nums1 = [1, 2, 3, 4, 5]
nums2 = [6, 7, 8, 9, 10]

putStrLn $ "Maximum of nums1: " ++ show (maximum' nums1)
putStrLn $ "Minimum of nums1: " ++ show (minimum' nums1)
putStrLn $ "Mean of nums1: " ++ show (mean nums1)
putStrLn $ "Median of nums1: " ++ show (median nums1)
putStrLn $ "Mode of nums1: " ++ show (mode nums1)
putStrLn $ "Range of nums1: " ++ show (range nums1)
putStrLn $ "Standard deviation of nums1: " ++ show (standardDeviation nums1)
putStrLn $ "Variance of nums1: " ++ show (variance nums1)
putStrLn $ "Covariance of nums1 and nums2: " ++ show (covariance nums1 nums2)
putStrLn $ "Correlation coefficient of nums1 and nums2: " ++ show (correlationCoefficient nums1 nums2)
```

Explanation:

* The code defines a number of functions for performing statistical operations on lists of integers.
* These functions include finding the maximum, minimum, mean, median, mode, range, standard deviation, variance, covariance, and correlation coefficient.
* The code uses Haskell's list processing and higher-order functions to perform these calculations efficiently and concisely.
* The example usage at the end of the code shows how to use these functions to calculate statistical measures for a given list of integers.