```haskell
-- Function to find the sum of a list of numbers
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Function to find the maximum of a list of numbers
maxList :: [Int] -> Int
maxList [] = error "Empty list"
maxList [x] = x
maxList (x:xs) = max x (maxList xs)

-- Function to find the minimum of a list of numbers
minList :: [Int] -> Int
minList [] = error "Empty list"
minList [x] = x
minList (x:xs) = min x (minList xs)

-- Function to find the average of a list of numbers
avgList :: [Int] -> Double
avgList [] = error "Empty list"
avgList xs = sumList xs / fromIntegral (length xs)

-- Function to find the median of a list of numbers
medianList :: [Int] -> Double
medianList [] = error "Empty list"
medianList [x] = fromIntegral x
medianList xs =
  let sorted = sort xs
      n = length xs
  in if n `mod` 2 == 0
     then (fromIntegral (sorted !! (n `div` 2 - 1)) + fromIntegral (sorted !! (n `div` 2))) / 2
     else fromIntegral (sorted !! (n `div` 2))

-- Function to find the mode of a list of numbers
modeList :: [Int] -> [Int]
modeList [] = []
modeList xs =
  let counts = foldl (\acc x -> acc ++ [(x, 1)]) [] xs
      maxCount = maximum (map snd counts)
      modes = filter (\(x, y) -> y == maxCount) counts
  in map fst modes

-- Function to find the range of a list of numbers
rangeList :: [Int] -> Int
rangeList [] = error "Empty list"
rangeList [x] = 0
rangeList xs = maxList xs - minList xs

-- Function to find the standard deviation of a list of numbers
stdDevList :: [Int] -> Double
stdDevList [] = error "Empty list"
stdDevList xs =
  let mean = avgList xs
      squaredDiffs = map (\x -> (x - mean) ** 2) xs
      variance = sumList squaredDiffs / fromIntegral (length xs)
  in sqrt variance
```

This code contains a collection of functions for performing various statistical operations on a list of numbers. These functions are:

1. `sumList`: Calculates the sum of all numbers in a list.

2. `maxList`: Finds the maximum value in a list.

3. `minList`: Finds the minimum value in a list.

4. `avgList`: Calculates the average (mean) of all numbers in a list.

5. `medianList`: Finds the median value in a list (middle value when sorted).

6. `modeList`: Identifies the most frequently occurring value (mode) in a list.

7. `rangeList`: Calculates the difference between the maximum and minimum values in a list.

8. `stdDevList`: Computes the standard deviation of a list, which measures how spread out the data is.

This code can be used for various statistical analysis tasks, such as summarizing data, identifying trends, and making inferences about a population.