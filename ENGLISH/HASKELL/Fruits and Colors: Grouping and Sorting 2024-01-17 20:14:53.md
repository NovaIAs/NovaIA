```haskell
-- Import necessary modules
import Data.List (sortBy, groupBy)
import Data.Char (toUpper)
import Data.Ord (comparing)

-- Define custom data types
data Fruit = Apple | Orange | Banana deriving (Eq, Ord, Enum, Show)
data Color = Red | Green | Yellow deriving (Eq, Ord, Enum, Show)

-- Create a list of fruits and colors
fruits :: [Fruit]
fruits = [Apple, Orange, Banana, Apple, Orange, Banana, Apple]

colors :: [Color]
colors = [Red, Green, Yellow, Red, Green, Yellow, Red]

-- Define a function to map fruits to their colors
fruitColors :: Fruit -> Color
fruitColors Apple = Red
fruitColors Orange = Orange
fruitColors Banana = Yellow

-- Define a function to group fruits by their colors
groupByColor :: [Fruit] -> [[Fruit]]
groupByColor = groupBy (\a b -> fruitColors a == fruitColors b)

-- Define a function to sort fruits by their names
sortByFruitName :: [Fruit] -> [Fruit]
sortByFruitName = sortBy (comparing toUpper . show)

-- Main function
main :: IO ()
main = do
  -- Print the original list of fruits
  putStrLn "Original list of fruits:"
  print fruits

  -- Print the original list of colors
  putStrLn "Original list of colors:"
  print colors

  -- Group fruits by their colors
  let groupedFruits = groupByColor fruits

  -- Print the grouped fruits
  putStrLn "Fruits grouped by their colors:"
  mapM_ print groupedFruits

  -- Sort fruits by their names
  let sortedFruits = sortByFruitName fruits

  -- Print the sorted fruits
  putStrLn "Fruits sorted by their names:"
  print sortedFruits
```

Explanation:

1. We first import necessary modules for list processing, character conversion, and sorting.

2. We define custom data types `Fruit` and `Color` to represent fruits and their colors.

3. We create two lists, `fruits` and `colors`, containing sample fruit names and colors.

4. We define a function `fruitColors` to map each fruit to its corresponding color.

5. We define a function `groupByColor` to group fruits by their colors using the `groupBy` function from `Data.List`.

6. We define a function `sortByFruitName` to sort fruits by their names using the `sortBy` function from `Data.Ord`.

7. In the `main` function:

   - We print the original lists of fruits and colors.

   - We group fruits by their colors using `groupByColor`.

   - We print the grouped fruits.

   - We sort fruits by their names using `sortByFruitName`.

   - We print the sorted fruits.

This code showcases various Haskell features, including custom data types, function mapping, list grouping and sorting, and modular code organization. Additionally, it demonstrates working with enumerations (`Fruit` and `Color`) and converting characters to uppercase for sorting.