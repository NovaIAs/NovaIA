```haskell
-- Import necessary libraries
import Data.List (sort, group, sortBy, maximumBy)
import Data.Function (on)
import Data.Char (isDigit)
import Data.Ord (comparing)
import Data.Maybe (fromJust, isJust)
import Control.Monad (liftM, liftM2, ap, forM, mapM)
import Control.Applicative ((<*>))
import qualified Data.Map as Map

-- Define data types
data Ingredient = Ingredient String [String] deriving (Show)
data IngredientMap = IngredientMap [Ingredient] deriving (Show)
data Recipe = Recipe String [Ingredient] [String] deriving (Show)
data RecipeBook = RecipeBook [Recipe] deriving (Show)

-- Parse input data
parseIngredient :: String -> Ingredient
parseIngredient line = Ingredient (head parts) (tail parts)
    where parts = words line

parseRecipe :: [String] -> Recipe
parseRecipe (name:ingredients:instructions) =
    Recipe name (map parseIngredient ingredients) (tail instructions)

parseRecipeBook :: [String] -> RecipeBook
parseRecipeBook = RecipeBook . map parseRecipe . groupBy ((==) `on` head)

-- Helper functions
groupBy :: (a -> b) -> [a] -> [[a]]
groupBy f xs = go [] xs
    where go acc [] = reverse acc
          go acc (x:xs) = if f x == last acc then go (x:acc) xs else go [x] xs

maximumBy' :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy' f xs = maximumBy f xs <|> maximumBy' f (tail xs)

-- Calculate calories per ingredient
calculateCalories :: [Ingredient] -> IngredientMap
calculateCalories ingredients =
    IngredientMap $ map (\(ingredient, count) ->
        let calories = sum . map read . filter isDigit . words $ head count
        in Ingredient (head count) [show calories])
    $ groupBy ((==) `on` head) $ concatMap (\(Ingredient _ count) ->
        zip (map head count) count) ingredients

-- Find the most popular ingredient
mostPopularIngredient :: IngredientMap -> String
mostPopularIngredient =
    fromJust . maximumBy' (\(Ingredient _ count1) (Ingredient _ count2) -> compare (length count1) (length count2)) . Map.elems

-- Find the most popular ingredient in each recipe
mostPopularIngredientInRecipe :: Recipe -> String
mostPopularIngredientInRecipe recipe =
    fromJust . maximumBy' (\(Ingredient _ count1) (Ingredient _ count2) -> compare (length count1) (length count2)) $
    calculateCalories $ ingredients recipe

-- Find the recipe with the most popular ingredient
mostPopularRecipe :: RecipeBook -> String
mostPopularRecipe =
    fromJust . maximumBy' (\(Recipe _ ingredients1) (Recipe _ ingredients2) -> compare (length (mostPopularIngredientInRecipe $ Recipe "" ingredients1 [])) (length (mostPopularIngredientInRecipe $ Recipe "" ingredients2 []))) $
    recipes

-- Main function
main :: IO ()
main = do
    input <- readFile "input.txt"
    let recipeBook = parseRecipeBook $ lines input
    putStrLn $ "The most popular ingredient is: " ++ mostPopularIngredient (calculateCalories $ ingredients recipeBook)
    putStrLn $ "The recipe with the most popular ingredient is: " ++ mostPopularRecipe recipeBook
```

Explanation:

* The code parses input data from a text file, representing a recipe book.
* It defines data types for ingredients, recipes, and a recipe book.
* It calculates the calories per ingredient using regular expressions to extract numeric values from ingredient descriptions.
* It finds the most popular ingredient overall and in each recipe using helper functions for grouping and sorting.
* It identifies the recipe with the most popular ingredient.
* The main function reads the input file, parses the data, and prints the results.