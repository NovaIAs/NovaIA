```haskell
module ComplexCode where

import Data.List (nub, sortBy)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Map as Map (Map, findWithDefault, insert, lookup, size)

-- Data structures

data Person = Person { name :: String, age :: Int } deriving (Show)

data Company = Company { name :: String, employees :: [Person] } deriving (Show)

-- Functions

findOldestEmployee :: Company -> Maybe Person
findOldestEmployee (Company _ employees) = maximumBy (compare `on` age) employees

averageAge :: Company -> Double
averageAge (Company _ employees) = sum (map age employees) / fromIntegral (length employees)

countEmployees :: Company -> Int
countEmployees (Company _ employees) = length employees

groupEmployeesByName :: Company -> Map.Map String [Person]
groupEmployeesByName (Company _ employees) =
  Map.fromListWith (++) [(name person, [person]) | person <- employees]

sortEmployeesByAge :: Company -> Company
sortEmployeesByAge (Company name employees) =
  Company name (sortBy (compare `on` age) employees)

-- Main function

main :: IO ()
main = do
  let employees =
        [ Person "Alice" 25
        , Person "Bob" 30
        , Person "Charlie" 28
        , Person "Dave" 22
        , Person "Eve" 27
        ]

  let company = Company "Acme Corp" employees

  putStrLn "Oldest employee:"
  print $ findOldestEmployee company

  putStrLn "Average age:"
  print $ averageAge company

  putStrLn "Number of employees:"
  print $ countEmployees company

  putStrLn "Employees grouped by name:"
  print $ groupEmployeesByName company

  putStrLn "Employees sorted by age:"
  print $ sortEmployeesByAge company

-- Explanation

The code defines a module named `ComplexCode`, which contains several functions and data structures.

The data structure `Person` represents a person with a name and an age.

The data structure `Company` represents a company with a name and a list of employees.

The function `findOldestEmployee` finds the oldest employee in a company.

The function `averageAge` calculates the average age of the employees in a company.

The function `countEmployees` counts the number of employees in a company.

The function `groupEmployeesByName` groups the employees in a company by name.

The function `sortEmployeesByAge` sorts the employees in a company by age.

The main function creates a company with a list of employees and then calls the other functions to print out various information about the company.

The code demonstrates how to use Haskell to define and work with complex data structures and functions.
```