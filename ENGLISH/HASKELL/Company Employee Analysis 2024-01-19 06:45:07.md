```haskell
-- Import necessary libraries
import Data.List (sort, nub)
import Data.Maybe (fromMaybe)
import Control.Monad (guard, when)

-- Define custom types
type Person = (String, Int) -- (name, age)
type Company = (String, [Person]) -- (name, employees)

-- Define helper functions
minAge :: Company -> Int
minAge (_, employees) = minimum $ map snd employees

maxAge :: Company -> Int
maxAge (_, employees) = maximum $ map snd employees

averageAge :: Company -> Double
averageAge (_, employees) = sum (map snd employees) / fromMaybe 1 (length employees)

-- Define the main function
main :: IO ()
main = do
  -- Create a list of companies
  let companies = [
        ("Acme Corporation", [("Alice", 25), ("Bob", 30), ("Carol", 28)]),
        ("XYZ Inc.", [("Dave", 22), ("Eve", 26), ("Frank", 32)]),
        ("Mega Corp.", [("Grace", 24), ("Harry", 29), ("Ian", 31)])
    ]

  -- Print the companies and their information
  putStrLn "Companies and their information:"
  mapM_ (\(name, employees) -> do
    putStrLn $ "Company Name: " ++ name
    putStrLn $ "Minimum Age: " ++ show (minAge (name, employees))
    putStrLn $ "Maximum Age: " ++ show (maxAge (name, employees))
    putStrLn $ "Average Age: " ++ show (averageAge (name, employees))
    putStrLn "" -- Print a newline
  ) companies

  -- Find the company with the youngest employees
  let youngestCompany = minimumBy (\(_, employees1) (_, employees2) -> compare (minAge (name, employees1)) (minAge (name, employees2))) companies
  putStrLn "Company with the Youngest Employees:"
  putStrLn $ "Company Name: " ++ fst youngestCompany
  putStrLn $ "Minimum Age: " ++ show (minAge youngestCompany)

  -- Find the company with the oldest employees
  let oldestCompany = maximumBy (\(_, employees1) (_, employees2) -> compare (maxAge (name, employees1)) (maxAge (name, employees2))) companies
  putStrLn "Company with the Oldest Employees:"
  putStrLn $ "Company Name: " ++ fst oldestCompany
  putStrLn $ "Maximum Age: " ++ show (maxAge oldestCompany)

  -- Find the companies with employees over the age of 30
  let companiesWithOldEmployees = filter (\(_, employees) -> any (>30) (map snd employees)) companies
  putStrLn "Companies with Employees Over the Age of 30:"
  mapM_ (\(name, employees) -> do
    putStrLn $ "Company Name: " ++ name
    putStrLn $ "Employees Over 30:"
    mapM_ (\(name, age) -> putStrLn $ name ++ " (" ++ show age ++ ")") $ filter (>30) employees
    putStrLn "" -- Print a newline
  ) companiesWithOldEmployees

  -- Find the employees with unique names
  let uniqueNames = nub $ concatMap (\(_, employees) -> map fst employees) companies
  putStrLn "Employees with Unique Names:"
  mapM_ putStrLn uniqueNames
```

This code is a complex and differentiated Haskell program that performs various operations on a list of companies and their employees. It calculates and prints information such as the minimum age, maximum age, and average age of employees in each company, as well as the company with the youngest and oldest employees. The code also finds companies with employees over the age of 30 and lists the employees with unique names. Here's a detailed explanation of the code:

1. **Importing Libraries**:
   - `import Data.List (sort, nub)`: Imports functions for sorting and removing duplicate elements from lists.
   - `import Data.Maybe (fromMaybe)`: Imports the `fromMaybe` function for handling optional values.
   - `import Control.Monad (guard, when)`: Imports monadic functions for conditional branching.

2. **Defining Custom Types**:
   - `Person = (String, Int)`: Defines a custom type `Person` as a tuple containing a person's name and age.
   - `Company = (String, [Person])`: Defines a custom type `Company` as a tuple containing a company's name and a list of its employees.

3. **Defining Helper Functions**:
   - `minAge :: Company -> Int`: Calculates the minimum age of employees in a company.
   - `maxAge :: Company -> Int`: Calculates the maximum age of employees in a company.
   - `averageAge :: Company -> Double`: Calculates the average age of employees in a company.

4. **Main Function**:
   - `main :: IO ()`: Defines the main function of the program.

5. **Creating a List of Companies**:
   - `let companies = ...`: Creates a list of companies with their employees using the `Company` type.

6. **Printing Company Information**:
   - The program iterates through the list of companies, printing each company's name, minimum age, maximum age, and average age.

7. **Finding the Youngest and Oldest Companies**:
   - `youngestCompany`: Finds the company with the youngest employees using `minimumBy`.
   - `oldestCompany`: Finds the company with the oldest employees using `maximumBy`.
   - The results are then printed.

8. **Finding Companies with Employees Over 30**:
   - `companiesWithOldEmployees`: Filters the list of companies to find those with at least one employee over the age of 30.
   - The program prints these companies and their employees over the age of 30.

9. **Finding Employees with Unique Names**:
   - The program concatenates the list of employees from all companies and extracts their unique names using `nub`.
   - The unique names are then printed.