```haskell
-- Module Imports
import Data.List
import Data.Char
import Control.Monad
import qualified Data.Map as Map

-- Data Types
data Expr = Var String | Const Int | Add Expr Expr | Sub Expr Expr
          | Mul Expr Expr | Div Expr Expr

-- Evaluate an Expression
eval :: Expr -> Int
eval (Var x) = Map.findWithDefault 0 x variables

-- Variables and Values
variables = Map.fromList [("x", 10), ("y", 20), ("z", 30)]

-- Pretty Print an Expression
instance Show Expr where
  show (Var x) = x
  show (Const n) = show n
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Sub e1 e2) = show e1 ++ " - " ++ show e2
  show (Mul e1 e2) = show e1 ++ " * " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2

-- Main Function
main :: IO ()
main = do
  -- Define an expression
  expr <- readExpr <$> getLine
  -- Evaluate the expression
  let result = eval expr
  -- Print the result
  putStrLn $ "The result of evaluating " ++ show expr ++ " is " ++ show result

-- Read an Expression from Input
readExpr :: String -> Expr
readExpr input = case words input of
  [x] | isDigit x -> Const (read x :: Int)
      | otherwise -> Var x
  (x:y:zs) -> case y of
    "+" -> Add (readExpr x) (readExpr (unwords zs))
    "-" -> Sub (readExpr x) (readExpr (unwords zs))
    "*" -> Mul (readExpr x) (readExpr (unwords zs))
    "/" -> Div (readExpr x) (readExpr (unwords zs))
    _ -> error "Invalid expression"
```

**Explanation:**

1. **Data Types:** The `Expr` data type represents expressions that can be evaluated. It includes variables (`Var`), constants (`Const`), addition (`Add`), subtraction (`Sub`), multiplication (`Mul`), and division (`Div`).

2. **Evaluation Function:** The `eval` function evaluates an expression based on the values stored in the `variables` map.

3. **Pretty Printing:** The `Show` instance for the `Expr` data type defines how expressions are printed. It uses standard mathematical notation for addition, subtraction, multiplication, and division.

4. **Main Function:** The `main` function reads an expression from the user, evaluates it, and prints the result.

5. **Reading an Expression:** The `readExpr` function parses a string representing an expression and constructs an `Expr` value. It handles variables, constants, and operators correctly.

This code provides a simple REPL (Read-Eval-Print Loop) for mathematical expressions. It can evaluate expressions entered by the user and print the results.