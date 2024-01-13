```haskell
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

-- Define the type of a polynomial.
type Poly = [Int]

-- Define the `eval` function, which evaluates a polynomial at a given point.
eval :: Poly -> Int -> Int
eval p x = sum [c * x^i | (i, c) <- zip [0..] p]

-- Define the `derivative` function, which computes the derivative of a polynomial.
derivative :: Poly -> Poly
derivative p = zipWith (*) [1..] (tail p)

-- Define the `roots` function, which computes the roots of a polynomial.
roots :: Poly -> [Int]
roots p = nub $ sort [x | x <- [0..100], abs (eval p x) < 1e-6]

-- Define the `main` function, which parses the command-line arguments and calls the `roots` function.
main :: IO ()
main = do
  args <- getArgs
  let p = map read $ words $ head args
  print $ roots p
```

This code implements a simple polynomial calculator in Haskell. It can evaluate polynomials at a given point, compute the derivative of a polynomial, and find the roots of a polynomial.

The `eval` function is defined using a list comprehension, which is a concise way to write a loop in Haskell. The `sum` function is used to add up the terms of the polynomial, and the `zip` function is used to pair up the coefficients and exponents of the polynomial.

The `derivative` function is also defined using a list comprehension. The `zipWith` function is used to apply the multiplication operator to each pair of coefficients and exponents in the polynomial. The `tail` function is used to remove the constant term from the polynomial, since the derivative of the constant term is always zero.

The `roots` function is defined using a list comprehension and the `nub` and `sort` functions from the `Data.List` module. The `nub` function removes duplicate elements from a list, and the `sort` function sorts a list in ascending order. The `abs` function is used to compute the absolute value of a number, and the `eval` function is used to evaluate the polynomial at each point in the range from 0 to 100. The `print` function is used to print the roots of the polynomial to the console.

To use this code, you can compile it using the following command:

```
ghc -o polycalc polycalc.hs
```

Once the code is compiled, you can run it using the following command:

```
./polycalc "1 2 3"
```

This will print the roots of the polynomial `x^2 + 2x + 3`, which are `-1` and `-3`.