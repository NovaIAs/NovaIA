```haskell
module ComplexCode where

import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

-- A type to represent a complex number
data Complex = Complex Double Double

-- Functions to create and manipulate complex numbers
add :: Complex -> Complex -> Complex
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

subtract :: Complex -> Complex -> Complex
subtract (Complex a b) (Complex c d) = Complex (a - c) (b - d)

multiply :: Complex -> Complex -> Complex
multiply (Complex a b) (Complex c d) = Complex (a * c - b * d) (a * d + b * c)

divide :: Complex -> Complex -> Maybe Complex
divide (Complex a b) (Complex c d)
  | d == 0 = Nothing
  | otherwise = Just $ Complex (a * c + b * d) (b * c - a * d) / (c ^ 2 + d ^ 2)

-- A type to represent a polynomial
data Polynomial = Polynomial [Complex]

-- Functions to create and manipulate polynomials
zero :: Polynomial
zero = Polynomial []

constant :: Complex -> Polynomial
constant c = Polynomial [c]

linear :: Complex -> Complex -> Polynomial
linear a b = Polynomial [a, b]

quadratic :: Complex -> Complex -> Complex -> Polynomial
quadratic a b c = Polynomial [a, b, c]

degree :: Polynomial -> Int
degree (Polynomial []) = -1
degree (Polynomial (c:cs)) = 1 + degree (Polynomial cs)

-- Function to evaluate a polynomial at a given point
evaluate :: Polynomial -> Complex -> Complex
evaluate (Polynomial []) _ = 0
evaluate (Polynomial (c:cs)) x = c + x * evaluate (Polynomial cs) x

-- Function to find the roots of a polynomial
roots :: Polynomial -> [Complex]
roots (Polynomial []) = []
roots (Polynomial (c:cs)) = catMaybes $ map (divide c) $ roots (Polynomial cs)

-- Main function
main :: IO ()
main = do
  let p = quadratic (Complex 1 0) (Complex 2 0) (Complex 1 0)
  print $ evaluate p (Complex 1 0)
  print $ roots p
```

This code defines a module called `ComplexCode` that provides functionality for working with complex numbers and polynomials.

The `Complex` data type represents a complex number with two components, a real part and an imaginary part. The `add`, `subtract`, `multiply`, and `divide` functions are defined to perform basic arithmetic operations on complex numbers.

The `Polynomial` data type represents a polynomial, which is a sum of terms of the form `a_n x^n`, where `a_n` are complex coefficients and `x` is a variable. The `zero`, `constant`, `linear`, and `quadratic` functions are defined to create polynomials of different degrees.

The `degree` function returns the degree of a polynomial, which is the highest exponent of `x` in any of its terms.

The `evaluate` function evaluates a polynomial at a given point, by substituting the value of `x` and computing the sum of the terms.

The `roots` function finds the roots of a polynomial, which are the values of `x` for which the polynomial evaluates to zero.

The `main` function creates a quadratic polynomial and prints its value at `x = 1` and its roots.