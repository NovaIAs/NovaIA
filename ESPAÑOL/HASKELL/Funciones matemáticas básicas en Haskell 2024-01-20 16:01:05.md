```haskell

-- Definir una función que calcula el factorial de un número

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Definir una función que calcula el valor absoluto de un número

absoluto :: Int -> Int
absoluto x
  | x >= 0 = x
  | otherwise = -x

-- Definir una función que calcula el máximo común divisor de dos números

mcd :: Int -> Int -> Int
mcd x y
  | y == 0 = abs (x)
  | otherwise = mcd y (x `mod` y)

-- Definir una función que calcula el mínimo común múltiplo de dos números

mcm :: Int -> Int -> Int
mcm x y = abs (x * y) `div` gcd x y

-- Definir una función que calcula la suma de los primeros n números naturales

suma :: Int -> Int
suma 0 = 0
suma n = n + suma (n - 1)

-- Definir una función que calcula el producto de los primeros n números naturales

producto :: Int -> Int
producto 0 = 1
producto n = n * producto (n - 1)

-- Definir una función que calcula la secuencia de Fibonacci hasta el término n

fibonacci :: Int -> [Int]
fibonacci 0 = [0]
fibonacci 1 = [0, 1]
fibonacci n = fibonacci (n - 1) ++ [last (fibonacci (n - 1)) + (last (fibonacci (n - 2)))]

-- Definir una función que calcula la secuencia de los primeros n números primos

primos :: Int -> [Int]
primos 1 = []
primos n = criba [2..n]

criba :: [Int] -> [Int]
criba [] = []
criba (x:xs) = x : criba (filter (\y -> y `mod` x /= 0) xs)

-- Definir una función que calcula el logaritmo natural de un número

logaritmo :: Double -> Double
logaritmo x = fromIntegral (log x) / fromIntegral (log 2)

-- Definir una función que calcula la exponencial de un número

exponencial :: Double -> Double
exponencial x = 2 ^ x

```