```haskell
-- Definimos una función que recibe una lista de números y devuelve la suma de los mismos.
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

-- Definimos una función que recibe una lista de números y devuelve la media de los mismos.
media :: [Int] -> Int
media xs = suma xs `div` length xs

-- Definimos una función que recibe una lista de números y devuelve la desviación estándar de los mismos.
desviacionEstandar :: [Int] -> Int
desviacionEstandar xs = sqrt (varianza xs)

-- Definimos una función que recibe una lista de números y devuelve la varianza de los mismos.
varianza :: [Int] -> Int
varianza xs = suma (map (\x -> (x - media xs)^2) xs) `div` length xs

-- Definimos una función que recibe una lista de números y devuelve el valor máximo de los mismos.
maximo :: [Int] -> Int
maximo [] = error "La lista está vacía"
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

-- Definimos una función que recibe una lista de números y devuelve el valor mínimo de los mismos.
minimo :: [Int] -> Int
minimo [] = error "La lista está vacía"
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- Definimos una función que recibe una lista de números y devuelve una lista con los números ordenados de menor a mayor.
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = ordenar [y | y <- xs, y < x] ++ [x] ++ ordenar [y | y <- xs, y >= x]

-- Definimos una función que recibe una lista de números y devuelve una lista con los números ordenados de mayor a menor.
ordenarDescendente :: [Int] -> [Int]
ordenarDescendente [] = []
ordenarDescendente [x] = [x]
ordenarDescendente (x:xs) = ordenarDescendente [y | y <- xs, y > x] ++ [x] ++ ordenarDescendente [y | y <- xs, y <= x]

-- Definimos una función que recibe una lista de números y devuelve una lista con los números impares de la misma.
impares :: [Int] -> [Int]
impares [] = []
impares [x] = if x `mod` 2 == 1 then [x] else []
impares (x:xs) = if x `mod` 2 == 1 then [x] ++ impares xs else impares xs

-- Definimos una función que recibe una lista de números y devuelve una lista con los números pares de la misma.
pares :: [Int] -> [Int]
pares [] = []
pares [x] = if x `mod` 2 == 0 then [x] else []
pares (x:xs) = if x `mod` 2 == 0 then [x] ++ pares xs else pares xs

-- Definimos una función que recibe una lista de números y devuelve una lista con los números primos de la misma.
primos :: [Int] -> [Int]
primos [] = []
primos [x] = if x > 1 then [x] else []
primos (x:xs) = if esPrimo x then [x] ++ primos xs else primos xs
  where
    esPrimo :: Int -> Bool
    esPrimo n | n <= 1 = False
              | n == 2 = True
              | n `mod` 2 == 0 = False
              | otherwise = not $ any (\x -> n `mod` x == 0) [3,5..ceiling (sqrt (fromIntegral n))]

-- Definimos una función que recibe una lista de números y devuelve una lista con los números perfectos de la misma.
perfectos :: [Int] -> [Int]
perfectos [] = []
perfectos [x] = if esPerfecto x then [x] else []
perfectos (x:xs) = if esPerfecto x then [x] ++ perfectos xs else perfectos xs
  where
    esPerfecto :: Int -> Bool
    esPerfecto n = n > 0 && n == suma (divisores n)
      where
        divisores :: Int -> [Int]
        divisores n = [x | x <- [1..n-1], n `mod` x == 0]
```

Este código define una serie de funciones que realizan operaciones matemáticas básicas con listas de números, como sumar, calcular la media, la desviación estándar, el máximo, el mínimo, ordenar, filtrar números impares, pares, primos y perfectos.