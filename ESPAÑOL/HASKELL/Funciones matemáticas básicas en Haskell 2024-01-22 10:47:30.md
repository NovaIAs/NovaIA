```haskell
-- Definimos una función que recibe una lista de números y devuelve la suma de los cuadrados de los números de la lista.
sumaCuadrados :: [Int] -> Int
sumaCuadrados [] = 0
sumaCuadrados (x:xs) = x^2 + sumaCuadrados xs

-- Definimos una función que recibe un número y devuelve su factorial.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Definimos una función que recibe una lista de números y devuelve una lista con los factoriales de los números de la lista.
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- Definimos una función que recibe dos listas de números y devuelve una lista con las tuplas de los números de las dos listas.
zipConTuplas :: [Int] -> [Int] -> [(Int, Int)]
zipConTuplas [] _ = []
zipConTuplas _ [] = []
zipConTuplas (x:xs) (y:ys) = (x, y) : zipConTuplas xs ys

-- Definimos una función que recibe una lista de tuplas de números y devuelve una lista con los productos de los números de las tuplas.
productosDeTuplas :: [(Int, Int)] -> [Int]
productosDeTuplas [] = []
productosDeTuplas ((x, y):xs) = x * y : productosDeTuplas xs

-- Definimos una función que recibe una lista de números y devuelve una lista con los cuadrados de los números de la lista.
cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs

-- Definimos una función que recibe dos listas de números y devuelve una lista con las sumas de los números de las dos listas.
sumasDeListas :: [Int] -> [Int] -> [Int]
sumasDeListas [] _ = []
sumasDeListas _ [] = []
sumasDeListas (x:xs) (y:ys) = x + y : sumasDeListas xs ys

-- Definimos una función que recibe una lista de números y devuelve una lista con los números ordenados de menor a mayor.
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = ordenarMenoresQueX x xs ++ [x] ++ ordenarMayoresQueX x xs
  where
    ordenarMenoresQueX x [] = []
    ordenarMenoresQueX x (y:ys)
      | y < x = y : ordenarMenoresQueX x ys
      | otherwise = ordenarMenoresQueX x ys
    ordenarMayoresQueX x [] = []
    ordenarMayoresQueX x (y:ys)
      | y > x = y : ordenarMayoresQueX x ys
      | otherwise = ordenarMayoresQueX x ys

-- Definimos una función que recibe dos listas de números y devuelve una lista con la intersección de las dos listas.
interseccion :: [Int] -> [Int] -> [Int]
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) (y:ys)
  | x == y = x : interseccion xs ys
  | x < y = interseccion xs (y:ys)
  | otherwise = interseccion (x:xs) ys

-- Definimos una función que recibe dos listas de números y devuelve una lista con la unión de las dos listas.
union :: [Int] -> [Int] -> [Int]
union [] ys = ys
union xs [] = xs
union (x:xs) (y:ys)
  | x == y = x : union xs ys
  | x < y = x : union xs (y:ys)
  | otherwise = y : union (x:xs) ys
```

Explicación del código:

* La función `sumaCuadrados` recibe una lista de números y devuelve la suma de los cuadrados de los números de la lista.
* La función `factorial` recibe un número y devuelve su factorial.
* La función `factoriales` recibe una lista de números y devuelve una lista con los factoriales de los números de la lista.
* La función `zipConTuplas` recibe dos listas de números y devuelve una lista con las tuplas de los números de las dos listas.
* La función `productosDeTuplas` recibe una lista de tuplas de números y devuelve una lista con los productos de los números de las tuplas.
* La función `cuadrados` recibe una lista de números y devuelve una lista con los cuadrados de los números de la lista.
* La función `sumasDeListas` recibe dos listas de números y devuelve una lista con las sumas de los números de las dos listas.
* La función `ordenar` recibe una lista de números y devuelve una lista con los números ordenados de menor a mayor.
* La función `interseccion` recibe dos listas de números y devuelve una lista con la intersección de las dos listas.
* La función `union` recibe dos listas de números y devuelve una lista con la unión de las dos listas.