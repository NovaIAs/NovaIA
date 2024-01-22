```haskell
-- Definición de la función factorial

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Definición de la función de Fibonacci

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Definición de la función de suma de cuadrados

sumaCuadrados :: [Int] -> Int
sumaCuadrados [] = 0
sumaCuadrados (x:xs) = x^2 + sumaCuadrados xs

-- Definición de la función de producto de una lista

producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Definición de la función de máximo común divisor

mcd :: Int -> Int -> Int
mcd a b | b == 0 = a
       | otherwise = mcd b (a `mod` b)

-- Definición de la función de mínimo común múltiplo

mcm :: Int -> Int -> Int
mcm a b = a * b `div` mcd a b

-- Definición de la función de ordenación por burbuja

ordenacionBurbuja :: [Int] -> [Int]
ordenacionBurbuja [] = []
ordenacionBurbuja (x:xs) = minimo : ordenacionBurbuja (filter (/= minimo) xs)
  where minimo = head xs

-- Definición de la función de búsqueda binaria

busquedaBinaria :: Int -> [Int] -> Int
busquedaBinaria _ [] = -1
busquedaBinaria x (y:ys)
  | x == y    = 0
  | x < y     = busquedaBinaria x ys
  | otherwise = 1 + busquedaBinaria x ys
```

Este código implementa varias funciones matemáticas y algorítmicas comunes en Haskell. Las funciones son:

* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula el número de Fibonacci en una posición dada.
* `sumaCuadrados`: Calcula la suma de los cuadrados de los elementos de una lista.
* `producto`: Calcula el producto de los elementos de una lista.
* `mcd`: Calcula el máximo común divisor de dos números.
* `mcm`: Calcula el mínimo común múltiplo de dos números.
* `ordenacionBurbuja`: Ordena una lista de números usando el algoritmo de ordenación por burbuja.
* `busquedaBinaria`: Busca un elemento en una lista ordenada usando el algoritmo de búsqueda binaria.

El código está escrito en Haskell, un lenguaje de programación funcional puro. Haskell es un lenguaje muy expresivo y conciso, lo que lo hace ideal para escribir código matemático y algorítmico.