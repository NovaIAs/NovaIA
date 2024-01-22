```haskell
-- Definimos una función que recibe una lista de números y devuelve la suma de los mismos
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

-- Definimos una función que recibe una lista de números y devuelve el producto de los mismos
producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Definimos una función que recibe una lista de números y devuelve el máximo de los mismos
maximo :: [Int] -> Int
maximo [] = 0
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

-- Definimos una función que recibe una lista de números y devuelve el mínimo de los mismos
minimo :: [Int] -> Int
minimo [] = 0
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- Definimos una función que recibe una lista de números y devuelve la media de los mismos
media :: [Int] -> Int
media [] = 0
media [x] = x
media (x:xs) = (x + media xs) `div` 2

-- Definimos una función que recibe una lista de números y devuelve la varianza de los mismos
varianza :: [Int] -> Int
varianza [] = 0
varianza [x] = 0
varianza (x:xs) = (sum (map (\x -> (x - media xs)^2) (x:xs))) `div` (length xs)

-- Definimos una función que recibe una lista de números y devuelve la desviación estándar de los mismos
desviacionEstandar :: [Int] -> Int
desviacionEstandar xs = sqrt (varianza xs)

-- Definimos una función que recibe una lista de números y devuelve una lista de los números pares de la lista
pares :: [Int] -> [Int]
pares [] = []
pares [x] = if even x then [x] else []
pares (x:xs) = if even x then x:pares xs else pares xs

-- Definimos una función que recibe una lista de números y devuelve una lista de los números impares de la lista
impares :: [Int] -> [Int]
impares [] = []
impares [x] = if odd x then [x] else []
impares (x:xs) = if odd x then x:impares xs else impares xs

-- Definimos una función que recibe una lista de números y devuelve una lista de los números primos de la lista
primos :: [Int] -> [Int]
primos [] = []
primos [x] = if primo x then [x] else []
primos (x:xs) = if primo x then x:primos xs else primos xs
  where
    primo :: Int -> Bool
    primo 0 = False
    primo 1 = False
    primo x = all (\i -> x `mod` i /= 0) [2..x-1]

-- Definimos una función que recibe una lista de números y devuelve una lista de los números perfectos de la lista
perfectos :: [Int] -> [Int]
perfectos [] = []
perfectos [x] = if perfecto x then [x] else []
perfectos (x:xs) = if perfecto x then x:perfectos xs else perfectos xs
  where
    perfecto :: Int -> Bool
    perfecto 0 = False
    perfecto 1 = False
    perfecto x = sum (divisores x) == 2*x
      where
        divisores :: Int -> [Int]
        divisores 0 = []
        divisores 1 = []
        divisores x = [i | i <- [1..x-1], x `mod` i == 0]

-- Definimos una función que recibe una lista de números y devuelve una lista de los números amigos de la lista
amigos :: [Int] -> [Int]
amigos [] = []
amigos [x] = if amigo x then [x] else []
amigos (x:xs) = if amigo x then x:amigos xs else amigos xs
  where
    amigo :: Int -> Bool
    amigo 0 = False
    amigo 1 = False
    amigo x = sum (properDivisors x) == x
      where
        properDivisors :: Int -> [Int]
        properDivisors 0 = []
        properDivisors 1 = []
        properDivisors x = [i | i <- [1..x-1], x `mod` i == 0]

-- Imprimimos los resultados de las funciones definidas anteriormente
putStrLn ("Suma: " ++ show (suma [1, 2, 3, 4, 5]))
putStrLn ("Producto: " ++ show (producto [1, 2, 3, 4, 5]))
putStrLn ("Máximo: " ++ show (maximo [1, 2, 3, 4, 5]))
putStrLn ("Mínimo: " ++ show (minimo [1, 2, 3, 4, 5]))
putStrLn ("Media: " ++ show (media [1, 2, 3, 4, 5]))
putStrLn ("Varianza: " ++ show (varianza [1, 2, 3, 4, 5]))
putStrLn ("Desviación estándar: " ++ show (desviacionEstandar [1, 2, 3, 4, 5]))
putStrLn ("Pares: " ++ show (pares [1, 2, 3, 4, 5]))
putStrLn ("Impares: " ++ show (impares [1, 2, 3, 4, 5]))
putStrLn ("Primos: " ++ show (primos [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
putStrLn ("Perfectos: " ++ show (perfectos [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
putStrLn ("Amigos: " ++ show (amigos [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
```

Explicación del código:

* Definimos una serie de funciones que reciben una lista de números y devuelven un resultado determinado. Por ejemplo, la función `suma` devuelve la suma de los números de la lista, la función `producto` devuelve el producto de los números de la lista, la función `maximo` devuelve el máximo de los números de la lista, etc.


* Utilizamos la función `map` para aplicar una función a cada elemento de una lista. Por ejemplo, en la expresión `map (\x -> x^2) [1, 2, 3, 4, 5]` aplicamos la función `(\x -> x^2)` a cada elemento de la lista `[1, 2, 3, 4, 5]`, obteniendo la lista `[1, 4, 9, 16, 25]`.


* Utilizamos la función `filter` para seleccionar los elementos de una lista que satisfacen una determinada condición. Por ejemplo, en la expresión `filter (\x -> x > 3) [1, 2, 3, 4, 5]` seleccionamos los elementos de la lista `[1, 2, 3, 4, 5]` que son mayores que 3, obteniendo la lista `[4, 5]`.


* Utilizamos la función `foldr` para reducir una lista a un único valor. Por ejemplo, en la expresión `foldr (+) 0 [1, 2, 3, 4, 5]` reducimos la lista `[1, 2, 3, 4, 5]` a un único valor utilizando la función `(+)`. El resultado es 15.


* Utilizamos la función `zipWith` para aplicar una función a dos listas simultáneamente. Por ejemplo, en la expresión `zipWith (+) [1, 2, 3] [4, 5, 6]` aplicamos la función `(+)` a cada par de elementos de las listas `[1, 2, 3]` y `[4, 5, 6]`, obteniendo la lista `[5, 7, 9]`.