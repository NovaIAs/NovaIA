```haskell
-- Esta función toma una lista de números y devuelve el máximo
máximo :: [Int] -> Int
máximo [] = error "La lista está vacía"
máximo [x] = x
máximo (x:xs)
    | x > máximo xs = x
    | otherwise = máximo xs

-- Esta función toma una lista de números y devuelve la suma de todos los elementos de la lista
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

-- Esta función toma una lista de números y devuelve el producto de todos los elementos de la lista
producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Esta función toma una lista de números y devuelve el número de elementos de la lista
longitud :: [Int] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Esta función toma una lista de números y devuelve el número de elementos impares de la lista
impares :: [Int] -> Int
impares [] = 0
impares (x:xs)
    | x `mod` 2 == 1 = 1 + impares xs
    | otherwise = impares xs

-- Esta función toma una lista de números y devuelve el número de elementos pares de la lista
pares :: [Int] -> Int
pares [] = 0
pares (x:xs)
    | x `mod` 2 == 0 = 1 + pares xs
    | otherwise = pares xs

-- Esta función toma una lista de números y devuelve la media de todos los elementos de la lista
media :: [Int] -> Float
media [] = error "La lista está vacía"
media xs = sum xs / fromIntegral (length xs)

-- Esta función toma una lista de números y devuelve la varianza de todos los elementos de la lista
varianza :: [Int] -> Float
varianza [] = error "La lista está vacía"
varianza xs = sum (map (\x -> (x - media xs)^2) xs) / fromIntegral (length xs - 1)

-- Esta función toma una lista de números y devuelve la desviación estándar de todos los elementos de la lista
desviación_estándar :: [Int] -> Float
desviación_estándar = sqrt . varianza

-- Esta función toma una lista de números y devuelve la mediana de todos los elementos de la lista
mediana :: [Int] -> Int
mediana [] = error "La lista está vacía"
mediana xs = sort xs !! (length xs `div` 2)

-- Esta función toma una lista de números y devuelve la moda de todos los elementos de la lista
moda :: [Int] -> Int
moda [] = error "La lista está vacía"
moda xs = head (maximumBy (\(a, b) -> compare (length a) (length b)) (group (sort xs)))

-- Esta función toma una lista de números y devuelve el rango de todos los elementos de la lista
rango :: [Int] -> Int
rango [] = error "La lista está vacía"
rango xs = máximo xs - mínimo xs

-- Esta función toma una lista de números y devuelve el mínimo de todos los elementos de la lista
mínimo :: [Int] -> Int
mínimo [] = error "La lista está vacía"
mínimo [x] = x
mínimo (x:xs)
    | x < mínimo xs = x
    | otherwise = mínimo xs
```

Este código es un ejemplo complejo de código Haskell que contiene una variedad de funciones útiles para trabajar con listas de números. Las funciones incluyen:

* `máximo`: Devuelve el número máximo de una lista de números.
* `suma`: Devuelve la suma de todos los elementos de una lista de números.
* `producto`: Devuelve el producto de todos los elementos de una lista de números.
* `longitud`: Devuelve el número de elementos de una lista de números.
* `impares`: Devuelve el número de elementos impares de una lista de números.
* `pares`: Devuelve el número de elementos pares de una lista de números.
* `media`: Devuelve la media de todos los elementos de una lista de números.
* `varianza`: Devuelve la varianza de todos los elementos de una lista de números.
* `desviación_estándar`: Devuelve la desviación estándar de todos los elementos de una lista de números.
* `mediana`: Devuelve la mediana de todos los elementos de una lista de números.
* `moda`: Devuelve la moda de todos los elementos de una lista de números.
* `rango`: Devuelve el rango de todos los elementos de una lista de números.
* `mínimo`: Devuelve el número mínimo de una lista de números.

Este código es útil para realizar una variedad de tareas comunes de procesamiento de datos, como calcular la media, la varianza y la desviación estándar de un conjunto de datos. También se puede utilizar para encontrar el máximo, el mínimo, la mediana y la moda de un conjunto de datos.