```haskell
-- Definir una función que reciba un número y devuelva su factorial

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Definir una función que reciba una lista de números y devuelva su producto

producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Definir una función que reciba una lista de listas de números y devuelva una lista con los productos de cada lista

productos :: [[Int]] -> [Int]
productos [] = []
productos (x:xs) = producto x : productos xs

-- Definir una función que reciba una lista de números y devuelva una lista con las sumas de cada sublista de longitud 3

sumasDeTres :: [Int] -> [Int]
sumasDeTres [] = []
sumasDeTres xs = sum (take 3 xs) : sumasDeTres (drop 3 xs)

-- Definir una función que reciba una lista de listas de números y devuelva una lista con las sumas de cada lista

sumas :: [[Int]] -> [Int]
sumas [] = []
sumas (x:xs) = sum x : sumas xs

-- Definir una función que reciba una lista de números y devuelva una lista con los promedios de cada sublista de longitud 3

promediosDeTres :: [Int] -> [Int]
promediosDeTres [] = []
promediosDeTres xs = (sum (take 3 xs)) `div` 3 : promediosDeTres (drop 3 xs)

-- Definir una función que reciba una lista de listas de números y devuelva una lista con los promedios de cada lista

promedios :: [[Int]] -> [Int]
promedios [] = []
promedios (x:xs) = (sum x) `div` length x : promedios xs

-- Definir una función que reciba una lista de números y devuelva una lista con los máximos de cada sublista de longitud 3

maximosDeTres :: [Int] -> [Int]
maximosDeTres [] = []
maximosDeTres xs = maximum (take 3 xs) : maximosDeTres (drop 3 xs)

-- Definir una función que reciba una lista de listas de números y devuelva una lista con los máximos de cada lista

maximos :: [[Int]] -> [Int]
maximos [] = []
maximos (x:xs) = maximum x : maximos xs

-- Definir una función que reciba una lista de números y devuelva una lista con los mínimos de cada sublista de longitud 3

minimosDeTres :: [Int] -> [Int]
minimosDeTres [] = []
minimosDeTres xs = minimum (take 3 xs) : minimosDeTres (drop 3 xs)

-- Definir una función que reciba una lista de listas de números y devuelva una lista con los mínimos de cada lista

minimos :: [[Int]] -> [Int]
minimos [] = []
minimos (x:xs) = minimum x : minimos xs

-- Definir una función que reciba una lista de números y devuelva una lista con las medianas de cada sublista de longitud 3

medianasDeTres :: [Int] -> [Int]
medianasDeTres [] = []
medianasDeTres xs = sort (take 3 xs) !! 1 : medianasDeTres (drop 3 xs)

-- Definir una función que reciba una lista de listas de números y devuelva una lista con las medianas de cada lista

medianas :: [[Int]] -> [Int]
medianas [] = []
medianas (x:xs) = sort x !! (length x `div` 2) : medianas xs
```

Explicación del código:

- La función `factorial` calcula el factorial de un número utilizando recursión.
- La función `producto` calcula el producto de una lista de números utilizando recursión.
- La función `productos` calcula una lista con los productos de cada lista de una lista de listas de números utilizando `producto` y recursión.
- La función `sumasDeTres` calcula una lista con las sumas de cada sublista de longitud 3 de una lista de números utilizando recursión.
- La función `sumas` calcula una lista con las sumas de cada lista de una lista de listas de números utilizando `sumasDeTres` y recursión.
- La función `promediosDeTres` calcula una lista con los promedios de cada sublista de longitud 3 de una lista de números utilizando recursión.
- La función `promedios` calcula una lista con los promedios de cada lista de una lista de listas de números utilizando `promediosDeTres` y recursión.
- La función `maximosDeTres` calcula una lista con los máximos de cada sublista de longitud 3 de una lista de números utilizando recursión.
- La función `maximos` calcula una lista con los máximos de cada lista de una lista de listas de números utilizando `maximosDeTres` y recursión.
- La función `minimosDeTres` calcula una lista con los mínimos de cada sublista de longitud 3 de una lista de números utilizando recursión.
- La función `minimos` calcula una lista con los mínimos de cada lista de una lista de listas de números utilizando `minimosDeTres` y recursión.
- La función `medianasDeTres` calcula una lista con las medianas de cada sublista de longitud 3 de una lista de números utilizando recursión.
- La función `medianas` calcula una lista con las medianas de cada lista de una lista de listas de números utilizando `medianasDeTres` y recursión.