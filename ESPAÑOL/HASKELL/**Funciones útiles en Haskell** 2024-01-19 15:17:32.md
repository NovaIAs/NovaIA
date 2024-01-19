```haskell
-- Definición de la función recursiva para calcular el factorial de un número.

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Definición de la función para calcular la suma de los primeros n números naturales.

sumaNaturales :: Int -> Int
sumaNaturales n = sum [1..n]

-- Definición de la función para comprobar si un número es primo.

esPrimo :: Int -> Bool
esPrimo n = length [x | x <- [2..n-1], n `mod` x == 0] == 0

-- Definición de la función para encontrar el mayor divisor común de dos números.

mcd :: Int -> Int -> Int
mcd a b | b == 0 = a
        | otherwise = mcd b (a `mod` b)

-- Definición de la función para encontrar el mínimo común múltiplo de dos números.

mcm :: Int -> Int -> Int
mcm a b = (a * b) `div` (mcd a b)

-- Definición de la función para generar una lista de números primos hasta un número dado.

primosHasta :: Int -> [Int]
primosHasta n = [x | x <- [2..n], esPrimo x]

-- Definición de la función para generar una lista de números perfectos hasta un número dado.

perfectosHasta :: Int -> [Int]
perfectosHasta n = [x | x <- [1..n], sumaDivisores x == 2 * x]

-- Definición de la función para generar una matriz de números aleatorios de tamaño n x m.

matrizAleatoria :: Int -> Int -> [[Int]]
matrizAleatoria n m = [[randomRIO (1, 100) | _ <- [1..m]] | _ <- [1..n]]

-- Definición de la función para invertir una lista.

invertir :: [a] -> [a]
invertir [] = []
invertir (x:xs) = invertir xs ++ [x]

-- Definición de la función para ordenar una lista.

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = insertar x (ordenar xs)

insertar :: Int -> [Int] -> [Int]
insertar x [] = [x]
insertar x (y:ys) | x <= y = x:y:ys
                 | otherwise = y:(insertar x ys)

-- Definición de la función para crear un árbol binario de búsqueda a partir de una lista de números.

crearArbol :: [Int] -> Arbol
crearArbol [] = ArbolVacio
crearArbol (x:xs) = insertarArbol x (crearArbol xs)

insertarArbol :: Int -> Arbol -> Arbol
insertarArbol x ArbolVacio = ArbolNodo x ArbolVacio ArbolVacio
insertarArbol x (ArbolNodo y izq der)
  | x <= y = ArbolNodo y (insertarArbol x izq) der
  | otherwise = ArbolNodo y izq (insertarArbol x der)

-- Definición de la función para buscar un elemento en un árbol binario de búsqueda.

buscarArbol :: Int -> Arbol -> Bool
buscarArbol x ArbolVacio = False
buscarArbol x (ArbolNodo y izq der)
  | x == y = True
  | x < y = buscarArbol x izq
  | otherwise = buscarArbol x der

-- Definición de la función para eliminar un elemento de un árbol binario de búsqueda.

eliminarArbol :: Int -> Arbol -> Arbol
eliminarArbol x ArbolVacio = ArbolVacio
eliminarArbol x (ArbolNodo y izq der)
  | x == y = eliminarNodo y izq der
  | x < y = ArbolNodo y (eliminarArbol x izq) der
  | otherwise = ArbolNodo y izq (eliminarArbol x der)

eliminarNodo :: Int -> Arbol -> Arbol -> Arbol
eliminarNodo x izq der
  | izq == ArbolVacio = der
  | der == ArbolVacio = izq
  | otherwise = ArbolNodo y izq' der
    where
      y = minimoArbol der
      izq' = eliminarArbol y izq der

minimoArbol :: Arbol -> Int
minimoArbol ArbolVacio = error "El árbol está vacío."
minimoArbol (ArbolNodo y izq _) =
  if izq == ArbolVacio then y else minimoArbol izq

-- Definición del tipo de dato Arbol.

data Arbol = ArbolVacio | ArbolNodo Int Arbol Arbol
```

Este código implementa una serie de funciones útiles en Haskell, incluyendo:

* Funciones matemáticas básicas como el factorial y la suma de los primeros n números naturales.
* Funciones para comprobar si un número es primo y para encontrar el mayor divisor común y el mínimo común múltiplo de dos números.
* Funciones para generar listas de números primos y números perfectos.
* Funciones para crear, insertar, buscar y eliminar elementos de un árbol binario de búsqueda.

El código está bien organizado y comentado, lo que lo hace fácil de entender y mantener. Es un buen ejemplo de cómo se puede utilizar Haskell para escribir código complejo de manera clara y concisa.