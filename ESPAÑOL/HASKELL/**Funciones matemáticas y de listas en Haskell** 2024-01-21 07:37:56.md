```haskell
-- Definimos el tipo de dato Lista como una lista enlazada de elementos de tipo a.
data Lista a = Vacía | Nodo a (Lista a)

-- Definimos la función insertar, que inserta un elemento al principio de una lista.
insertar :: a -> Lista a -> Lista a
insertar x xs = Nodo x xs

-- Definimos la función eliminar, que elimina el primer elemento de una lista.
eliminar :: Lista a -> Lista a
eliminar (Nodo _ xs) = xs
eliminar Vacía = Vacía

-- Definimos la función buscar, que busca un elemento en una lista y devuelve su índice si lo encuentra, o -1 si no lo encuentra.
buscar :: Eq a => a -> Lista a -> Int
buscar x (Nodo y ys)
  | x == y = 0
  | otherwise = 1 + buscar x ys
buscar _ Vacía = -1

-- Definimos la función ordenar, que ordena una lista de enteros en orden ascendente.
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = insertar x (ordenar xs)

-- Definimos la función sumar, que suma todos los elementos de una lista de enteros.
sumar :: [Int] -> Int
sumar [] = 0
sumar (x:xs) = x + sumar xs

-- Definimos la función producto, que multiplica todos los elementos de una lista de enteros.
producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Definimos la función factorial, que calcula el factorial de un número entero.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Definimos la función fibonacci, que calcula el n-ésimo número de Fibonacci.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Definimos la función mcd, que calcula el máximo común divisor de dos números enteros.
mcd :: Int -> Int -> Int
mcd x y
  | y == 0 = abs x
  | otherwise = mcd y (x `rem` y)

-- Definimos la función mcm, que calcula el mínimo común múltiplo de dos números enteros.
mcm :: Int -> Int -> Int
mcm x y = abs (x * y) `div` mcd x y

-- Definimos la función primo, que comprueba si un número entero es primo.
primo :: Int -> Bool
primo n
  | n <= 1 = False
  | n == 2 = True
  | n `rem` 2 == 0 = False
  | otherwise = not $ any (\x -> n `rem` x == 0) [3,5..n-1]

-- Definimos la función siguientePrimo, que devuelve el siguiente número primo después de un número entero dado.
siguientePrimo :: Int -> Int
siguientePrimo n
  | primo n = n
  | otherwise = siguientePrimo (n+1)

-- Definimos la función imprimirLista, que imprime una lista de elementos de tipo a por pantalla.
imprimirLista :: [a] -> IO ()
imprimirLista [] = putStrLn ""
imprimirLista (x:xs) = putStr (show x ++ " ") >> imprimirLista xs

-- Definimos la función main, que es la función principal del programa.
main :: IO ()
main = do
  -- Creamos una lista de enteros.
  let lista = [1,2,3,4,5]

  -- Insertamos un elemento al principio de la lista.
  let nuevaLista = insertar 0 lista

  -- Eliminamos el primer elemento de la lista.
  let listaSinPrimero = eliminar nuevaLista

  -- Buscamos un elemento en la lista.
  let indice = buscar 3 lista

  -- Ordenamos la lista.
  let listaOrdenada = ordenar lista

  -- Sumamos todos los elementos de la lista.
  let suma = sumar lista

  -- Multiplicamos todos los elementos de la lista.
  let producto = producto lista

  -- Calculamos el factorial de un número.
  let factorial5 = factorial 5

  -- Calculamos el n-ésimo número de Fibonacci.
  let fibonacci10 = fibonacci 10

  -- Calculamos el máximo común divisor de dos números.
  let mcd12y18 = mcd 12 18

  -- Calculamos el mínimo común múltiplo de dos números.
  let mcm12y18 = mcm 12 18

  -- Comprobamos si un número es primo.
  let esPrimo13 = primo 13

  -- Obtenemos el siguiente número primo después de un número.
  let siguientePrimo13 = siguientePrimo 13

  -- Imprimimos la lista por pantalla.
  putStrLn "Lista original:"
  imprimirLista lista

  -- Imprimimos la lista con el nuevo elemento insertado al principio.
  putStrLn "Lista con el nuevo elemento insertado al principio:"
  imprimirLista nuevaLista

  -- Imprimimos la lista sin el primer elemento.
  putStrLn "Lista sin el primer elemento:"
  imprimirLista listaSinPrimero

  -- Imprimimos el índice del elemento buscado en la lista.
  putStrLn "Índice del elemento buscado en la lista:"
  print indice

  -- Imprimimos la lista ordenada.
  putStrLn "Lista ordenada:"
  imprimirLista listaOrdenada

  -- Imprimimos la suma de todos los elementos de la lista.
  putStrLn "Suma de todos los elementos de la lista:"
  print suma

  -- Imprimimos el producto de todos los elementos de la lista.
  putStrLn "Producto de todos los elementos de la lista:"
  print producto

  -- Imprimimos el factorial de un número.
  putStrLn "Factorial de 5:"
  print factorial5

  -- Imprimimos el n-ésimo número de Fibonacci.
  putStrLn "10-ésimo número de Fibonacci:"
  print fibonacci10

  -- Imprimimos el máximo común divisor de dos números.
  putStrLn "Máximo común divisor de 12 y 18:"
  print mcd12y18

  -- Imprimimos el mínimo común múltiplo de dos números.
  putStrLn "Mínimo común múltiplo de 12 y 18:"
  print mcm12y18

  -- Imprimimos si un número es primo.
  putStrLn "13 es primo:"
  print esPrimo13

  -- Imprimimos el siguiente número primo después de un número.
  putStrLn "Siguiente número primo después de 13:"
  print siguientePrimo13
```

Este código es una implementación en Haskell de varias funciones matemáticas y de listas. El código incluye funciones para insertar y eliminar elementos de una lista, buscar un elemento en una lista, ordenar una lista, sumar los elementos de una lista, multiplicar los elementos de una lista, calcular el factorial de un número, calcular el n-ésimo número de Fibonacci, calcular el máximo común divisor de dos números, calcular el mínimo común múltiplo de dos números, comprobar si un número es primo y obtener el siguiente número primo después de un número. El código también incluye una función para imprimir una lista por pantalla.

El código está bien documentado con comentarios en español, lo que lo hace fácil de entender y usar. El código también está bien estructurado, con funciones bien definidas y organizadas.