```haskell
import Data.List (nub, sort)

-- Función principal
main :: IO ()
main = do
  putStrLn "Introduce una lista de números separados por comas:"
  input <- getLine
  let numbers = map read (words input) :: [Int]
  let uniqueNumbers = nub numbers
  let sortedNumbers = sort uniqueNumbers
  putStrLn "Números únicos y ordenados:"
  mapM_ print sortedNumbers

-- Función auxiliar para leer una línea de texto y convertirla en una lista de números
words :: String -> [String]
words = fmap (:[]) . groupBy ((==) `on` head)

-- Función auxiliar para agrupar elementos consecutivos iguales en una lista
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f xs = go [] xs
  where
    go acc [] = reverse acc
    go acc (x:xs) | f x (head acc) = go (x:head acc) xs
                   | otherwise = go (x:[]) xs
```

Explicación:

* La función `main` es la función principal del programa. Primero, lee una línea de texto del usuario. A continuación, divide la línea de texto en palabras, utilizando la función `words`. Las palabras se convierten en números enteros, utilizando la función `read`.
* Los números se pasan a la función `nub`, que elimina los elementos duplicados de una lista. El resultado es una lista de números únicos.
* La lista de números únicos se pasa a la función `sort`, que ordena los números en orden ascendente.
* Finalmente, se imprime la lista de números únicos y ordenados, utilizando la función `mapM_` y la función `print`.

La función `words` es una función auxiliar que divide una línea de texto en palabras. Utiliza la función `groupBy` para agrupar los caracteres consecutivos iguales en una lista. La función `groupBy` utiliza una función de comparación `f` para determinar si dos caracteres son iguales. En este caso, la función de comparación es `(==) `on` head`, que compara los primeros caracteres de dos listas.

La función `nub` es una función auxiliar que elimina los elementos duplicados de una lista. Utiliza un algoritmo de ordenación interna para encontrar los elementos duplicados.

La función `sort` es una función auxiliar que ordena una lista en orden ascendente. Utiliza un algoritmo de ordenación rápida para ordenar la lista.

La función `mapM_` es una función auxiliar que aplica una función a cada elemento de una lista. En este caso, la función que se aplica es la función `print`.