```haskell
{-
Este código Haskell implementa el método de Gauss-Jordan para resolver sistemas de ecuaciones lineales.
-}

{-
Esta función toma una matriz de números reales y devuelve una matriz en forma de escalón reducido.
-}
escalonReducido :: [[Double]] -> [[Double]]
escalonReducido matriz = foldl escalonReducidoFila matriz [1..filas matriz]
  where
    filas matriz = length matriz

{-
Esta función toma una matriz y un índice de fila y devuelve una matriz en la que la fila especificada está en forma de escalón reducido.
-}
escalonReducidoFila :: [[Double]] -> Int -> [[Double]]
escalonReducidoFila matriz fila =
  let
    filaPivote = matriz !! (fila - 1)
    pivote = filaPivote !! (fila - 1)
    filasRestantes = take (fila - 2) matriz ++ drop fila matriz
  in
    map (escalonReducidoFila' filaPivote pivote) filasRestantes

{-
Esta función toma una fila de una matriz, el pivote de la fila y un índice de fila y devuelve una fila en la que el elemento en el índice de fila es cero.
-}
escalonReducidoFila' :: [Double] -> Double -> Int -> [Double]
escalonReducidoFila' filaPivote pivote fila =
  let
    factor = filaPivote !! (fila - 1) / pivote
  in
    zipWith (-) (filaPivote) (map (* factor) fila)

{-
Esta función toma una matriz en forma de escalón reducido y devuelve una lista de soluciones al sistema de ecuaciones lineales representado por la matriz.
-}
soluciones :: [[Double]] -> [Double]
soluciones matriz =
  let
    columnas = length (head matriz)
    filas = length matriz
  in
    map (solucionFila matriz) [columnas..1]

{-
Esta función toma una matriz en forma de escalón reducido y un índice de columna y devuelve la solución para la variable correspondiente a la columna especificada.
-}
solucionFila :: [[Double]] -> Int -> Double
solucionFila matriz columna =
  let
    fila = matriz !! (filas matriz - columna)
    pivote = fila !! (columna - 1)
    coeficientes = take (columna - 1) fila ++ drop columna fila
    solucionesRestantes = map (solucionFila matriz) [columna - 1..1]
  in
    (sum (zipWith (*) coeficientes solucionesRestantes) - fila !! (columnas matriz - 1)) / pivote

{-
Ejemplo de uso:
-}
main :: IO ()
main = do
  let matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  print (escalonReducido matriz)
  print (soluciones (escalonReducido matriz))
```

Explicación del código:

* La función `escalonReducido` toma una matriz de números reales y devuelve una matriz en forma de escalón reducido. Esto se hace aplicando repetidamente la función `escalonReducidoFila` a cada fila de la matriz.
* La función `escalonReducidoFila` toma una matriz y un índice de fila y devuelve una matriz en la que la fila especificada está en forma de escalón reducido. Esto se hace dividiendo cada elemento de la fila por el pivote de la fila, que es el primer elemento no cero de la fila. A continuación, se resta el producto del pivote y cada elemento de la fila de los elementos correspondientes de las demás filas.
* La función `soluciones` toma una matriz en forma de escalón reducido y devuelve una lista de soluciones al sistema de ecuaciones lineales representado por la matriz. Esto se hace resolviendo cada variable de la última fila de la matriz en términos de las otras variables. A continuación, se sustituyen las variables resueltas en las filas anteriores de la matriz y se repite el proceso hasta que se hayan resuelto todas las variables.
* La función `solucionFila` toma una matriz en forma de escalón reducido y un índice de columna y devuelve la solución para la variable correspondiente a la columna especificada. Esto se hace sumando los productos de los coeficientes de la variable y las soluciones de las variables anteriores, y restando el término constante de la fila. A continuación, se divide la suma por el pivote de la fila.

Este código es un ejemplo de cómo utilizar el método de Gauss-Jordan para resolver sistemas de ecuaciones lineales. El método de Gauss-Jordan es un método sistemático para resolver sistemas de ecuaciones lineales que se utiliza a menudo en matemáticas y ciencias.