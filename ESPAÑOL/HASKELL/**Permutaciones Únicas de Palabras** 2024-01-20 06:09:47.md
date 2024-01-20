```haskell
import Data.List (permutations, sort)
import Control.Monad (guard)
import System.IO

-- Función principal
main :: IO ()
main = do
  -- Leer el número de palabras
  putStrLn "Introduce el número de palabras:"
  numPalabras <- readLn
  
  -- Leer las palabras
  putStrLn "Introduce las palabras (separadas por espacios):"
  palabras <- getLine

  -- Separar las palabras
  palabrasSeparadas <- words palabras

  -- Comprobar si el número de palabras es válido
  guard (numPalabras == length palabrasSeparadas) || error "El número de palabras introducido no coincide con el número de palabras proporcionadas."

  -- Obtener todas las permutaciones de las palabras
  permutacionesPalabras <- permutations palabrasSeparadas

  -- Ordenar las permutaciones lexicográficamente
  permutacionesOrdenadas <- map sort permutacionesPalabras

  -- Eliminar las permutaciones duplicadas
  permutacionesUnicas <- nub permutacionesOrdenadas

  -- Imprimir las permutaciones únicas
  putStrLn "Permutaciones únicas:"
  mapM_ putStrLn permutacionesUnicas
```

Explicación del código:

* La función principal `main` se encarga de leer el número de palabras y las palabras del usuario, y de llamar a las funciones necesarias para obtener las permutaciones únicas.
* La función `permutations` genera todas las permutaciones de una lista.
* La función `sort` ordena una lista lexicográficamente.
* La función `nub` elimina los elementos duplicados de una lista.
* La función `mapM_` aplica una función a cada elemento de una lista y devuelve una lista con los resultados.
* La función `putStrLn` imprime una cadena de texto seguida de un salto de línea.