```haskell
-- Módulo principal del programa.
module Main where

-- Importar las bibliotecas necesarias.
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

-- Función principal del programa.
main :: IO ()
main = do
  -- Obtener los argumentos del programa.
  args <- getArgs

  -- Comprobar si se ha especificado un archivo de entrada.
  if null args then
    error "Se debe especificar un archivo de entrada."
  else
    -- Leer el archivo de entrada.
    lines <- readFile (head args)

  -- Eliminar los espacios en blanco de las líneas del archivo.
  let lines' = map (filter (/= ' ')) lines

  -- Obtener las palabras únicas de las líneas del archivo.
  let words = nub (concat lines')

  -- Ordenar las palabras alfabéticamente.
  let words' = sort words

  -- Mostrar las palabras únicas en la consola.
  putStrLn "Palabras únicas:"
  mapM_ putStrLn words'

  -- Obtener la palabra más larga.
  let longestWord = fromJust (maximumBy (compare `on` length) words')

  -- Mostrar la palabra más larga en la consola.
  putStrLn "Palabra más larga:"
  putStrLn longestWord

  -- Obtener la palabra más corta.
  let shortestWord = fromJust (minimumBy (compare `on` length) words')

  -- Mostrar la palabra más corta en la consola.
  putStrLn "Palabra más corta:"
  putStrLn shortestWord
```

Explicación del código:

* El programa comienza importando las bibliotecas necesarias.
* La función principal del programa `main` obtiene los argumentos del programa y comprueba si se ha especificado un archivo de entrada.
* Si se ha especificado un archivo de entrada, el programa lee el archivo y elimina los espacios en blanco de las líneas.
* A continuación, el programa obtiene las palabras únicas de las líneas del archivo y las ordena alfabéticamente.
* Finalmente, el programa muestra las palabras únicas, la palabra más larga y la palabra más corta en la consola.

Este código es complejo porque utiliza varias funciones de las bibliotecas importadas y realiza múltiples operaciones de procesamiento de texto. Además, el código está bien estructurado y comentado, lo que lo hace fácil de leer y entender.