```haskell
-- Módulo principal
module Main where

-- Importar las bibliotecas necesarias
import Data.List (sortBy, groupBy)
import Data.Char (toLower, isDigit)
import Control.Monad (guard)

-- Función principal
main :: IO ()
main = do
  -- Leer el texto del usuario
  texto <- getLine

  -- Convertir el texto a minúsculas y eliminar los dígitos
  textoLimpio <- map toLower texto >>= filter (not . isDigit)

  -- Dividir el texto en palabras
  palabras <- words textoLimpio

  -- Agrupar las palabras por su primera letra
  grupos <- groupBy (\x y -> head x == head y) palabras

  -- Ordenar los grupos alfabéticamente
  gruposOrdenados <- sortBy (compare `on` head) grupos

  -- Imprimir los grupos de palabras
  mapM_ printGrupos gruposOrdenados

-- Función para imprimir los grupos de palabras
printGrupos :: [String] -> IO ()
printGrupos grupos = do
  -- Imprimir la primera letra del grupo
  putStrLn $ "¡" ++ head grupos ++ "!"

  -- Imprimir las palabras del grupo
  mapM_ putStrLn grupos

```

**Explicación del código:**

* La función `main` es la función principal del programa.
* La función `getLine` lee una línea de texto del usuario.
* La función `map toLower texto` convierte el texto a minúsculas.
* La función `filter (not . isDigit)` elimina los dígitos del texto.
* La función `words` divide el texto en palabras.
* La función `groupBy (\x y -> head x == head y)` agrupa las palabras por su primera letra.
* La función `sortBy (compare `on` head)` ordena los grupos alfabéticamente.
* La función `printGrupos` imprime los grupos de palabras.

El ejemplo funciona de la siguiente manera:

* El usuario introduce un texto.
* El programa convierte el texto a minúsculas y elimina los dígitos.
* El programa divide el texto en palabras.
* El programa agrupa las palabras por su primera letra.
* El programa ordena los grupos alfabéticamente.
* El programa imprime los grupos de palabras.