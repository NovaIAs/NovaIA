```haskell
-- Módulo principal
module Main where

-- Importaciones
import           Data.List          (intercalate)
import           System.Environment (getArgs)

-- Tipo de dato para representar un árbol binario
data Arbol a = Hoja a | Rama (Arbol a) a (Arbol a) deriving (Show)

-- Función para crear un árbol binario a partir de una lista
crearArbol :: [a] -> Arbol a
crearArbol [] = Hoja ' '
crearArbol (x:xs) = Rama (crearArbol xs) x (crearArbol [])

-- Función para imprimir un árbol binario
imprimirArbol :: Arbol a -> String
imprimirArbol (Hoja a) = [a]
imprimirArbol (Rama izq a der) = intercalate " " [imprimirArbol izq, [a], imprimirArbol der]

-- Función principal
main :: IO ()
main = do
  -- Obtener los argumentos de la línea de comandos
  args <- getArgs

  -- Si no hay argumentos, mostrar un mensaje de error
  if null args then putStrLn "Error: se requiere un argumento"
    else do
      -- Crear el árbol binario a partir del primer argumento
      arbol <- crearArbol (head args)

      -- Imprimir el árbol binario
      putStrLn (imprimirArbol arbol)
```

Explicación:

* El módulo principal `Main` importa los módulos `Data.List` y `System.Environment`.
* El tipo de dato `Arbol` representa un árbol binario. Puede ser una hoja (`Hoja a`) o una rama (`Rama izq a der`), donde `izq` y `der` son subárboles y `a` es el valor del nodo actual.
* La función `crearArbol` crea un árbol binario a partir de una lista. Si la lista está vacía, devuelve una hoja. De lo contrario, devuelve una rama con el primer elemento de la lista como valor del nodo actual, el subárbol izquierdo creado a partir del resto de la lista y el subárbol derecho creado a partir de una lista vacía.
* La función `imprimirArbol` imprime un árbol binario como una cadena de caracteres. Si el árbol es una hoja, devuelve el valor de la hoja. Si el árbol es una rama, devuelve la cadena de caracteres obtenida al intercalar las cadenas de caracteres obtenidas al imprimir los subárboles izquierdo y derecho, con el valor del nodo actual intercalado entre ellas.
* La función `main` obtiene los argumentos de la línea de comandos. Si no hay argumentos, muestra un mensaje de error. De lo contrario, crea un árbol binario a partir del primer argumento y lo imprime.