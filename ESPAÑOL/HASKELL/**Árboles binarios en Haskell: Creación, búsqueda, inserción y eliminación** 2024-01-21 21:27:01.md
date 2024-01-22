```haskell
-- Módulo principal del programa
module Main where

-- Importamos las bibliotecas necesarias
import Data.List (nub, sort)
import Data.Char (toUpper)

-- Definimos el tipo de datos para representar un árbol binario
data Arbol a = Hoja a | Rama (Arbol a) a (Arbol a)
  deriving (Eq, Show)

-- Función para crear un árbol binario
crearArbol :: [a] -> Arbol a
crearArbol [] = Hoja ' '
crearArbol (x:xs) = Rama (crearArbol xs) x (crearArbol (tail xs))

-- Función para buscar un elemento en un árbol binario
buscar :: Eq a => a -> Arbol a -> Bool
buscar x Hoja _ = False
buscar x (Rama izq _ der) = x == raiz || buscar x izq || buscar x der
  where raiz = raizArbol (Rama izq _ der)

-- Función para insertar un elemento en un árbol binario
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Hoja _ = Rama Hoja x Hoja
insertar x (Rama izq raiz der)
  | x <= raiz = Rama (insertar x izq) raiz der
  | otherwise = Rama izq raiz (insertar x der)

-- Función para eliminar un elemento de un árbol binario
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x Hoja _ = Hoja ' '
eliminar x (Rama izq raiz der)
  | x == raiz = unir izq der
  | x < raiz = Rama (eliminar x izq) raiz der
  | otherwise = Rama izq raiz (eliminar x der)
  where unir izq der =
          case izq of
            Hoja _ -> der
            Rama izq' raiz' der' -> Rama izq' raiz' (unir der' der)

-- Función para imprimir un árbol binario en forma de árbol ASCII
imprimirArbol :: Arbol a -> String
imprimirArbol Hoja _ = ""
imprimirArbol (Rama izq raiz der) =
  imprimirArbol izq ++ "     " ++ imprimirArbol der ++ "\n" ++
  replicate (length (imprimirArbol izq) + 4) ' ' ++ toUpper (show raiz)

-- Función principal del programa
main :: IO ()
main = do
  -- Creamos un árbol binario a partir de una lista de elementos
  let arbol = crearArbol [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

  -- Imprimimos el árbol en forma de árbol ASCII
  putStrLn (imprimirArbol arbol)

  -- Buscamos un elemento en el árbol
  let elementoBuscado = 5
  if buscar elementoBuscado arbol
    then putStrLn (show elementoBuscado ++ " está en el árbol")
    else putStrLn (show elementoBuscado ++ " no está en el árbol")

  -- Insertamos un elemento en el árbol
  let elementoInsertado = 11
  let arbolInsertado = insertar elementoInsertado arbol
  putStrLn ("Se ha insertado " ++ show elementoInsertado ++ " en el árbol")
  putStrLn (imprimirArbol arbolInsertado)

  -- Eliminamos un elemento del árbol
  let elementoEliminado = 6
  let arbolEliminado = eliminar elementoEliminado arbol
  putStrLn ("Se ha eliminado " ++ show elementoEliminado ++ " del árbol")
  putStrLn (imprimirArbol arbolEliminado)
```

Explicación del código:

* El código define un tipo de datos para representar un árbol binario, con dos constructores: `Hoja` para árboles vacíos y `Rama` para árboles con un valor y dos subárboles.
* Se definen varias funciones para crear, buscar, insertar y eliminar elementos de un árbol binario.
* La función `imprimirArbol` imprime un árbol binario en forma de árbol ASCII.
* La función `main` crea un árbol binario a partir de una lista de elementos, lo imprime, busca un elemento, lo inserta y lo elimina.

El código es complejo porque:

* Utiliza recursión para crear, buscar, insertar y eliminar elementos de un árbol binario.
* La función `imprimirArbol` utiliza recursión y una cadena de caracteres para imprimir el árbol en forma de árbol ASCII.
* La función `main` utiliza varias funciones para crear, buscar, insertar y eliminar elementos de un árbol binario.