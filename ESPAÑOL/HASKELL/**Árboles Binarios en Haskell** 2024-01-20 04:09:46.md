```haskell
-- Definición de módulos

module Main where

import Data.List

-- Tipos de datos

data Arbol a = Hoja a | Rama [Arbol a]

-- Funciones

-- Construye un árbol a partir de una lista de elementos
arbol :: [a] -> Arbol a
arbol [] = Hoja 0
arbol (x:xs) = Rama $ map arbol xs

-- Imprime un árbol
imprimirArbol :: Arbol a -> IO ()
imprimirArbol (Hoja x) = putStrLn $ show x
imprimirArbol (Rama xs) = do
  mapM_ imprimirArbol xs
  putStrLn ""

-- Busca un elemento en un árbol
buscar :: Eq a => a -> Arbol a -> Bool
buscar x (Hoja y) = x == y
buscar x (Rama xs) = any (buscar x) xs

-- Inserta un elemento en un árbol
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x (Hoja y) = Rama [Hoja x, Hoja y]
insertar x (Rama xs) = Rama $ insert x xs

-- Elimina un elemento de un árbol
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x (Hoja y) = Hoja y
eliminar x (Rama xs) = Rama $ delete x xs

-- Función principal

main :: IO ()
main = do
  -- Crear un árbol a partir de una lista de elementos
  let arbol = arbol [1, 2, 3, 4, 5]

  -- Imprimir el árbol
  imprimirArbol arbol

  -- Buscar un elemento en el árbol
  let encontrado = buscar 3 arbol
  putStrLn $ "Elemento encontrado: " ++ show encontrado

  -- Insertar un elemento en el árbol
  let arbolInsertado = insertar 6 arbol
  imprimirArbol arbolInsertado

  -- Eliminar un elemento del árbol
  let arbolEliminado = eliminar 2 arbol
  imprimirArbol arbolEliminado
```

Explicación del código:

* El código está dividido en varios módulos, cada uno de los cuales contiene un conjunto de funciones y tipos de datos relacionados.
* El módulo `Data.List` contiene una serie de funciones útiles para trabajar con listas, como `map`, `filter` y `foldr`.
* El tipo de datos `Arbol a` representa un árbol binario, donde `a` es el tipo de los elementos del árbol.
* La función `arbol` construye un árbol a partir de una lista de elementos, creando un nodo hoja para cada elemento de la lista.
* La función `imprimirArbol` imprime un árbol en la consola.
* La función `buscar` busca un elemento en un árbol, devolviendo `True` si el elemento se encuentra en el árbol y `False` en caso contrario.
* La función `insertar` inserta un elemento en un árbol, creando un nuevo nodo hoja para el elemento y añadiéndolo al árbol.
* La función `eliminar` elimina un elemento de un árbol, eliminando el nodo hoja que contiene el elemento.
* La función `main` es la función principal del programa, y se encarga de crear un árbol, imprimirlo, buscar un elemento en el árbol, insertar un elemento en el árbol y eliminar un elemento del árbol.