```haskell
-- Definición de dato recursivo para representar árboles binarios
data Arbol a = Hoja a
            | Nodo (Arbol a) a (Arbol a)

-- Función para crear un árbol binario a partir de una lista de elementos
crearArbol :: [a] -> Arbol a
crearArbol [] = Hoja undefined
crearArbol (x:xs) = Nodo (crearArbol xs) x (crearArbol [])

-- Función para buscar un elemento en un árbol binario
buscar :: Eq a => a -> Arbol a -> Bool
buscar x (Hoja y) = x == y
buscar x (Nodo izq y der) = x == y || buscar x izq || buscar x der

-- Función para insertar un elemento en un árbol binario
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x (Hoja y) = Nodo (Hoja y) x (Hoja undefined)
insertar x (Nodo izq y der) | x < y = Nodo (insertar x izq) y der
                           | x > y = Nodo izq y (insertar x der)
                           | otherwise = Nodo izq y der

-- Función para eliminar un elemento de un árbol binario
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x (Hoja y) = Hoja undefined
eliminar x (Nodo izq y der) | x < y = Nodo (eliminar x izq) y der
                           | x > y = Nodo izq y (eliminar x der)
                           | otherwise = unir izq der

-- Función auxiliar para unir dos árboles binarios
unir :: Arbol a -> Arbol a -> Arbol a
unir (Hoja x) (Hoja y) = Hoja (x `min` y)
unir (Nodo izq x der) (Hoja y) = Nodo izq x (unir der (Hoja y))
unir (Hoja x) (Nodo izq y der) = Nodo (unir (Hoja x) izq) y der
unir (Nodo izq1 x der1) (Nodo izq2 y der2) = Nodo (unir izq1 izq2) x (unir der1 der2)

-- Función para imprimir un árbol binario en consola
imprimirArbol :: Show a => Arbol a -> IO ()
imprimirArbol (Hoja x) = putStrLn (show x)
imprimirArbol (Nodo izq x der) = do
  imprimirArbol izq
  putStrLn (show x)
  imprimirArbol der

-- Función para probar el código
main :: IO ()
let arbol = crearArbol [10, 5, 15, 2, 7, 12, 20]
imprimirArbol arbol
putStrLn (show (buscar 15 arbol))
putStrLn (show (buscar 25 arbol))
let arbol2 = insertar 8 arbol
imprimirArbol arbol2
let arbol3 = eliminar 10 arbol2
imprimirArbol arbol3
```

Este código en Haskell es una implementación de un árbol binario. Un árbol binario es una estructura de datos que consiste en un conjunto de nodos, cada uno de los cuales tiene un valor y un máximo de dos hijos.

El código comienza definiendo el tipo de datos Arbol, que es una definición de datos recursiva. Esto significa que un árbol puede ser una hoja (un nodo sin hijos) o un nodo (un nodo con dos hijos).

A continuación, el código define una función para crear un árbol binario a partir de una lista de elementos. Esta función crea un nodo para cada elemento de la lista y luego los une para formar un árbol binario.

El código también define funciones para buscar un elemento en un árbol binario, insertar un elemento en un árbol binario y eliminar un elemento de un árbol binario.

Por último, el código define una función para imprimir un árbol binario en consola. Esta función recorre el árbol en orden preorden (visitando primero la raíz, luego los hijos izquierdos y luego los hijos derechos) e imprime el valor de cada nodo.

Para probar el código, el código crea un árbol binario a partir de una lista de números y luego lo imprime en consola. A continuación, el código busca un elemento en el árbol y lo imprime. Por último, el código inserta un elemento en el árbol y lo elimina.

Este código es un ejemplo de cómo se pueden utilizar los árboles binarios para almacenar y manipular datos. Los árboles binarios se utilizan en una amplia variedad de aplicaciones, incluyendo la búsqueda, el ordenamiento y la compresión de datos.