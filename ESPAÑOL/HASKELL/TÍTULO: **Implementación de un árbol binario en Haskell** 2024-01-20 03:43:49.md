```haskell
import Data.List
import Data.Maybe
import Data.Ord
import Data.Char
import Data.Function

-- Definición de tipos de datos
data ArbolBinario a = Hoja a | Nodo (ArbolBinario a) (ArbolBinario a)

-- Funciones de árbol binario
altura :: ArbolBinario a -> Int
altura Hoja{} = 0
altura (Nodo l r) = 1 + max (altura l) (altura r)

recorridoEnOrden :: ArbolBinario a -> [a]
recorridoEnOrden Hoja{} = []
recorridoEnOrden (Nodo l r) = recorridoEnOrden l ++ [getRoot r] ++ recorridoEnOrden r

recorridoPreOrden :: ArbolBinario a -> [a]
recorridoPreOrden Hoja{} = []
recorridoPreOrden (Nodo l r) = [getRoot l] ++ recorridoPreOrden l ++ recorridoPreOrden r

recorridoPostOrden :: ArbolBinario a -> [a]
recorridoPostOrden Hoja{} = []
recorridoPostOrden (Nodo l r) = recorridoPostOrden l ++ recorridoPostOrden r ++ [getRoot r]

getRoot :: ArbolBinario a -> a
getRoot (Nodo l r) = l

-- Funciones de búsqueda en árbol binario
buscar :: Ord a => a -> ArbolBinario a -> Maybe a
buscar x Hoja{} = Nothing
buscar x (Nodo l r)
  | x == l = Just l
  | x < l = buscar x l
  | otherwise = buscar x r

-- Funciones de inserción en árbol binario
insertar :: Ord a => a -> ArbolBinario a -> ArbolBinario a
insertar x Hoja{} = Nodo (Hoja x) Hoja{}
insertar x (Nodo l r)
  | x < l = Nodo (insertar x l) r
  | otherwise = Nodo l (insertar x r)

-- Funciones de eliminación en árbol binario
eliminar :: Ord a => a -> ArbolBinario a -> ArbolBinario a
eliminar x Hoja{} = Hoja{}
eliminar x (Nodo l r)
  | x == l = r
  | x < l = Nodo (eliminar x l) r
  | otherwise = Nodo l (eliminar x r)

-- Función auxiliar para eliminar el nodo raíz de un árbol binario
eliminarRaiz :: ArbolBinario a -> ArbolBinario a
eliminarRaiz Hoja{} = Hoja{}
eliminarRaiz (Nodo l r)
  | altura l > altura r = Nodo l (eliminarRaiz r)
  | otherwise = Nodo (eliminarRaiz l) r

-- Función principal
main = do
  let arbol = Nodo (Nodo (Hoja 1) (Hoja 2)) (Nodo (Hoja 3) (Hoja 4))
  print $ altura arbol
  print $ recorridoEnOrden arbol
  print $ recorridoPreOrden arbol
  print $ recorridoPostOrden arbol
  print $ buscar 2 arbol
  print $ insertar 5 arbol
  print $ eliminar 2 arbol
  print $ eliminarRaiz arbol
```

Explicación del código:

* Se definen los tipos de datos necesarios para representar un árbol binario y sus nodos.
* Se definen las funciones necesarias para realizar recorridos del árbol binario en orden, preorden y postorden.
* Se define la función `buscar` para buscar un elemento en un árbol binario.
* Se define la función `insertar` para insertar un elemento en un árbol binario.
* Se define la función `eliminar` para eliminar un elemento de un árbol binario.
* Se define la función auxiliar `eliminarRaiz` para eliminar el nodo raíz de un árbol binario.
* En la función principal, se crea un árbol binario de ejemplo, se calculan su altura y sus recorridos, se busca un elemento, se inserta un elemento, se elimina un elemento y se elimina el nodo raíz.