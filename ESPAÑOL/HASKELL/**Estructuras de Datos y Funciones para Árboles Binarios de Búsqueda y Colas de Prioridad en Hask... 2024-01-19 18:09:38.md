```haskell
-- Definimos un tipo de dato para representar un árbol binario de búsqueda.
data Arbol a = Vacío | Nodo a (Arbol a) (Arbol a)

-- Función para insertar un elemento en un árbol binario de búsqueda.
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Vacío = Nodo x Vacío Vacío
insertar x (Nodo y izq der)
  | x < y = Nodo y (insertar x izq) der
  | x > y = Nodo y izq (insertar x der)
  | otherwise = Nodo x izq der

-- Función para buscar un elemento en un árbol binario de búsqueda.
buscar :: Ord a => a -> Arbol a -> Bool
buscar x Vacío = False
buscar x (Nodo y izq der)
  | x == y = True
  | x < y = buscar x izq
  | x > y = buscar x der

-- Función para eliminar un elemento de un árbol binario de búsqueda.
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x Vacío = Vacío
eliminar x (Nodo y izq der)
  | x == y = eliminarRaiz (Nodo y izq der)
  | x < y = Nodo y (eliminar x izq) der
  | x > y = Nodo y izq (eliminar x der)

-- Función auxiliar para eliminar la raíz de un árbol binario de búsqueda.
eliminarRaiz :: Arbol a -> Arbol a
eliminarRaiz Vacío = Vacío
eliminarRaiz (Nodo _ Vacío der) = der
eliminarRaiz (Nodo _ izq Vacío) = izq
eliminarRaiz (Nodo x izq der) =
  let (y, nuevoIzq) = eliminarMax izq
  in Nodo y nuevoIzq der

-- Función auxiliar para eliminar el elemento máximo de un árbol binario de búsqueda.
eliminarMax :: Arbol a -> (a, Arbol a)
eliminarMax Vacío = error "No se puede eliminar el máximo de un árbol vacío."
eliminarMax (Nodo x izq Vacío) = (x, izq)
eliminarMax (Nodo x izq der) =
  let (y, nuevoIzq) = eliminarMax izq
  in (y, Nodo x nuevoIzq der)

-- Función para imprimir un árbol binario de búsqueda.
imprimir :: Show a => Arbol a -> String
imprimir Vacío = ""
imprimir (Nodo x izq der) = intercalar "\n" [imprimir izq, show x, imprimir der]

-- Definimos un tipo de dato para representar una cola de prioridad.
data ColaPrioridad a = ColaPrioridad [(a, Int)]

-- Función para insertar un elemento en una cola de prioridad.
insertarColaPrioridad :: Ord a => a -> ColaPrioridad a -> ColaPrioridad a
insertarColaPrioridad x (ColaPrioridad xs) =
  ColaPrioridad (insertarAux x xs)

-- Función auxiliar para insertar un elemento en una cola de prioridad.
insertarAux :: Ord a => a -> [(a, Int)] -> [(a, Int)]
insertarAux x [] = [(x, 1)]
insertarAux x xs@(y:ys)
  | x == y = (x, fst y + 1) : ys
  | x < y = (x, 1) : xs
  | otherwise = y : insertarAux x ys

-- Función para eliminar el elemento con mayor prioridad de una cola de prioridad.
eliminarColaPrioridad :: ColaPrioridad a -> ColaPrioridad a
eliminarColaPrioridad (ColaPrioridad []) = error "No se puede eliminar el elemento con mayor prioridad de una cola de prioridad vacía."
eliminarColaPrioridad (ColaPrioridad ((x, _):xs)) = ColaPrioridad xs

-- Función para obtener el elemento con mayor prioridad de una cola de prioridad.
primeroColaPrioridad :: ColaPrioridad a -> a
primeroColaPrioridad (ColaPrioridad []) = error "No se puede obtener el elemento con mayor prioridad de una cola de prioridad vacía."
primeroColaPrioridad (ColaPrioridad ((x, _):_)) = x

-- Función para imprimir una cola de prioridad.
imprimirColaPrioridad :: Show a => ColaPrioridad a -> String
imprimirColaPrioridad (ColaPrioridad []) = ""
imprimirColaPrioridad (ColaPrioridad ((x, n):xs)) = intercalar "\n" [show x ++ " (" ++ show n ++ ")", imprimirColaPrioridad (ColaPrioridad xs)]
```

Este código define varios tipos de datos y funciones para trabajar con árboles binarios de búsqueda y colas de prioridad en Haskell.

El tipo de datos `Arbol a` representa un árbol binario de búsqueda, que es un tipo de estructura de datos en forma de árbol en la que cada nodo tiene un valor y dos subárboles, uno a la izquierda y otro a la derecha. Los valores en el árbol se ordenan de menor a mayor de izquierda a derecha.

Las funciones `insertar`, `buscar` y `eliminar` permiten insertar, buscar y eliminar elementos de un árbol binario de búsqueda, respectivamente.

El tipo de datos `ColaPrioridad a` representa una cola de prioridad, que es un tipo de estructura de datos en forma de cola en la que los elementos se ordenan por su prioridad, de mayor a menor.

Las funciones `insertarColaPrioridad`, `eliminarColaPrioridad` y `primeroColaPrioridad` permiten insertar, eliminar y obtener el elemento con mayor prioridad de una cola de prioridad, respectivamente.

La función `imprimir` permite imprimir un árbol binario de búsqueda o una cola de prioridad.