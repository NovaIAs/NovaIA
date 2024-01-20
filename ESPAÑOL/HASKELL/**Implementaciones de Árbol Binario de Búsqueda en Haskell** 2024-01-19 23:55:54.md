```haskell
-- Definición de tipos

-- Tipo de dato para representar un árbol binario de búsqueda
data Árbol a = Vacío | Nodo a (Árbol a) (Árbol a)

-- Tipo de dato para representar una lista enlazada
data List a = Nil | Cons a (List a)

-- Definición de funciones

-- Función para insertar un elemento en un árbol binario de búsqueda
insertar :: Ord a => a -> Árbol a -> Árbol a
insertar x Vacío = Nodo x Vacío Vacío
insertar x (Nodo y izq der)
  | x < y    = Nodo y (insertar x izq) der
  | x > y    = Nodo y izq (insertar x der)
  | otherwise = Nodo x izq der

-- Función para buscar un elemento en un árbol binario de búsqueda
buscar :: Ord a => a -> Árbol a -> Bool
buscar x Vacío = False
buscar x (Nodo y izq der)
  | x < y    = buscar x izq
  | x > y    = buscar x der
  | otherwise = True

-- Función para eliminar un elemento de un árbol binario de búsqueda
eliminar :: Ord a => a -> Árbol a -> Árbol a
eliminar x Vacío = Vacío
eliminar x (Nodo y izq der)
  | x < y    = Nodo y (eliminar x izq) der
  | x > y    = Nodo y izq (eliminar x der)
  | otherwise = combinar izq der

combinar :: Árbol a -> Árbol a -> Árbol a
combinar Vacío der = der
combinar izq Vacío = izq
combinar (Nodo x izq1 der1) (Nodo y izq2 der2)
  | x < y    = Nodo x izq1 (combinar der1 (Nodo y izq2 der2))
  | otherwise = Nodo y (combinar (Nodo x izq1 der1) izq2) der2

-- Función para recorrer un árbol binario de búsqueda en orden
recorrerEnOrden :: Árbol a -> [a]
recorrerEnOrden Vacío = []
recorrerEnOrden (Nodo x izq der) = recorrerEnOrden izq ++ [x] ++ recorrerEnOrden der

-- Función para recorrer un árbol binario de búsqueda en preorden
recorrerEnPreorden :: Árbol a -> [a]
recorrerEnPreorden Vacío = []
recorrerEnPreorden (Nodo x izq der) = [x] ++ recorrerEnPreorden izq ++ recorrerEnPreorden der

-- Función para recorrer un árbol binario de búsqueda en postorden
recorrerEnPostorden :: Árbol a -> [a]
recorrerEnPostorden Vacío = []
recorrerEnPostorden (Nodo x izq der) = recorrerEnPostorden izq ++ recorrerEnPostorden der ++ [x]

-- Función para convertir un árbol binario de búsqueda en una lista enlazada
convertirEnLista :: Árbol a -> List a
convertirEnLista Vacío = Nil
convertirEnLista (Nodo x izq der) = Cons x (convertirEnLista izq ++ convertirEnLista der)

-- Función para convertir una lista enlazada en un árbol binario de búsqueda
convertirEnÁrbol :: List a -> Árbol a
convertirEnÁrbol Nil = Vacío
convertirEnÁrbol (Cons x xs) = insertar x (convertirEnÁrbol xs)

-- Función para imprimir un árbol binario de búsqueda
imprimirÁrbol :: Árbol a -> String
imprimirÁrbol Vacío = ""
imprimirÁrbol (Nodo x izq der) = imprimirÁrbol izq ++ x ++ imprimirÁrbol der

-- Función principal
main :: IO ()
main = do
  -- Crear un árbol binario de búsqueda
  let árbol = insertar 5 (insertar 3 (insertar 2 (Vacío) (Vacío)) (insertar 4 (Vacío) (Vacío))) (insertar 7 (insertar 6 (Vacío) (Vacío)) (insertar 8 (Vacío) (Vacío)))

  -- Buscar un elemento en el árbol binario de búsqueda
  let resultado = buscar 4 árbol

  -- Eliminar un elemento del árbol binario de búsqueda
  let árbolEliminado = eliminar 5 árbol

  -- Recorrer el árbol binario de búsqueda en orden
  let recorridoEnOrden = recorrerEnOrden árbol

  -- Recorrer el árbol binario de búsqueda en preorden
  let recorridoEnPreorden = recorrerEnPreorden árbol

  -- Recorrer el árbol binario de búsqueda en postorden
  let recorridoEnPostorden = recorrerEnPostorden árbol

  -- Convertir el árbol binario de búsqueda en una lista enlazada
  let lista = convertirEnLista árbol

  -- Convertir la lista enlazada en un árbol binario de búsqueda
  let árbol2 = convertirEnÁrbol lista

  -- Imprimir el árbol binario de búsqueda
  let árbolImpreso = imprimirÁrbol árbol

  -- Mostrar los resultados
  putStrLn $ "Resultado de la búsqueda: " ++ show resultado
  putStrLn $ "Árbol eliminado: " ++ imprimirÁrbol árbolEliminado
  putStrLn $ "Recorrido en orden: " ++ show recorridoEnOrden
  putStrLn $ "Recorrido en preorden: " ++ show recorridoEnPreorden
  putStrLn $ "Recorrido en postorden: " ++ show recorridoEnPostorden
  putStrLn $ "Lista: " ++ show lista
  putStrLn $ "Árbol2: " ++ imprimirÁrbol árbol2
  putStrLn $ "Árbol impreso: " ++ árbolImpreso
```

Explicación del código:

* La función `insertar` inserta un elemento en un árbol binario de búsqueda. Si el árbol está vacío, lo crea con el elemento dado como raíz. Si ya existe un nodo con el mismo valor que el elemento dado, el elemento no se inserta. De lo contrario, el elemento se inserta en el nodo izquierdo o derecho, según sea menor o mayor que el valor del nodo.

* La función `buscar` busca un elemento en un árbol binario de búsqueda. Si el árbol está vacío, la búsqueda falla. De lo contrario, el elemento se busca en el nodo izquierdo o derecho, según sea menor o mayor que el valor del nodo. Si el elemento se encuentra, se devuelve `True`; de lo contrario, se devuelve `False`.

* La función `eliminar` elimina un elemento de un árbol binario de búsqueda. Si el árbol está vacío, no hay nada que eliminar. De lo contrario, el elemento se elimina del nodo izquierdo o derecho, según sea menor o mayor que el valor del nodo. Si el elemento no se encuentra, el árbol no se modifica.

* La función `combinar` combina dos árboles binarios de búsqueda en un solo árbol binario de búsqueda. El árbol resultante es el árbol más pequeño que contiene todos los elementos de los dos árboles dados.

* La función `recorrerEnOrden` recorre un árbol binario de búsqueda en orden. Esto significa que visita los nodos del árbol en orden ascendente.

* La función `recorrerEnPreorden` recorre un árbol binario de búsqueda en preorden. Esto significa que visita la raíz del árbol primero, luego los nodos del subárbol izquierdo y finalmente los nodos del subárbol derecho.

* La función `recorrerEnPostorden` recorre un árbol binario de búsqueda en postorden. Esto significa que visita los nodos del subárbol izquierdo primero, luego los nodos del subárbol derecho y finalmente la raíz del árbol.

* La función `convertirEnLista` convierte un árbol binario de búsqueda en una lista enlazada. La lista resultante contiene todos los elementos del árbol en orden ascendente.

* La función `convertirEnÁrbol` convierte una lista enlazada en un árbol binario de búsqueda. El árbol resultante es el árbol más pequeño que contiene todos los elementos de la lista dada.

* La función `imprimirÁrbol` imprime un árbol binario de búsqueda en una cadena de texto. La cadena resultante muestra el árbol en una representación gráfica.

* La función `main` es la función principal del programa. En esta función se crean varios árboles binarios de búsqueda, se realizan búsquedas y eliminaciones, se recorren los árboles en diferentes órdenes y se convierten en listas enlazadas y de vuelta en árboles binarios de búsqueda. Finalmente, se imprimen los resultados de las operaciones realizadas.