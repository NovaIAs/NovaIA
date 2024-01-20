```haskell
-- Implementación de un árbol binario de búsqueda en Haskell

-- Definición del tipo de datos del árbol binario de búsqueda
data Arbol a = Nulo | Nodo a (Arbol a) (Arbol a)

-- Función para insertar un elemento en el árbol
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Nulo = Nodo x Nulo Nulo
insertar x (Nodo y izq der)
  | x < y     = Nodo y (insertar x izq) der
  | otherwise = Nodo y izq (insertar x der)

-- Función para buscar un elemento en el árbol
buscar :: Ord a => a -> Arbol a -> Bool
buscar x Nulo = False
buscar x (Nodo y izq der)
  | x == y    = True
  | x < y     = buscar x izq
  | otherwise = buscar x der

-- Función para eliminar un elemento del árbol
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x Nulo = Nulo
eliminar x (Nodo y izq der)
  | x == y    = eliminarMinimo der
  | x < y     = Nodo y (eliminar x izq) der
  | otherwise = Nodo y izq (eliminar x der)

-- Función para eliminar el elemento mínimo del árbol
eliminarMinimo :: Ord a => Arbol a -> Arbol a
eliminarMinimo Nulo = Nulo
eliminarMinimo (Nodo y Nulo der) = der
eliminarMinimo (Nodo y izq der) = Nodo y (eliminarMinimo izq) der

-- Función para imprimir el árbol en orden
imprimirOrden :: Ord a => Arbol a -> [a]
imprimirOrden Nulo = []
imprimirOrden (Nodo y izq der) = imprimirOrden izq ++ [y] ++ imprimirOrden der

-- Función para imprimir el árbol en preorden
imprimirPreorden :: Ord a => Arbol a -> [a]
imprimirPreorden Nulo = []
imprimirPreorden (Nodo y izq der) = [y] ++ imprimirPreorden izq ++ imprimirPreorden der

-- Función para imprimir el árbol en postorden
imprimirPostorden :: Ord a => Arbol a -> [a]
imprimirPostorden Nulo = []
imprimirPostorden (Nodo y izq der) = imprimirPostorden izq ++ imprimirPostorden der ++ [y]

-- Ejemplo de uso del árbol binario de búsqueda
main :: IO ()
main = do
  -- Crear un árbol binario de búsqueda vacío
  let arbol = Nulo

  -- Insertar elementos en el árbol
  let arbolConElementos = insertar 10 arbol
                       >>= insertar 5 arbolConElementos
                       >>= insertar 15 arbolConElementos
                       >>= insertar 2 arbolConElementos
                       >>= insertar 7 arbolConElementos
                       >>= insertar 12 arbolConElementos
                       >>= insertar 20 arbolConElementos

  -- Buscar un elemento en el árbol
  let elementoBuscado = 12
  let resultadoBusqueda = buscar elementoBuscado arbolConElementos
  putStrLn $ "El elemento " ++ show elementoBuscado ++ " " ++ (if resultadoBusqueda then "sí" else "no") ++ " está en el árbol"

  -- Eliminar un elemento del árbol
  let elementoAEliminar = 5
  let arbolSinElemento = eliminar elementoAEliminar arbolConElementos

  -- Imprimir el árbol en orden
  let orden = imprimirOrden arbolSinElemento
  putStrLn $ "El árbol en orden es: " ++ show orden

  -- Imprimir el árbol en preorden
  let preorden = imprimirPreorden arbolSinElemento
  putStrLn $ "El árbol en preorden es: " ++ show preorden

  -- Imprimir el árbol en postorden
  let postorden = imprimirPostorden arbolSinElemento
  putStrLn $ "El árbol en postorden es: " ++ show postorden
```

Explicación del código:

* El código define un tipo de datos llamado `Arbol a`, que representa un árbol binario de búsqueda. Un árbol binario de búsqueda es un árbol binario en el que cada nodo tiene un valor y sus hijos izquierdo y derecho contienen valores menores y mayores que el valor del nodo, respectivamente.
* El código define una serie de funciones para trabajar con árboles binarios de búsqueda, incluyendo:
    * `insertar`: Inserta un elemento en el árbol.
    * `buscar`: Busca un elemento en el árbol.
    * `eliminar`: Elimina un elemento del árbol.
    * `eliminarMinimo`: Elimina el elemento mínimo del árbol.
    * `imprimirOrden`: Imprime el árbol en orden.
    * `imprimirPreorden`: Imprime el árbol en preorden.
    * `imprimirPostorden`: Imprime el árbol en postorden.
* El código también incluye un ejemplo de uso del árbol binario de búsqueda. El ejemplo crea un árbol binario de búsqueda vacío, inserta algunos elementos en el árbol, busca un elemento en el árbol, elimina un elemento del árbol e imprime el árbol en orden, preorden y postorden.