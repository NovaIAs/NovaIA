```haskell
-- Definición de un tipo de dato para representar un árbol binario
data Árbol a = Hoja a | Rama (Árbol a) a (Árbol a)
  deriving (Eq, Ord, Show)

-- Función para crear un árbol binario a partir de una lista de elementos
crearÁrbol :: [a] -> Árbol a
crearÁrbol [] = Hoja 0
crearÁrbol (x:xs) = Rama (crearÁrbol xs) x (crearÁrbol xs)

-- Función para buscar un elemento en un árbol binario
buscar :: Eq a => a -> Árbol a -> Bool
buscar x (Hoja y) = x == y
buscar x (Rama izq y der) = x == y || buscar x izq || buscar x der

-- Función para insertar un elemento en un árbol binario
insertar :: Ord a => a -> Árbol a -> Árbol a
insertar x (Hoja y) = Rama (Hoja x) x (Hoja x)
insertar x (Rama izq y der)
  | x <= y = Rama (insertar x izq) y der
  | otherwise = Rama izq y (insertar x der)

-- Función para eliminar un elemento de un árbol binario
eliminar :: Ord a => a -> Árbol a -> Árbol a
eliminar x (Hoja y) = Hoja 0
eliminar x (Rama izq y der)
  | x < y = Rama (eliminar x izq) y der
  | x > y = Rama izq y (eliminar x der)
  | otherwise = merge izq der

-- Función para fusionar dos árboles binarios
merge :: Árbol a -> Árbol a -> Árbol a
merge (Hoja x) (Hoja y) = Hoja (x + y)
merge (Rama izq x der) (Rama izq2 y der2)
  = Rama (merge izq izq2) (x + y) (merge der der2)

-- Función para imprimir un árbol binario en forma de cadena
imprimir :: Árbol a -> String
imprimir (Hoja x) = show x
imprimir (Rama izq x der) = "(" ++ imprimir izq ++ ", " ++ show x ++ ", " ++ imprimir der ++ ")"

-- Función principal
main :: IO ()
main = do
  let árbol = crearÁrbol [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  print (buscar 5 árbol)  -- True
  print (insertar 11 árbol)
  print (eliminar 7 árbol)
  print (merge árbol árbol)
  print (imprimir árbol)
```

Explicación del código:

* El tipo de dato `Árbol a` representa un árbol binario genérico, donde `a` es el tipo de los elementos del árbol. Un árbol binario es una estructura de datos que consta de un nodo raíz y dos subárboles, llamados árbol izquierdo y árbol derecho.
* La función `crearÁrbol` crea un árbol binario a partir de una lista de elementos. El árbol se crea de forma recursiva, comenzando por el nodo raíz y añadiendo los elementos de la lista como subárboles izquierdo y derecho.
* La función `buscar` busca un elemento en un árbol binario. La función también se implementa de forma recursiva, comenzando por el nodo raíz y recorriendo el árbol en orden hasta encontrar el elemento buscado.
* La función `insertar` inserta un elemento en un árbol binario. La función también se implementa de forma recursiva, comenzando por el nodo raíz y recorriendo el árbol en orden hasta encontrar el lugar adecuado para insertar el elemento.
* La función `eliminar` elimina un elemento de un árbol binario. La función también se implementa de forma recursiva, comenzando por el nodo raíz y recorriendo el árbol en orden hasta encontrar el elemento que se va a eliminar.
* La función `merge` fusiona dos árboles binarios en un solo árbol. La función también se implementa de forma recursiva, comenzando por los nodos raíz de los dos árboles y fusionando los subárboles izquierdo y derecho de cada árbol.
* La función `imprimir` imprime un árbol binario en forma de cadena. La función también se implementa de forma recursiva, comenzando por el nodo raíz y recorriendo el árbol en orden.

La función `main` es la función principal del programa. En esta función se crea un árbol binario a partir de una lista de elementos, se busca un elemento en el árbol, se inserta un nuevo elemento en el árbol, se elimina un elemento del árbol, se fusionan dos árboles binarios y se imprime el árbol resultante.