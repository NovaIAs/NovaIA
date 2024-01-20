```haskell
-- Definición de tipos de datos
data Arbol a = Hoja a | Rama (Arbol a) a (Arbol a)
  deriving (Show, Eq, Ord)

-- Función para crear un nuevo árbol
crearArbol :: a -> Arbol a
crearArbol x = Hoja x

-- Función para insertar un nuevo elemento en el árbol
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x (Hoja y) = Rama (Hoja y) x (Hoja y)
insertar x (Rama izq y der)
  | x <= y    = Rama (insertar x izq) y der
  | otherwise = Rama izq y (insertar x der)

-- Función para buscar un elemento en el árbol
buscar :: Ord a => a -> Arbol a -> Bool
buscar x (Hoja y) = x == y
buscar x (Rama izq y der)
  | x < y     = buscar x izq
  | x > y     = buscar x der
  | otherwise = True

-- Función para eliminar un elemento del árbol
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x (Hoja y) = Hoja y
eliminar x (Rama izq y der)
  | x == y    = combinar izq der
  | x < y     = Rama (eliminar x izq) y der
  | otherwise = Rama izq y (eliminar x der)

-- Función para combinar dos árboles
combinar :: Arbol a -> Arbol a -> Arbol a
combinar (Hoja x) (Hoja y) = Hoja (min x y)
combinar (Hoja x) (Rama izq y der) = Rama (Hoja x) y der
combinar (Rama izq y der) (Hoja x) = Rama izq y (Hoja x)
combinar (Rama izq1 y1 der1) (Rama izq2 y2 der2) = Rama (combinar izq1 izq2) y1 (combinar der1 der2)

-- Usando el árbol binario para resolver un problema
problema :: [Int] -> Int
problema xs = altura (crearArbol xs)

-- Función para calcular la altura del árbol
altura :: Arbol a -> Int
altura (Hoja _) = 0
altura (Rama izq _ der) = 1 + max (altura izq) (altura der)

```

Este código Haskell define una estructura de datos de árbol binario y varias funciones para trabajar con él. El árbol binario está representado usando la estructura de datos `Arbol a` con dos constructores: `Hoja a` y `Rama (Arbol a) a (Arbol a)`. El constructor `Hoja` representa una hoja del árbol, mientras que el constructor `Rama` representa una rama que tiene un subárbol izquierdo, un valor y un subárbol derecho.

La función `crearArbol` crea un nuevo árbol binario a partir de un valor dado. La función `insertar` inserta un nuevo valor en el árbol binario. La función `buscar` busca un valor dado en el árbol binario y devuelve un valor booleano indicando si el valor fue encontrado o no. La función `eliminar` elimina un valor dado del árbol binario. La función `combinar` combina dos árboles binarios en un solo árbol binario.

La función `problema` usa el árbol binario para resolver un problema específico. En este caso, el problema es encontrar la altura del árbol binario, que se define como el número máximo de nodos desde la raíz hasta cualquier hoja. La función `altura` calcula la altura del árbol binario.

Este código Haskell es complejo porque define una estructura de datos sofisticada y varias funciones para trabajar con ella. También usa la estructura de datos del árbol binario para resolver un problema específico. Este código es útil para aprender sobre estructuras de datos avanzadas y cómo usarlas para resolver problemas.