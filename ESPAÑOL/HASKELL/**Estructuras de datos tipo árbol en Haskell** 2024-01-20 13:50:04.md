```haskell
-- Definición de datos
data Arbol a = Hoja a
            | Rama (Arbol a) a (Arbol a)

-- Función de búsqueda
busqueda :: (Eq a) => a -> Arbol a -> Bool
busqueda x (Hoja y) = x == y
busqueda x (Rama izq y der) = x == y || busqueda x izq || busqueda x der

-- Función de inserción
insercion :: (Ord a) => a -> Arbol a -> Arbol a
insercion x (Hoja y) = Rama (Hoja x) y (Hoja x)
insercion x (Rama izq y der)
  | x <= y    = Rama (insercion x izq) y der
  | otherwise = Rama izq y (insercion x der)

-- Función de eliminación
eliminacion :: (Eq a) => a -> Arbol a -> Arbol a
eliminacion x (Hoja y) = Hoja y
eliminacion x (Rama izq y der)
  | x == y    = fusion izq der
  | x < y     = Rama (eliminacion x izq) y der
  | otherwise = Rama izq y (eliminacion x der)
  where
    fusion :: Arbol a -> Arbol a -> Arbol a
    fusion (Hoja x) (Hoja y) = Hoja x
    fusion (Rama izq1 y1 der1) (Rama izq2 y2 der2)
      | y1 <= y2  = Rama (fusion izq1 izq2) y1 (der1 `union` der2)
      | otherwise = Rama (izq1 `union` izq2) y2 (fusion der1 der2)

-- Función de unión de árboles
union :: Arbol a -> Arbol a -> Arbol a
union (Hoja x) (Hoja y) = Hoja x
union (Rama izq1 y1 der1) (Rama izq2 y2 der2)
  | y1 <= y2  = Rama (union izq1 izq2) y1 (der1 `union` der2)
  | otherwise = Rama (izq1 `union` izq2) y2 (fusion der1 der2)

-- Función principal
main :: IO ()
main = do
  let arbol = Rama (Rama (Hoja 1) 2 (Hoja 3)) 4 (Rama (Hoja 5) 6 (Hoja 7))
  print $ busqueda 4 arbol
  let arbol2 = insercion 8 arbol
  print arbol2
  let arbol3 = eliminacion 2 arbol2
  print arbol3
```

Este código define una estructura de datos llamada `Arbol` que puede almacenar valores de tipo `a`. Los árboles pueden ser hojas, que contienen un solo valor, o ramas, que contienen una hoja, un valor y otra hoja.

El código también define varias funciones que pueden operar en árboles. La función `busqueda` busca un valor en un árbol y devuelve `True` si el valor se encuentra en el árbol. La función `insercion` inserta un valor en un árbol y devuelve un nuevo árbol con el valor insertado. La función `eliminacion` elimina un valor de un árbol y devuelve un nuevo árbol con el valor eliminado. La función `union` fusiona dos árboles en uno solo.

La función `main` es la función principal del programa. Crea un árbol de ejemplo, imprime el resultado de la búsqueda de un valor en el árbol, inserta un valor en el árbol, elimina un valor del árbol y finalmente imprime el árbol resultante.