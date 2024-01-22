```haskell
-- Definición de tipos de datos

data Lista a = Vacia | Cons a (Lista a)

data Árbol a = Hoja a | Rama (Árbol a) a (Árbol a)

-- Funciones de listas

-- Devuelve el primer elemento de una lista, si existe
primero :: Lista a -> Maybe a
primero Vacia = Nothing
primero (Cons x _) = Just x

-- Devuelve el resto de una lista, sin el primer elemento, si existe
resto :: Lista a -> Maybe (Lista a)
resto Vacia = Nothing
resto (Cons _ xs) = Just xs

-- Devuelve la longitud de una lista
longitud :: Lista a -> Int
longitud Vacia = 0
longitud (Cons _ xs) = 1 + longitud xs

-- Devuelve el elemento en la posición especificada de una lista, si existe
elementoEnPosición :: Int -> Lista a -> Maybe a
elementoEnPosición _ Vacia = Nothing
elementoEnPosición 0 (Cons x _) = Just x
elementoEnPosición n (Cons _ xs) = elementoEnPosición (n-1) xs

-- Funciones de árboles

-- Devuelve el valor del nodo raíz de un árbol, si existe
raiz :: Árbol a -> Maybe a
raiz (Hoja x) = Just x
raiz (Rama _ x _) = Just x

-- Devuelve los subárboles izquierdo y derecho de un árbol, si existen
subárboles :: Árbol a -> Maybe (Árbol a, Árbol a)
subárboles (Hoja _) = Nothing
subárboles (Rama l x r) = Just (l, r)

-- Devuelve el árbol resultante de insertar un nuevo valor en un árbol binario de búsqueda
insertar :: Ord a => a -> Árbol a -> Árbol a
insertar x (Hoja _) = Hoja x
insertar x (Rama l y r) | x <= y = Rama (insertar x l) y r
                       | otherwise = Rama l y (insertar x r)

-- Funciones de búsqueda

-- Devuelve el valor buscado en un árbol binario de búsqueda, si existe
buscar :: Ord a => a -> Árbol a -> Maybe a
buscar x (Hoja _) = Nothing
buscar x (Rama l y r) | x == y = Just y
                      | x < y  = buscar x l
                      | otherwise = buscar x r

-- Funciones de ordenación

-- Devuelve una lista ordenada de los valores de un árbol binario de búsqueda
ordenar :: Árbol a -> Lista a
ordenar (Hoja x) = Cons x Vacia
ordenar (Rama l x r) = ordenar l ++ Cons x (ordenar r)

-- Ejemplo de uso

-- Crear un árbol binario de búsqueda
árbol = Rama (Rama (Hoja 1) 3 (Rama (Hoja 2) 4 (Hoja 5))) 6 (Rama (Hoja 7) 8 (Hoja 9))

-- Buscar un valor en el árbol
resultado = buscar 4 árbol

-- Imprimir el resultado
print resultado

```

Este código implementa una serie de funciones para trabajar con listas y árboles binarios de búsqueda. Las funciones de listas incluyen: `primero`, `resto`, `longitud` y `elementoEnPosición`. Las funciones de árboles incluyen: `raiz`, `subárboles`, `insertar`, `buscar` y `ordenar`.

El ejemplo de uso muestra cómo crear un árbol binario de búsqueda, buscar un valor en el árbol y imprimir el resultado.

El código está escrito en Haskell, un lenguaje de programación funcional puro y de tipado estático. Haskell es conocido por su elegante sintaxis y su sistema de tipos seguro.