```haskell
-- Definición de tipos de datos:

data Árbol a = Hoja a | Rama (Árbol a) a (Árbol a)
              deriving (Show, Eq)

data Lista a = Vacía | Cons a (Lista a)
              deriving (Show, Eq)

-- Funciones de recorrido de árboles:

recorridoEnOrden :: Árbol a -> [a]
recorridoEnOrden (Hoja x) = [x]
recorridoEnOrden (Rama izq x der) = recorridoEnOrden izq ++ [x] ++ recorridoEnOrden der

recorridoPreOrden :: Árbol a -> [a]
recorridoPreOrden (Hoja x) = [x]
recorridoPreOrden (Rama izq x der) = [x] ++ recorridoPreOrden izq ++ recorridoPreOrden der

recorridoPostOrden :: Árbol a -> [a]
recorridoPostOrden (Hoja x) = [x]
recorridoPostOrden (Rama izq x der) = recorridoPostOrden izq ++ recorridoPostOrden der ++ [x]

-- Funciones de búsqueda en árboles:

buscar :: Eq a => a -> Árbol a -> Maybe a
buscar x (Hoja y) = if x == y then Just x else Nothing
buscar x (Rama izq y der) = case comparar x y of
  LT -> buscar x izq
  EQ -> Just y
  GT -> buscar x der
  where
    comparar :: Ord a => a -> a -> Ordering
    comparar x y
      | x < y = LT
      | x > y = GT
      | otherwise = EQ

-- Función para crear una lista en orden a partir de un árbol:

listaEnOrden :: Árbol a -> Lista a
listaEnOrden (Hoja x) = Cons x Vacía
listaEnOrden (Rama izq x der) = concatenar (listaEnOrden izq) (Cons x (listaEnOrden der))
  where
    concatenar :: Lista a -> Lista a -> Lista a
    concatenar (Cons x xs) ys = Cons x (concatenar xs ys)
    concatenar Vacía ys = ys

-- Ejemplo de uso:

miÁrbol :: Árbol Int
miÁrbol = Rama (Rama (Hoja 1) 2 (Hoja 3)) 4 (Rama (Hoja 5) 6 (Hoja 7))

resultadoEnOrden = recorridoEnOrden miÁrbol
resultadoPreOrden = recorridoPreOrden miÁrbol
resultadoPostOrden = recorridoPostOrden miÁrbol

resultadoBuscar1 = buscar 4 miÁrbol
resultadoBuscar2 = buscar 8 miÁrbol

resultadoListaEnOrden = listaEnOrden miÁrbol

-- Impresión de los resultados:

putStrLn "Recorrido en orden:"
print resultadoEnOrden

putStrLn "Recorrido pre-orden:"
print resultadoPreOrden

putStrLn "Recorrido post-orden:"
print resultadoPostOrden

putStrLn "Búsqueda del elemento 4:"
print resultadoBuscar1

putStrLn "Búsqueda del elemento 8:"
print resultadoBuscar2

putStrLn "Lista en orden:"
print resultadoListaEnOrden
```

Explicación del código:

1. **Definición de tipos de datos:**

   - **`Árbol a`**: Representa un árbol binario genérico que puede almacenar valores de tipo `a`. 
   - **`Lista a`**: Representa una lista genérica de valores de tipo `a`.

2. **Funciones de recorrido de árboles:**

   - **`recorridoEnOrden`**: Realiza un recorrido en orden del árbol, visitando primero el subárbol izquierdo, luego la raíz y finalmente el subárbol derecho.
   - **`recorridoPreOrden`**: Realiza un recorrido pre-orden del árbol, visitando primero la raíz, luego el subárbol izquierdo y finalmente el subárbol derecho.
   - **`recorridoPostOrden`**: Realiza un recorrido post-orden del árbol, visitando primero el subárbol izquierdo, luego el subárbol derecho y finalmente la raíz.

3. **Funciones de búsqueda en árboles:**

   - **`buscar`**: Busca un valor en un árbol. 
   - **`comparar`**: Función auxiliar que compara dos valores.

4. **Función para crear una lista en orden a partir de un árbol:**

   - **`listaEnOrden`**: Convierte un árbol en una lista en orden.

5. **Ejemplo de uso:**

   - Se define un árbol binario de ejemplo.
   - Se llaman a las funciones de recorrido y búsqueda para obtener los resultados.
   - Se convierte el árbol en una lista en orden.

6. **Impresión de los resultados:**

   - Se imprimen los resultados en la consola.