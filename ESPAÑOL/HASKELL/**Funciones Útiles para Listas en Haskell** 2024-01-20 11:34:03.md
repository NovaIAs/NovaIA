```haskell
-- Definición del tipo de datos Lista
data Lista a = Vacía | Cons a (Lista a)

-- Función para concatenar dos listas
concatenar :: Lista a -> Lista a -> Lista a
concatenar Vacía ys = ys
concatenar (Cons x xs) ys = Cons x (concatenar xs ys)

-- Función para invertir una lista
invertir :: Lista a -> Lista a
invertir Vacía = Vacía
invertir (Cons x xs) = concatenar (invertir xs) (Cons x Vacía)

-- Función para filtrar una lista
filtrar :: (a -> Bool) -> Lista a -> Lista a
filtrar p Vacía = Vacía
filtrar p (Cons x xs)
  | p x = Cons x (filtrar p xs)
  | otherwise = filtrar p xs

-- Función para mapear una función sobre una lista
mapear :: (a -> b) -> Lista a -> Lista b
mapear f Vacía = Vacía
mapear f (Cons x xs) = Cons (f x) (mapear f xs)

-- Función para reducir una lista a un valor
reducir :: (a -> b -> a) -> a -> Lista b -> a
reducir f v Vacía = v
reducir f v (Cons x xs) = reducir f (f v x) xs

-- Función para obtener el máximo de una lista de números
máximo :: Lista Int -> Int
máximo Vacía = error "La lista está vacía"
máximo (Cons x xs) = reducir max x xs

-- Función para obtener el mínimo de una lista de números
mínimo :: Lista Int -> Int
mínimo Vacía = error "La lista está vacía"
mínimo (Cons x xs) = reducir min x xs

-- Función para ordenar una lista de números
ordenar :: Lista Int -> Lista Int
ordenar Vacía = Vacía
ordenar (Cons x xs) = insertar x (ordenar xs)

insertar :: Int -> Lista Int -> Lista Int
insertar x Vacía = Cons x Vacía
insertar x (Cons y ys)
  | x <= y = Cons x (Cons y ys)
  | otherwise = Cons y (insertar x ys)
```

Este código define varias funciones útiles para trabajar con listas en Haskell. Las funciones incluyen:

* `concatenar`: Concatena dos listas.
* `invertir`: Invierte una lista.
* `filtrar`: Filtra una lista según un predicado.
* `mapear`: Mapea una función sobre una lista.
* `reducir`: Reduce una lista a un valor.
* `máximo`: Obtiene el máximo de una lista de números.
* `mínimo`: Obtiene el mínimo de una lista de números.
* `ordenar`: Ordena una lista de números.

El código utiliza el tipo de datos `Lista` para representar listas. Una lista puede ser vacía (representada por `Vacía`) o puede ser un elemento seguido de una lista (representado por `Cons`).

Las funciones se definen utilizando la sintaxis de Haskell. Las funciones toman uno o más argumentos y devuelven un valor. El cuerpo de una función se define utilizando la palabra clave `where`.

El código utiliza varias funciones auxiliares, como `insertar`, para implementar las funciones principales. La función `insertar` se utiliza para insertar un elemento en una lista ordenada.

El código utiliza el módulo `Prelude` para importar las funciones estándar de Haskell, como `max` y `min`.