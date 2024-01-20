```haskell

-- Definimos el tipo de datos "Lista" como una lista enlazada de elementos de tipo "a".
data Lista a = Vacía | Nodo a (Lista a)

-- Definimos la función "agregar" que agrega un elemento al final de una lista.
agregar :: a -> Lista a -> Lista a
agregar x xs = Nodo x xs

-- Definimos la función "quitar" que quita el primer elemento de una lista.
quitar :: Lista a -> (a, Lista a)
quitar (Nodo x xs) = (x, xs)
quitar Vacía = error "Lista vacía"

-- Definimos la función "buscar" que busca un elemento en una lista.
buscar :: Eq a => a -> Lista a -> Bool
buscar x Vacía = False
buscar x (Nodo y ys) = x == y || buscar x ys

-- Definimos la función "ordenar" que ordena una lista.
ordenar :: Ord a => Lista a -> Lista a
ordenar Vacía = Vacía
ordenar (Nodo x xs) = insertar x (ordenar xs)

-- Definimos la función "insertar" que inserta un elemento en una lista ordenada.
insertar :: Ord a => a -> Lista a -> Lista a
insertar x Vacía = Nodo x Vacía
insertar x (Nodo y ys)
  | x <= y    = Nodo x (insertar x ys)
  | otherwise = Nodo y (insertar x ys)

-- Definimos la función "invertir" que invierte una lista.
invertir :: Lista a -> Lista a
invertir Vacía = Vacía
invertir (Nodo x xs) = invertir xs ++ [x]

-- Definimos la función "concatenar" que concatena dos listas.
concatenar :: Lista a -> Lista a -> Lista a
concatenar Vacía ys = ys
concatenar (Nodo x xs) ys = Nodo x (concatenar xs ys)

-- Definimos la función "longitud" que calcula la longitud de una lista.
longitud :: Lista a -> Int
longitud Vacía = 0
longitud (Nodo x xs) = 1 + longitud xs

-- Definimos la función "map" que aplica una función a cada elemento de una lista.
map :: (a -> b) -> Lista a -> Lista b
map f Vacía = Vacía
map f (Nodo x xs) = Nodo (f x) (map f xs)

-- Definimos la función "filter" que filtra los elementos de una lista según un predicado.
filter :: (a -> Bool) -> Lista a -> Lista a
filter p Vacía = Vacía
filter p (Nodo x xs)
  | p x       = Nodo x (filter p xs)
  | otherwise = filter p xs

-- Definimos la función "foldr" que reduce una lista a un valor único mediante una función binaria.
foldr :: (a -> b -> b) -> b -> Lista a -> b
foldr f z Vacía = z
foldr f z (Nodo x xs) = f x (foldr f z xs)

-- Definimos la función "sumar" que suma los elementos de una lista.
sumar :: Num a => Lista a -> a
sumar = foldr (+) 0

-- Definimos la función "producto" que calcula el producto de los elementos de una lista.
producto :: Num a => Lista a -> a
producto = foldr (*) 1

-- Definimos la función "máximo" que calcula el máximo elemento de una lista.
máximo :: Ord a => Lista a -> a
máximo = foldr max minBound

-- Definimos la función "mínimo" que calcula el mínimo elemento de una lista.
mínimo :: Ord a => Lista a -> a
mínimo = foldr min maxBound

```

Este código implementa una serie de funciones básicas para trabajar con listas enlazadas en Haskell. Incluye funciones para agregar, quitar, buscar, ordenar, invertir, concatenar, calcular la longitud, mapear, filtrar, reducir, sumar, multiplicar, encontrar el máximo y el mínimo.

El código está escrito en un estilo funcional puro, utilizando recursión y composición de funciones. Las funciones son concisas y fáciles de entender, lo que hace que el código sea fácil de mantener y modificar.

El código está bien documentado, con comentarios que explican cada función y su propósito. Esto hace que el código sea más fácil de entender y usar para otros programadores.