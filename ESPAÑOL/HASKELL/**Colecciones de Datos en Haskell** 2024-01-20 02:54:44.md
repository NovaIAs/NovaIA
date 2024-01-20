```haskell
-- Módulo principal del programa.

module Main where

-- Importamos la biblioteca estándar de Haskell.
import Prelude

-- Definimos el tipo 'Lista'.
data Lista a = Vacía | Cons a (Lista a) deriving (Show, Eq)

-- Definimos la función 'longitud'.
longitud :: Lista a -> Int
longitud Vacía = 0
longitud (Cons _ xs) = 1 + longitud xs

-- Definimos la función 'invertir'.
invertir :: Lista a -> Lista a
invertir Vacía = Vacía
invertir (Cons x xs) = invertir xs `Cons` x

-- Definimos la función 'concatenar'.
concatenar :: Lista a -> Lista a -> Lista a
concatenar Vacía ys = ys
concatenar (Cons x xs) ys = Cons x (concatenar xs ys)

-- Definimos la función 'filtrar'.
filtrar :: (a -> Bool) -> Lista a -> Lista a
filtrar _ Vacía = Vacía
filtrar f (Cons x xs) =
  if f x then Cons x (filtrar f xs) else filtrar f xs

-- Definimos la función 'mapear'.
mapear :: (a -> b) -> Lista a -> Lista b
mapear _ Vacía = Vacía
mapear f (Cons x xs) = Cons (f x) (mapear f xs)

-- Definimos la función 'doblar'.
doblar :: Lista a -> Lista a
doblar Vacía = Vacía
doblar (Cons x xs) = Cons x (Cons x (doblar xs))

-- Definimos la función 'partir'.
partir :: Int -> Lista a -> (Lista a, Lista a)
partir n Vacía = (Vacía, Vacía)
partir n (Cons x xs) =
  if n == 1 then (Cons x Vacía, xs) else let (ys, zs) = partir (n - 1) xs in (Cons x ys, zs)

-- Definimos la función 'ordenar'.
ordenar :: Lista a -> Lista a
ordenar Vacía = Vacía
ordenar (Cons x xs) = insertar x (ordenar xs)

-- Definimos la función 'insertar'.
insertar :: a -> Lista a -> Lista a
insertar x Vacía = Cons x Vacía
insertar x (Cons y ys) =
  if x <= y then Cons x (Cons y ys) else Cons y (insertar x ys)

-- Definimos la función 'buscar'.
buscar :: Eq a => a -> Lista a -> Bool
buscar _ Vacía = False
buscar x (Cons y ys) =
  if x == y then True else buscar x ys

-- Definimos la función 'eliminar'.
eliminar :: Eq a => a -> Lista a -> Lista a
eliminar x Vacía = Vacía
eliminar x (Cons y ys) =
  if x == y then ys else Cons y (eliminar x ys)

-- Definimos la función 'unir'.
unir :: Eq a => Lista a -> Lista a -> Lista a
unir xs ys = ordenar (concatenar xs ys)

-- Definimos la función 'intersecar'.
intersecar :: Eq a => Lista a -> Lista a ->