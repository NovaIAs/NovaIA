```haskell
-- Módulo para el manejo de listas
module Listas where

-- Función para crear una lista
crearLista :: [a] -> [a]
crearLista xs = xs

-- Función para agregar un elemento a una lista
agregarElemento :: a -> [a] -> [a]
agregarElemento x xs = x:xs

-- Función para eliminar un elemento de una lista
eliminarElemento :: a -> [a] -> [a]
eliminarElemento x xs = filter (/=x) xs

-- Función para obtener el primer elemento de una lista
primerElemento :: [a] -> a
primerElemento (x:_) = x

-- Función para obtener el último elemento de una lista
ultimoElemento :: [a] -> a
ultimoElemento [] = error "La lista está vacía"
ultimoElemento (x:xs) = if null xs then x else ultimoElemento xs

-- Función para obtener el largo de una lista
largoLista :: [a] -> Int
largoLista [] = 0
largoLista (x:xs) = 1 + largoLista xs

-- Función para invertir una lista
invertirLista :: [a] -> [a]
invertirLista [] = []
invertirLista (x:xs) = invertirLista xs ++ [x]

-- Función para concatenar dos listas
concatenarListas :: [a] -> [a] -> [a]
concatenarListas [] y = y
concatenarListas (x:xs) y = x:concatenarListas xs y

-- Función para ordenar una lista
ordenarLista :: Ord a => [a] -> [a]
ordenarLista [] = []
ordenarLista (x:xs) = insertar x (ordenarLista xs)

insertar :: Ord a => a -> [a] -> [a]
insertar x [] = [x]
insertar x (y:ys)
  | x <= y    = x:y:ys
  | otherwise = y:insertar x ys
```