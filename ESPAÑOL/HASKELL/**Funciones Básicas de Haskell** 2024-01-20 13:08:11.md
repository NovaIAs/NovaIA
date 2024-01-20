```haskell
-- Importamos las bibliotecas necesarias
import Prelude
import Data.List
import Data.Maybe
import Data.Function

-- Definimos una función para sumar dos números
suma :: Num a => a -> a -> a
suma a b = a + b

-- Definimos una función para restar dos números
resta :: Num a => a -> a -> a
resta a b = a - b

-- Definimos una función para multiplicar dos números
multiplica :: Num a => a -> a -> a
multiplica a b = a * b

-- Definimos una función para dividir dos números
divide :: Fractional a => a -> a -> Maybe a
divide a b = if b == 0 then Nothing else Just (a / b)

-- Definimos una función para calcular el factorial de un número
factorial :: Integral a => a -> a
factorial n = product [1..n]

-- Definimos una función para calcular el máximo común divisor de dos números
mcd :: Integral a => a -> a -> a
mcd a b = if b == 0 then a else mcd b (a `mod` b)

-- Definimos una función para calcular el mínimo común múltiplo de dos números
mcm :: Integral a => a -> a -> a
mcm a b = a * b `div` mcd a b

-- Definimos una función para comprobar si un número es primo
esPrimo :: Integral a => a -> Bool
esPrimo n = all (\x -> n `mod` x /= 0) [2..n-1]

-- Definimos una función para generar una lista de números primos hasta un determinado número
primosHasta :: Integral a => a -> [a]
primosHasta n = filter esPrimo [2..n]

-- Definimos una función para calcular el enésimo número primo
enesimoPrimo :: Integral a => a -> Maybe a
enesimoPrimo n = nth (n-1) primosHasta

-- Definimos una función para comprobar si una lista está ordenada
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x:y:xs) = x <= y && estaOrdenada (y:xs)

-- Definimos una función para ordenar una lista
ordena :: Ord a => [a] -> [a]
ordena = sort

-- Definimos una función para invertir una lista
invierte :: [a] -> [a]
invierte = reverse

-- Definimos una función para concatenar dos listas
concatena :: [a] -> [a] -> [a]
concatena = (++)

-- Definimos una función para obtener el primer elemento de una lista
primero :: [a] -> Maybe a
primero = headMaybe

-- Definimos una función para obtener el último elemento de una lista
ultimo :: [a] -> Maybe a
ultimo = lastMaybe

-- Definimos una función para obtener el elemento enésimo de una lista
enesimo :: Int -> [a] -> Maybe a
enesimo n = nth (n-1)

-- Definimos una función para eliminar el primer elemento de una lista
eliminaPrimero :: [a] -> [a]
eliminaPrimero = tail

-- Definimos una función para eliminar el último elemento de una lista
eliminaUltimo :: [a] -> [a]
eliminaUltimo = init

-- Definimos una función para eliminar el elemento enésimo de una lista
eliminaEnesimo :: Int -> [a] -> Maybe [a]
eliminaEnesimo n = fmap (deleteAt (n-1))

-- Definimos una función para insertar un elemento en el primer lugar de una lista
insertaPrimero :: a -> [a] -> [a]
insertaPrimero x = (:) x

-- Definimos una función para insertar un elemento en el último lugar de una lista
insertaUltimo :: a -> [a] -> [a]
insertaUltimo x = (++) [x]

-- Definimos una función para insertar un elemento en el enésimo lugar de una lista
inser