```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- Definimos una función para obtener el máximo común divisor de dos números.
mcd :: Int -> Int -> Int
mcd m n
  | n == 0 = m
  | otherwise = mcd n (m `mod` n)

-- Definimos una función para obtener el mínimo común múltiplo de dos números.
mcm :: Int -> Int -> Int
mcm m n = m * n `div` mcd m n

-- Definimos una función para obtener la lista de todos los divisores de un número.
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- Definimos una función para obtener la lista de los divisores propios de un número.
divisoresPropios :: Int -> [Int]
divisoresPropios n = [x | x <- divisores n, x < n]

-- Definimos una función para obtener la suma de los divisores propios de un número.
sumaDivisoresPropios :: Int -> Int
sumaDivisoresPropios n = sum (divisoresPropios n)

-- Definimos un tipo de datos para representar un número perfecto.
data NumeroPerfecto = NumeroPerfecto Int

-- Definimos una instancia de la clase Show para poder imprimir los números perfectos.
instance Show NumeroPerfecto where
  show (NumeroPerfecto n) = "El número perfecto es " ++ show n

-- Definimos una función para encontrar todos los números perfectos menores que un número dado.
numerosPerfectos :: Int -> [NumeroPerfecto]
numerosPerfectos n =
  [NumeroPerfecto p | p <- [1..n], sumaDivisoresPropios p == p]

-- Ordenamos la lista de números perfectos por orden ascendente.
numerosPerfectosOrdenados :: Int -> [NumeroPerfecto]
numerosPerfectosOrdenados n = sortBy (comparing (\(NumeroPerfecto p) -> p)) (numerosPerfectos n)

-- Imprimimos los primeros 10 números perfectos.
main :: IO ()
main =
  do
    putStrLn "Los primeros 10 números perfectos son:"
    mapM_ print (take 10 (numerosPerfectosOrdenados 10000))
```

Explicación:

1. El código define una serie de funciones para trabajar con números perfectos.
2. Primero, define una función para obtener el máximo común divisor (mcd) de dos números. El mcd es el mayor número entero que divide a ambos números sin dejar resto.
3. A continuación, define una función para obtener el mínimo común múltiplo (mcm) de dos números. El mcm es el menor número entero que es divisible por ambos números.
4. También define una función para obtener la lista de todos los divisores de un número.
5. Luego, define una función para obtener la lista de los divisores propios de un número. Los divisores propios de un número son todos los divisores del número, excepto el propio número.
6. A continuación, define una función para obtener la suma de los divisores propios de un número.
7. Luego, define un tipo de datos para representar un número perfecto. Un número perfecto es un número cuya suma de divisores propios es igual al propio número.
8. A continuación, define una instancia de la clase Show para poder imprimir los números perfectos.
9. Luego, define una función para encontrar todos los números perfectos menores que un número dado.
10. A continuación, ordena la lista de números perfectos por orden ascendente.
11. Por último, imprime los primeros 10 números perfectos.