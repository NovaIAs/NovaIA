```haskell

-- Definición del tipo de datos Lista
data Lista a = Vacía | Cons a (Lista a)

-- `longitud` calcula la longitud de una lista
longitud :: Lista a -> Int
longitud Vacía = 0
longitud (Cons _ l) = 1 + longitud l

-- `reverse` invierte una lista
reverse :: Lista a -> Lista a
reverse Vacía = Vacía
reverse (Cons x xs) = (reverse xs) `Cons` x

-- `maximum` devuelve el elemento máximo de una lista
maximum :: Ord a => Lista a -> a
maximum Vacía = error "La lista está vacía"
maximum (Cons x Vacía) = x
maximum (Cons x xs) = if x > (maximum xs) then x else (maximum xs)

-- `map` aplica una función a cada elemento de una lista
map :: (a -> b) -> Lista a -> Lista b
map _ Vacía = Vacía
map f (Cons x xs) = Cons (f x) (map f xs)

-- `filter` filtra los elementos de una lista que cumplen una condición
filter :: (a -> Bool) -> Lista a -> Lista a
filter _ Vacía = Vacía
filter f (Cons x xs) = if f x then Cons x (filter f xs) else filter f xs

-- `fold` aplica una función de acumulación a cada elemento de una lista
fold :: (a -> b -> b) -> b -> Lista a -> b
fold _ acc Vacía = acc
fold f acc (Cons x xs) = fold f (f acc x) xs

-- `escalar` multiplica cada elemento de una lista por un número
escalar :: Num a => a -> Lista a -> Lista a
escalar n Vacía = Vacía
escalar n (Cons x xs) = Cons (n * x) (escalar n xs)

-- `sumarListas` suma los elementos de dos listas
sumarListas :: Num a => Lista a -> Lista a -> Lista a
sumarListas Vacía Vacía = Vacía
sumarListas (Cons x xs) Vacía = Cons x (sumarListas xs Vacía)
sumarListas Vacía (Cons x xs) = Cons x (sumarListas Vacía xs)
sumarListas (Cons x xs) (Cons y ys) = Cons (x + y) (sumarListas xs ys)

-- `productoEscalar` calcula el producto escalar de dos listas
productoEscalar :: Num a => Lista a -> Lista a -> a
productoEscalar Vacía Vacía = 0
productoEscalar (Cons x xs) Vacía = 0
productoEscalar Vacía (Cons x xs) = 0
productoEscalar (Cons x xs) (Cons y ys) = x * y + (productoEscalar xs ys)

-- `potencia` calcula la potencia de un número entero no negativo
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia n e = n * (potencia n (e - 1))

-- `factorial` calcula el factorial de un número entero no negativo
factorial :: Int -> Int
factorial n = fold (*) 1 [1..n]

-- `esPrimo` comprueba si un número entero es primo
esPrimo :: Int -> Bool
esPrimo n = all (\f -> n `mod` f /= 0) [2..(n `div` 2)]

-- `primosHastaN` devuelve una lista con los números primos hasta un límite
primosHastaN :: Int -> Lista Int
primosHastaN n = filter esPrimo [2..n]

```