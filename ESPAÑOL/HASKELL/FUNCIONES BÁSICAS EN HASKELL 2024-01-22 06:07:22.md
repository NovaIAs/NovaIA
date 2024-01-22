```haskell
-- Definición de tipos

-- Una persona tiene un nombre y una edad.
data Persona = Persona { nombre :: String, edad :: Int }

-- Un árbol binario tiene un valor y dos subárboles.
data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a)

-- Definición de funciones

-- La función 'suma' suma los dos primeros números de una lista.
suma :: [Int] -> Int
suma (x:y:_) = x + y

-- La función 'producto' multiplica los dos primeros números de una lista.
producto :: [Int] -> Int
producto (x:y:_) = x * y

-- La función 'factorial' calcula el factorial de un número.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- La función 'al cuadrado' eleva al cuadrado un número.
alCuadrado :: Int -> Int
alCuadrado n = n * n

-- La función 'mayor' devuelve el mayor número de una lista.
mayor :: [Int] -> Int
mayor [] = error "La lista está vacía."
mayor (x:xs) = foldr max x xs

-- La función 'menor' devuelve el menor número de una lista.
menor :: [Int] -> Int
menor [] = error "La lista está vacía."
menor (x:xs) = foldr min x xs

-- La función 'ordenar' ordena una lista de números en orden creciente.
ordenar :: [Int] -> [Int]
ordenar xs = foldr insert [] xs

-- La función 'insert' inserta un número en una lista ordenada.
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x:y:ys
  | otherwise = y:insert x ys

-- La función 'buscar' busca un número en una lista ordenada.
buscar :: Int -> [Int] -> Bool
buscar x [] = False
buscar x (y:ys)
  | x == y    = True
  | x < y     = False
  | otherwise = buscar x ys

-- La función 'altura' calcula la altura de un árbol binario.
altura :: Arbol a -> Int
altura Hoja = 0
altura (Nodo _ izq der) = 1 + max (altura izq) (altura der)

-- La función 'contarHojas' cuenta el número de hojas de un árbol binario.
contarHojas :: Arbol a -> Int
contarHojas Hoja = 1
contarHojas (Nodo _ izq der) = contarHojas izq + contarHojas der

-- La función 'imprimirArbol' imprime un árbol binario en una cadena de texto.
imprimirArbol :: Arbol a -> String
imprimirArbol Hoja = "()"
imprimirArbol (Nodo x izq der) = "(" ++ x ++ ", " ++ imprimirArbol izq ++ ", " ++ imprimirArbol der ++ ")"

-- Ejemplos de uso

-- Crear una lista de personas.
personas :: [Persona]
personas = [Persona "Juan" 20, Persona "María" 25, Persona "Pedro" 30]

-- Obtener el nombre de la primera persona de la lista.
nombrePrimeraPersona :: String
nombrePrimeraPersona = nombre $ head personas

-- Obtener la edad de la segunda persona de la lista.
edadSegundaPersona :: Int
edadSegundaPersona = edad $ snd personas

-- Sumar los dos primeros números de la lista [1, 2, 3, 4, 5].
sumaDosPrimerosNumeros :: Int
sumaDosPrimerosNumeros = suma [1, 2, 3, 4, 5]

-- Multiplicar los dos primeros números de la lista [1, 2, 3, 4, 5].
productoDosPrimerosNumeros :: Int
productoDosPrimerosNumeros = producto [1, 2, 3, 4, 5]

-- Calcular el factorial de 5.
factorialDe5 :: Int
factorialDe5 = factorial 5

-- Elevar al cuadrado el número 3.
alCuadrado3 :: Int
alCuadrado3 = alCuadrado 3

-- Obtener el mayor número de la lista [1, 2, 3, 4, 5].
mayorNumero :: Int
mayorNumero = mayor [1, 2, 3, 4, 5]

-- Obtener el menor número de la lista [1, 2, 3, 4, 5].
menorNumero :: Int
menorNumero = menor [1, 2, 3, 4, 5]

-- Ordenar la lista [5, 2, 1, 3, 4].
listaOrdenada :: [Int]
listaOrdenada = ordenar [5, 2, 1, 3, 4]

-- Buscar el número 3 en la lista [1, 2, 3, 4, 5].
numero3EnLista :: Bool
numero3EnLista = buscar 3 [1, 2, 3, 4, 5]

-- Crear un árbol binario.