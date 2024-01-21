```haskell
-- Definiciones de tipo
data Árbol a = Hoja a | Nodo (Árbol a) a (Árbol a)

-- Funciones de árbol
altura :: Árbol a -> Int
altura (Hoja _) = 0
altura (Nodo izquierdo _ derecho) = 1 + max (altura izquierdo) (altura derecho)

esHoja :: Árbol a -> Bool
esHoja (Hoja _) = True
esHoja (Nodo _ _ _) = False

-- Funciones de lista
longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- Funciones de cadenas
longitudCadena :: String -> Int
longitudCadena = longitud

subcadena :: String -> Int -> Int -> String
subcadena xs inicio fin = take (fin - inicio + 1) (drop inicio xs)

-- Funciones matemáticas
suma :: Num a => a -> a -> a
suma x y = x + y

resta :: Num a => a -> a -> a
resta x y = x - y

multiplicación :: Num a => a -> a -> a
multiplicación x y = x * y

división :: Fractional a => a -> a -> a
división x y = x / y

-- Función principal
main :: IO ()
main = do
    putStrLn "Hola, mundo!"

    -- Ejemplo de árbol
    let árbol = Nodo (Nodo (Hoja 1) 2 (Hoja 3)) 4 (Nodo (Hoja 5) 6 (Hoja 7))

    -- Imprimir la altura del árbol
    putStrLn ("Altura del árbol: " ++ show (altura árbol))

    -- Imprimir si el árbol es una hoja
    putStrLn ("¿Es el árbol una hoja?: " ++ show (esHoja árbol))

    -- Ejemplo de lista
    let lista = [1, 2, 3, 4, 5]

    -- Imprimir la longitud de la lista
    putStrLn ("Longitud de la lista: " ++ show (longitud lista))

    -- Imprimir la lista mapeada por el cuadrado de cada elemento
    putStrLn ("Lista mapeada por el cuadrado de cada elemento: " ++ show (map cuadrado lista))

    -- Imprimir la lista filtrada por los elementos pares
    putStrLn ("Lista filtrada por los elementos pares: " ++ show (filter par lista))

    -- Ejemplo de cadena
    let cadena = "Hola, mundo!"

    -- Imprimir la longitud de la cadena
    putStrLn ("Longitud de la cadena: " ++ show (longitudCadena cadena))

    -- Imprimir la subcadena desde el índice 0 hasta el índice 4
    putStrLn ("Subcadena desde el índice 0 hasta el índice 4: " ++ subcadena cadena 0 4)

    -- Ejemplo de operaciones matemáticas
    let x = 10
    let y = 5

    -- Imprimir la suma de x e y
    putStrLn ("Suma de x e y: " ++ show (suma x y))

    -- Imprimir la resta de x e y
    putStrLn ("Resta de x e y: " ++ show (resta x y))

    -- Imprimir la multiplicación de x e y
    putStrLn ("Multiplicación de x e y: " ++ show (multiplicación x y))

    -- Imprimir la división de x e y
    putStrLn ("División de x e y: " ++ show (división x y))

    -- Funciones auxiliares
cuadrado :: Num a => a -> a
cuadrado x = x * x

par :: Num a => a -> Bool
par x = x `mod` 2 == 0
```

Este código es más complejo que los ejemplos anteriores porque utiliza varias funciones definidas por el usuario y definiciones de tipos. Aquí hay una explicación breve de cada parte del código:

* Definiciones de tipo:

    * `Árbol a`: Define un tipo de datos para representar árboles binarios. Un árbol binario puede ser una hoja, que contiene un solo valor, o un nodo, que contiene un árbol izquierdo, un valor y un árbol derecho.
    * `String`: Este es el tipo de datos para representar cadenas de texto en Haskell.

* Funciones de árbol:

    * `altura`: Calcula la altura de un árbol binario. La altura de un árbol es el número de nodos en el camino más largo desde la raíz hasta una hoja.
    * `esHoja`: Comprueba si un árbol binario es una hoja.

* Funciones de lista:

    * `longitud`: Calcula la longitud de una lista.
    * `map`: Aplica una función a cada elemento de una lista y devuelve una nueva lista con los resultados.
    * `filter`: Filtra una lista para incluir solo los elementos que cumplen una condición.

* Funciones de cadenas:

    * `longitudCadena`: Calcula la longitud de una cadena de texto.
    * `subcadena`: Extrae una subcadena de una cadena de texto.

* Funciones matemáticas:

    * `suma`, `resta`, `multiplicación` y `división`: Estas son funciones para realizar operaciones matemáticas básicas.

* Función principal:

    * `main`: Esta es la función principal del programa. Imprime "Hola, mundo!" en la consola y luego ejecuta algunos ejemplos de las funciones definidas anteriormente.

* Funciones auxiliares:

    * `cuadrado`: Calcula el cuadrado de un número.
    * `par`: Comprueba si un número es par.