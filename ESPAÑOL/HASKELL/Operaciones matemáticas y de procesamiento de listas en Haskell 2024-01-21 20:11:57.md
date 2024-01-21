```haskell
import Data.List (sortBy)
import Data.Char (toUpper)

-- Función que recibe un número y devuelve su factorial.
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Función que recibe una lista de números y devuelve su suma.
suma :: [Integer] -> Integer
suma = sum

-- Función que recibe una lista de números y devuelve su promedio.
promedio :: [Integer] -> Double
promedio = (\ns -> sum ns / fromIntegral (length ns))

-- Función que recibe una lista de números y devuelve su mediana.
mediana :: [Integer] -> Double
mediana ns = let nsSorted = sortBy compare ns
                len = length nsSorted
            in if len `mod` 2 == 0
               then (fromIntegral (nsSorted !! (len `div` 2 - 1)) + fromIntegral (nsSorted !! (len `div` 2))) / 2
               else fromIntegral (nsSorted !! (len `div` 2))

-- Función que recibe una lista de números y devuelve su desviación estándar.
desviacionEstandar :: [Integer] -> Double
desviacionEstandar ns = let avg = promedio ns
                            nsDif = map (\n -> (n - avg) ^ 2) ns
                        in sqrt (suma nsDif / fromIntegral (length ns))

-- Función que recibe una lista de cadenas y devuelve una lista de cadenas en mayúsculas.
aMayusculas :: [String] -> [String]
aMayusculas = map toUpper

-- Función que recibe una lista de cadenas y devuelve una lista de tuplas con la cadena y su longitud.
cadenasConLongitud :: [String] -> [(String, Integer)]
cadenasConLongitud = map (\s -> (s, length s))

-- Función que recibe una lista de cadenas y devuelve un diccionario con las cadenas como claves y sus longitudes como valores.
diccionarioCadenas :: [String] -> IO ()
diccionarioCadenas = do
    let ns = [1, 2, 3, 4, 5]
        ss = ["hola", "mundo", "cruel"]
    putStrLn "Factorial de 5:"
    print (factorial 5)
    putStrLn "Suma de [1, 2, 3, 4, 5]:"
    print (suma ns)
    putStrLn "Promedio de [1, 2, 3, 4, 5]:"
    print (promedio ns)
    putStrLn "Mediana de [1, 2, 3, 4, 5]:"
    print (mediana ns)
    putStrLn "Desviación estándar de [1, 2, 3, 4, 5]:"
    print (desviacionEstandar ns)
    putStrLn "Cadenas en mayúsculas:"
    print (aMayusculas ss)
    putStrLn "Cadenas con longitud:"
    print (cadenasConLongitud ss)
    putStrLn "Diccionario de cadenas:"
    print (diccionarioCadenas ss)
```

Este código implementa varias funciones matemáticas y de procesamiento de listas en Haskell. Las funciones incluyen:

* `factorial`: Calcula el factorial de un número.
* `suma`: Calcula la suma de una lista de números.
* `promedio`: Calcula el promedio de una lista de números.
* `mediana`: Calcula la mediana de una lista de números.
* `desviacionEstandar`: Calcula la desviación estándar de una lista de números.
* `aMayusculas`: Convierte una lista de cadenas a mayúsculas.
* `cadenasConLongitud`: Genera una lista de tuplas con cadenas y sus longitudes.
* `diccionarioCadenas`: Genera un diccionario con cadenas como claves y sus longitudes como valores.

El código también incluye una función principal que llama a todas las demás funciones y muestra los resultados en la consola.