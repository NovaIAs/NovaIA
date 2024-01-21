```haskell
-- Módulo principal del programa
---------------------------------
module Main where

-- Importamos las bibliotecas necesarias
import Data.List (sort)
import Data.Char (toUpper)

-- Definimos el tipo de dato Persona
data Persona = Persona {
    nombre :: String,
    edad :: Int
} deriving (Show, Eq)

-- Definimos la función compararPersonas
-- que compara dos personas por su edad
compararPersonas :: Persona -> Persona -> Ordering
compararPersonas p1 p2 = compare (edad p1) (edad p2)

-- Definimos la función main
-- que ejecuta el programa
main :: IO ()
main = do
    -- Creamos una lista de personas
    let personas = [
        Persona "Juan" 20,
        Persona "María" 30,
        Persona "Pedro" 40
    ]

    -- Ordenamos la lista de personas
    -- por su edad en orden ascendente
    let personasOrdenadas = sort compararPersonas personas

    -- Imprimimos la lista de personas ordenadas
    mapM_ print personasOrdenadas

    -- Convertimos los nombres de las personas ordenadas
    -- a mayúsculas
    let nombresEnMayusculas = map (map toUpper) (map nombre personasOrdenadas)

    -- Imprimimos la lista de nombres en mayúsculas
    mapM_ print nombresEnMayusculas
```

Explicación del código:

* El código define un tipo de dato `Persona` que tiene dos campos: `nombre` y `edad`.
* Luego define una función `compararPersonas` que compara dos personas por su edad.
* La función `main` crea una lista de personas, la ordena por edad y luego imprime la lista de personas ordenadas.
* Finalmente, la función `main` convierte los nombres de las personas ordenadas a mayúsculas y los imprime.