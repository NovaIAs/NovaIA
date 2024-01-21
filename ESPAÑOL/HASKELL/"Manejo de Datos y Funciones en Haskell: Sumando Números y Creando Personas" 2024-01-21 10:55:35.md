```haskell
-- Módulo principal
module MiModulo where

-- Función principal
main :: IO ()
main = do
  -- Crear una lista de números del 1 al 10
  let numeros = [1..10]

  -- Aplicar la función "sumar" a cada elemento de la lista
  let suma = sum numeros

  -- Imprimir el resultado de la suma
  putStrLn ("La suma de los números del 1 al 10 es: " ++ show suma)

-- Función para sumar dos números
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Data type para representar una persona
data Persona = Persona
  { nombre :: String
  , edad :: Int
  , estatura :: Float
  }

-- Función para crear una nueva persona
crearPersona :: String -> Int -> Float -> Persona
crearPersona nombre edad estatura = Persona {nombre = nombre, edad = edad, estatura = estatura}

-- Función para imprimir el nombre de una persona
imprimirNombre :: Persona -> IO ()
imprimirNombre (Persona nombre _ _) = putStrLn nombre

-- Función para imprimir la edad de una persona
imprimirEdad :: Persona -> IO ()
imprimirEdad (Persona _ edad _) = putStrLn (show edad)

-- Función para imprimir la estatura de una persona
imprimirEstatura :: Persona -> IO ()
imprimirEstatura (Persona _ _ estatura) = putStrLn (show estatura)
```

Explicación:

1. El código está escrito en Haskell, un lenguaje de programación funcional.
2. El módulo principal es `MiModulo`, que contiene la función principal `main`.
3. La función `main` crea una lista de números del 1 al 10, aplica la función `sum` a cada elemento de la lista y imprime el resultado de la suma.
4. La función `sumar` suma dos números.
5. El data type `Persona` representa a una persona con los siguientes campos: `nombre`, `edad` y `estatura`.
6. La función `crearPersona` crea una nueva persona con los valores proporcionados.
7. Las funciones `imprimirNombre`, `imprimirEdad` e `imprimirEstatura` imprimen el nombre, la edad y la estatura de una persona, respectivamente.