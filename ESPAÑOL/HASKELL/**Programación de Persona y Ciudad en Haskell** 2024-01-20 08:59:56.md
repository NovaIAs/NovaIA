```haskell
-- Definición de tipos

data Persona = Persona { nombre :: String
                      , edad  :: Int
                      , dni   :: String
                      } deriving (Show)

data Ciudad = Ciudad { nombre :: String
                    , provincia :: String
                    , poblacion :: Int
                    } deriving (Show)

-- Funciones

-- Crear una nueva persona
nuevaPersona :: String -> Int -> String -> Persona
nuevaPersona nombre edad dni = Persona nombre edad dni

-- Crear una nueva ciudad
nuevaCiudad :: String -> String -> Int -> Ciudad
nuevaCiudad nombre provincia poblacion = Ciudad nombre provincia poblacion

-- Obtener el nombre de una persona
nombrePersona :: Persona -> String
nombrePersona (Persona nombre _ _) = nombre

-- Obtener la edad de una persona
edadPersona :: Persona -> Int
edadPersona (Persona _ edad _) = edad

-- Obtener el DNI de una persona
dniPersona :: Persona -> String
dniPersona (Persona _ _ dni) = dni

-- Obtener el nombre de una ciudad
nombreCiudad :: Ciudad -> String
nombreCiudad (Ciudad nombre _ _) = nombre

-- Obtener la provincia de una ciudad
provinciaCiudad :: Ciudad -> String
provinciaCiudad (Ciudad _ provincia _) = provincia

-- Obtener la población de una ciudad
poblacionCiudad :: Ciudad -> Int
poblacionCiudad (Ciudad _ _ poblacion) = poblacion

-- Imprimir el nombre de una persona
imprimirNombrePersona :: Persona -> IO ()
imprimirNombrePersona (Persona nombre _ _) = putStrLn nombre

-- Imprimir la edad de una persona
imprimirEdadPersona :: Persona -> IO ()
imprimirEdadPersona (Persona _ edad _) = putStrLn (show edad)

-- Imprimir el DNI de una persona
imprimirDniPersona :: Persona -> IO ()
imprimirDniPersona (Persona _ _ dni) = putStrLn dni

-- Imprimir el nombre de una ciudad
imprimirNombreCiudad :: Ciudad -> IO ()
imprimirNombreCiudad (Ciudad nombre _ _) = putStrLn nombre

-- Imprimir la provincia de una ciudad
imprimirProvinciaCiudad :: Ciudad -> IO ()
imprimirProvinciaCiudad (Ciudad _ provincia _) = putStrLn provincia

-- Imprimir la población de una ciudad
imprimirPoblacionCiudad :: Ciudad -> IO ()
imprimirPoblacionCiudad (Ciudad _ _ poblacion) = putStrLn (show poblacion)

-- Programa principal

main :: IO ()
main = do
  -- Crear una nueva persona
  persona1 <- nuevaPersona "Juan" 30 "123456789"

  -- Crear una nueva ciudad
  ciudad1 <- nuevaCiudad "Madrid" "Madrid" 3000000

  -- Imprimir el nombre de la persona
  imprimirNombrePersona persona1

  -- Imprimir la edad de la persona
  imprimirEdadPersona persona1

  -- Imprimir el DNI de la persona
  imprimirDniPersona persona1

  -- Imprimir el nombre de la ciudad
  imprimirNombreCiudad ciudad1

  -- Imprimir la provincia de la ciudad
  imprimirProvinciaCiudad ciudad1

  -- Imprimir la población de la ciudad
  imprimirPoblacionCiudad ciudad1
```

Este código es más complejo que el anterior porque define nuevos tipos de datos (Persona y Ciudad) y funciones para trabajar con ellos. También define un programa principal que crea una nueva persona y una nueva ciudad y luego imprime sus datos.