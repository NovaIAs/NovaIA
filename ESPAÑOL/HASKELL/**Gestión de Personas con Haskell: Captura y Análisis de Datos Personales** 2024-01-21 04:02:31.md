```haskell
-- Definimos el tipo de dato Persona con sus campos correspondientes.
data Persona = Persona
    { nombre :: String
    , apellido :: String
    , edad :: Int
    , genero :: Char
    } deriving (Eq, Ord, Show)

-- Definimos la función principal `main`.
main :: IO ()
main = do
    -- Creamos una lista de personas.
    personas <- leerPersonas

    -- Imprimimos la lista de personas.
    putStrLn "Lista de personas:"
    mapM_ print personas

    -- Calculamos la edad promedio de las personas.
    edadPromedio <- calcularEdadPromedio personas

    -- Imprimimos la edad promedio.
    putStrLn ("Edad promedio: " ++ show edadPromedio)

-- Definimos la función `leerPersonas`.
leerPersonas :: IO [Persona]
leerPersonas = do
    -- Pedimos al usuario que ingrese la cantidad de personas.
    putStrLn "Ingrese la cantidad de personas:"
    cantidad <- readLn

    -- Creamos una lista vacía para almacenar las personas.
    personas <- replicateM cantidad (crearPersona)

    -- Devolvemos la lista de personas.
    return personas

-- Definimos la función `crearPersona`.
crearPersona :: IO Persona
crearPersona = do
    -- Pedimos al usuario que ingrese el nombre de la persona.
    putStrLn "Ingrese el nombre de la persona:"
    nombre <- getLine

    -- Pedimos al usuario que ingrese el apellido de la persona.
    putStrLn "Ingrese el apellido de la persona:"
    apellido <- getLine

    -- Pedimos al usuario que ingrese la edad de la persona.
    putStrLn "Ingrese la edad de la persona:"
    edad <- readLn

    -- Pedimos al usuario que ingrese el género de la persona.
    putStrLn "Ingrese el género de la persona (M/F):"
    genero <- getLine

    -- Creamos una nueva persona con los datos ingresados por el usuario.
    let persona = Persona nombre apellido edad genero

    -- Devolvemos la persona.
    return persona

-- Definimos la función `calcularEdadPromedio`.
calcularEdadPromedio :: [Persona] -> IO Double
calcularEdadPromedio personas = do
    -- Obtenemos la suma de las edades de las personas.
    sumaEdades <- foldM (\acc persona -> return (acc + edad persona)) 0 personas

    -- Obtenemos la cantidad de personas.
    cantidad <- return (length personas)

    -- Calculamos la edad promedio.
    let edadPromedio = sumaEdades / fromIntegral cantidad

    -- Devolvemos la edad promedio.
    return edadPromedio
```

Este código pide al usuario que ingrese una lista de personas, con sus nombres, apellidos, edades y géneros. A continuación, calcula la edad promedio de las personas y la imprime en la consola.

El código está estructurado de la siguiente manera:

* La función `main` es la función principal del programa.
* La función `leerPersonas` pide al usuario que ingrese una lista de personas.
* La función `crearPersona` pide al usuario que ingrese los datos de una persona.
* La función `calcularEdadPromedio` calcula la edad promedio de una lista de personas.

El código utiliza las siguientes características de Haskell:

* **Tipos de datos:** Los tipos de datos se utilizan para definir los tipos de los datos que se pueden almacenar en una variable.
* **Funciones:** Las funciones se utilizan para definir operaciones que se pueden realizar sobre los datos.
* **Expresiones lambda:** Las expresiones lambda son una forma compacta de definir funciones.
* **Comprensiones de listas:** Las comprensiones de listas son una forma compacta de definir listas.
* **Funciones de orden superior:** Las funciones de orden superior son funciones que toman otras funciones como argumentos o que devuelven otras funciones.
* **Monadas:** Las monadas son una forma de organizar el código para manejar efectos secundarios.

Este código es un ejemplo de cómo Haskell se puede utilizar para escribir programas complejos y expresivos.