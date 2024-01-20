```haskell
-- Definición de tipos de datos personalizados

data Persona = Persona { nombre :: String, edad :: Int, origen :: String } deriving (Show)
data Ciudad = Ciudad { nombre :: String, país :: String, población :: Int } deriving (Show)

-- Función para crear una nueva persona
crearPersona :: String -> Int -> String -> Persona
crearPersona nombre edad origen = Persona {nombre = nombre, edad = edad, origen = origen}

-- Función para crear una nueva ciudad
crearCiudad :: String -> String -> Int -> Ciudad
crearCiudad nombre país población = Ciudad {nombre = nombre, país = país, población = población}

-- Función principal
main :: IO ()
main = do
  -- Crear una lista de personas
  personas <- return [
    crearPersona "Juan" 25 "España",
    crearPersona "María" 30 "México",
    crearPersona "Pedro" 40 "Colombia"
  ]

  -- Crear una lista de ciudades
  ciudades <- return [
    crearCiudad "Madrid" "España" 3200000,
    crearCiudad "Ciudad de México" "México" 21000000,
    crearCiudad "Bogotá" "Colombia" 7900000
  ]

  -- Imprimir la lista de personas
  putStrLn "Lista de personas:"
  mapM_ print personas

  -- Imprimir la lista de ciudades
  putStrLn "Lista de ciudades:"
  mapM_ print ciudades

  -- Buscar una persona por su nombre
  nombrePersona <- getLine
  let personaEncontrada = find ((== nombrePersona) . nombre) personas
  if personaEncontrada /= Nothing
    then print personaEncontrada
    else putStrLn "Persona no encontrada"

  -- Buscar una ciudad por su nombre
  nombreCiudad <- getLine
  let ciudadEncontrada = find ((== nombreCiudad) . nombre) ciudades
  if ciudadEncontrada /= Nothing
    then print ciudadEncontrada
    else putStrLn "Ciudad no encontrada"
```

Explicación del código:

* Se definen dos tipos de datos personalizados: `Persona` y `Ciudad`. Cada tipo de datos tiene sus propios campos, que son atributos de ese tipo de datos. Por ejemplo, el tipo de datos `Persona` tiene los campos `nombre`, `edad` y `origen`, mientras que el tipo de datos `Ciudad` tiene los campos `nombre`, `país` y `población`.
* Se definen dos funciones para crear nuevas instancias de los tipos de datos `Persona` y `Ciudad`. Estas funciones reciben los valores de los campos como argumentos y devuelven una nueva instancia del tipo de datos correspondiente.
* Se define una función principal `main` que es el punto de entrada del programa. Esta función crea dos listas, una de personas y otra de ciudades, utilizando las funciones definidas anteriormente.
* Se imprimen las listas de personas y ciudades en la consola.
* Se pide al usuario que introduzca un nombre de persona y se busca esa persona en la lista de personas. Si se encuentra, se imprime en la consola. De lo contrario, se imprime un mensaje indicando que la persona no se ha encontrado.
* Se pide al usuario que introduzca un nombre de ciudad y se busca esa ciudad en la lista de ciudades. Si se encuentra, se imprime en la consola. De lo contrario, se imprime un mensaje indicando que la ciudad no se ha encontrado.