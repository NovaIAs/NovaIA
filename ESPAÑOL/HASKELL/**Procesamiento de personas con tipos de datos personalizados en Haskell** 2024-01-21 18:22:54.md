```haskell
-- Definición de tipos de datos personalizados

-- Tipo de dato para representar una persona
data Persona = Persona {
    nombre :: String,
    edad :: Int,
    peso :: Float
}

-- Tipo de dato para representar una lista de personas
type ListaPersonas = [Persona]

-- Definición de funciones

-- Función para crear una nueva persona
nuevaPersona :: String -> Int -> Float -> Persona
nuevaPersona nombre edad peso = Persona {
    nombre = nombre,
    edad = edad,
    peso = peso
}

-- Función para obtener el nombre de una persona
nombrePersona :: Persona -> String
nombrePersona persona = nombre persona

-- Función para obtener la edad de una persona
edadPersona :: Persona -> Int
edadPersona persona = edad persona

-- Función para obtener el peso de una persona
pesoPersona :: Persona -> Float
pesoPersona persona = peso persona

-- Función para crear una lista de personas
nuevaListaPersonas :: [Persona] -> ListaPersonas
nuevaListaPersonas personas = personas

-- Función para obtener el tamaño de una lista de personas
tamanioListaPersonas :: ListaPersonas -> Int
tamanioListaPersonas personas = length personas

-- Función para añadir una persona a una lista de personas
agregarPersona :: Persona -> ListaPersonas -> ListaPersonas
agregarPersona persona personas = persona:personas

-- Función para eliminar una persona de una lista de personas
eliminarPersona :: Persona -> ListaPersonas -> ListaPersonas
eliminarPersona persona personas = filter (\p -> p /= persona) personas

-- Función para ordenar una lista de personas por edad
ordenarPersonasPorEdad :: ListaPersonas -> ListaPersonas
ordenarPersonasPorEdad personas = sortBy (\p1 p2 -> edadPersona p1 - edadPersona p2) personas

-- Función principal

main :: IO ()
main = do
    -- Crear una lista de personas
    personas <- nuevaListaPersonas [
        nuevaPersona "Juan" 20 70.5,
        nuevaPersona "María" 25 65.2,
        nuevaPersona "Pedro" 30 80.1
    ]

    -- Mostrar la lista de personas
    print personas

    -- Obtener el tamaño de la lista de personas
    tamanio <- tamanioListaPersonas personas

    -- Mostrar el tamaño de la lista de personas
    print tamanio

    -- Añadir una persona a la lista de personas
    personas <- agregarPersona (nuevaPersona "Ana" 28 62.3) personas

    -- Mostrar la lista de personas
    print personas

    -- Eliminar una persona de la lista de personas
    personas <- eliminarPersona (nuevaPersona "Juan" 20 70.5) personas

    -- Mostrar la lista de personas
    print personas

    -- Ordenar la lista de personas por edad
    personas <- ordenarPersonasPorEdad personas

    -- Mostrar la lista de personas
    print personas
```

Explicación del código:

* El código define dos tipos de datos personalizados: `Persona` y `ListaPersonas`. El tipo de datos `Persona` representa una persona con su nombre, edad y peso. El tipo de datos `ListaPersonas` representa una lista de personas.
* El código define varias funciones que trabajan con estos tipos de datos. Por ejemplo, la función `nuevaPersona` crea una nueva persona, la función `nombrePersona` obtiene el nombre de una persona, la función `edadPersona` obtiene la edad de una persona, la función `pesoPersona` obtiene el peso de una persona, la función `nuevaListaPersonas` crea una nueva lista de personas, la función `tamanioListaPersonas` obtiene el tamaño de una lista de personas, la función `agregarPersona` añade una persona a una lista de personas, la función `eliminarPersona` elimina una persona de una lista de personas, y la función `ordenarPersonasPorEdad` ordena una lista de personas por edad.
* La función `main` es la función principal del programa. Esta función crea una lista de personas, muestra la lista de personas, obtiene el tamaño de la lista de personas, añade una persona a la lista de personas, elimina una persona de la lista de personas, y ordena la lista de personas por edad. Finalmente, muestra la lista de personas ordenada.