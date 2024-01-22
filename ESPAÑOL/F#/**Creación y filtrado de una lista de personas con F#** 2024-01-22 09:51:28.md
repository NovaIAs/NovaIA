```f#

// Definir un tipo de dato personalizado:
type Persona = { Nombre: string; Apellido: string; Edad: int }

// Crear una función para crear una nueva persona:
let crearPersona nombre apellido edad =
    { Nombre = nombre; Apellido = apellido; Edad = edad }

// Crear una lista de personas:
let personas =
    [
        crearPersona "Juan" "Pérez" 25;
        crearPersona "María" "Gómez" 30;
        crearPersona "Pedro" "Rodríguez" 35;
    ]

// Iterar sobre la lista de personas y mostrar sus nombres:
for persona in personas do
    printfn "%s %s" persona.Nombre persona.Apellido

// Definir una función para filtrar la lista de personas por edad:
let filtrarPorEdad edadMinima edadMaxima =
    fun persona ->
        persona.Edad >= edadMinima && persona.Edad <= edadMaxima

// Filtrar la lista de personas por edad:
let personasFiltradas =
    personas
    |> List.filter (filtrarPorEdad 25 35)

// Mostrar los nombres de las personas filtradas:
for persona in personasFiltradas do
    printfn "%s %s" persona.Nombre persona.Apellido

```

Explicación del código:

* El tipo de dato personalizado `Persona` se define con tres campos: `Nombre`, `Apellido` y `Edad`.
* La función `crearPersona` se utiliza para crear una nueva persona.
* La lista de personas `personas` se crea utilizando la función `crearPersona` para crear tres personas ficticias.
* El bucle `for` itera sobre la lista de personas y muestra sus nombres.
* La función `filtrarPorEdad` se define para filtrar la lista de personas por edad.
* La lista de personas filtradas `personasFiltradas` se crea utilizando la función `List.filter` para filtrar la lista de personas con la función `filtrarPorEdad`.
* El bucle `for` itera sobre la lista de personas filtradas y muestra sus nombres.