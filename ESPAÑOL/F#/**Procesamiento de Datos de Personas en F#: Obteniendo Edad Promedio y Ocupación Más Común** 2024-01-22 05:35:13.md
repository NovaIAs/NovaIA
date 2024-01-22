```
// Definir un tipo de datos personalizado para representar una persona.
type Persona = { nombre: string; edad: int; ocupacion: string }

// Definir una lista de personas.
let personas = [
    { nombre = "Juan"; edad = 20; ocupacion = "Estudiante" },
    { nombre = "María"; edad = 25; ocupacion = "Ingeniera" },
    { nombre = "Pedro"; edad = 30; ocupacion = "Doctor" }
]

// Define una función para obtener las edades de las personas.
let obtenerEdades personas =
    Seq.map (fun persona -> persona.edad) personas

// Definir una función para obtener la edad promedio de las personas.
let getEdadPromedio personas =
    Seq.average (obtenerEdades personas)

// Define una función para obtener las ocupaciones de las personas.
let getOcupaciones personas =
    Seq.map (fun persona -> persona.ocupacion) personas

// Definir una función para obtener la ocupación más común de las personas.
let getOcupacionMasComun personas =
    let ocupaciones = getOcupaciones personas
    ocupaciones
    |> Seq.groupBy id
    |> Seq.map (fun (ocupacion, personas) -> (ocupacion, Seq.length personas))
    |> Seq.maxBy (fun (_, count) -> count)
    |> fst

// Imprimir la edad promedio y la ocupación más común de las personas.
printfn "Edad promedio: %A" (getEdadPromedio personas)
printfn "Ocupación más común: %A" (getOcupacionMasComun personas)
```

Este código define un tipo de datos personalizado para representar una persona, con propiedades para el nombre, la edad y la ocupación. También define una lista de personas y varias funciones para trabajar con esa lista. Las funciones obtienen las edades de las personas, calculan la edad promedio, obtienen las ocupaciones de las personas y encuentran la ocupación más común de las personas. El código imprime la edad promedio y la ocupación más común de las personas.