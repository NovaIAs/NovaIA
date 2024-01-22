```f#
// Definir un tipo de datos para representar una persona
type Persona = {
    Nombre: string
    Edad: int
    EstadoCivil: string
}

// Definir una función para crear una nueva persona
let crearPersona (nombre: string) (edad: int) (estadoCivil: string) =
    { Nombre = nombre; Edad = edad; EstadoCivil = estadoCivil }

// Definir una lista de personas
let personas = [
    crearPersona "Juan" 25 "Soltero"
    crearPersona "María" 30 "Casada"
    crearPersona "Pedro" 40 "Divorciado"
]

// Definir una función para obtener el nombre de una persona
let obtenerNombre (persona: Persona) = persona.Nombre

// Definir una función para obtener la edad de una persona
let obtenerEdad (persona: Persona) = persona.Edad

// Definir una función para obtener el estado civil de una persona
let obtenerEstadoCivil (persona: Persona) = persona.EstadoCivil

// Definir una función para imprimir los datos de una persona
let imprimirPersona (persona: Persona) =
    printfn "Nombre: %s" (obtenerNombre persona)
    printfn "Edad: %d" (obtenerEdad persona)
    printfn "Estado civil: %s" (obtenerEstadoCivil persona)

// Iterar sobre la lista de personas e imprimir los datos de cada una
for persona in personas do
    imprimirPersona persona
```

Explicación del código:

1. Se define un tipo de datos llamado `Persona` que representa a una persona con los campos `Nombre`, `Edad` y `EstadoCivil`.
2. Se define una función llamada `crearPersona` que toma como parámetros el nombre, la edad y el estado civil de una persona y devuelve una nueva instancia del tipo `Persona`.
3. Se define una lista de personas llamada `personas` que contiene tres instancias del tipo `Persona`.
4. Se definen tres funciones llamadas `obtenerNombre`, `obtenerEdad` y `obtenerEstadoCivil` que toman como parámetro una instancia del tipo `Persona` y devuelven el nombre, la edad y el estado civil de la persona, respectivamente.
5. Se define una función llamada `imprimirPersona` que toma como parámetro una instancia del tipo `Persona` e imprime los datos de la persona en la consola.
6. Se itera sobre la lista de personas e se imprime los datos de cada persona en la consola utilizando la función `imprimirPersona`.