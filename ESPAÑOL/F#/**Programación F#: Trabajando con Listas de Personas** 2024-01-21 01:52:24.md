```f#
// Definición de tipos y estructuras de datos:

// Definimos un tipo "Persona" que tiene dos propiedades: "nombre" y "edad".
type Persona = { nombre: string; edad: int }

// Definimos una lista de personas para trabajar con ella.
let personas = [
    { nombre = "Juan"; edad = 20 },
    { nombre = "María"; edad = 30 },
    { nombre = "Pedro"; edad = 40 },
    { nombre = "Ana"; edad = 50 }
]

// Definición de funciones:

// Definimos una función "promedioEdad" que calcula la edad promedio de una lista de personas.
let promedioEdad personas =
    let sumaEdades = personas |> Seq.sumBy (fun p -> p.edad)
    let cantidadPersonas = personas |> Seq.length
    sumaEdades / cantidadPersonas

// Definimos una función "ordenarPorEdad" que ordena una lista de personas por edad en orden ascendente.
let ordenarPorEdad personas =
    personas |> Seq.sortBy (fun p -> p.edad)

// Definimos una función "encontrarPorNombre" que encuentra a una persona en una lista por su nombre.
let encontrarPorNombre personas nombre =
    personas |> Seq.tryFind (fun p -> p.nombre = nombre)

// Definimos una función "imprimirPersona" que imprime la información de una persona.
let imprimirPersona persona =
    printfn "%s tiene %d años" persona.nombre persona.edad

// Uso de las funciones y estructuras de datos:

// Calculamos la edad promedio de la lista de personas.
let edadPromedio = promedioEdad personas

// Imprimimos la edad promedio.
printfn "La edad promedio es: %f" edadPromedio

// Ordenamos la lista de personas por edad.
let personasOrdenadas = ordenarPorEdad personas

// Imprimimos la lista de personas ordenadas.
personasOrdenadas |> Seq.iter (fun p -> imprimirPersona p)

// Buscamos a una persona en la lista por su nombre.
let personaEncontrada = encontrarPorNombre personas "Pedro"

// Si la persona fue encontrada, la imprimimos.
if personaEncontrada.IsSome then
    imprimirPersona personaEncontrada.Value

// Si la persona no fue encontrada, imprimimos un mensaje.
else
    printfn "No se encontró a la persona con el nombre \"Pedro\""
```

Este código es un ejemplo de un programa escrito en F# que realiza operaciones con una lista de personas. El código define estructuras de datos, funciones y luego usa esas funciones para manipular y mostrar información sobre la lista de personas.

El código primero define un tipo "Persona" con dos propiedades: "nombre" y "edad". Luego, define una lista de personas con cuatro elementos, cada uno con un nombre y una edad diferentes.

Luego, el código define tres funciones:

- "promedioEdad": Calcula la edad promedio de una lista de personas.
- "ordenarPorEdad": Ordena una lista de personas por edad en orden ascendente.
- "encontrarPorNombre": Encuentra a una persona en una lista por su nombre.

El código finalmente usa estas funciones para calcular la edad promedio de la lista de personas, ordenar la lista por edad e imprimir la información de cada persona en la lista.