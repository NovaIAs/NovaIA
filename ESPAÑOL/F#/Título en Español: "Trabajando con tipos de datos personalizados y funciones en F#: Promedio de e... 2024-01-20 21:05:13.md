```f#
// Crea un nuevo tipo de datos llamado "Persona" con dos propiedades: "nombre" y "edad".
type Persona =
    { Nombre : string
      Edad : int }

// Crea una lista de personas.
let personas = [
    { Nombre = "Juan"; Edad = 20 },
    { Nombre = "María"; Edad = 25 },
    { Nombre = "Pedro"; Edad = 30 },
    { Nombre = "Ana"; Edad = 35 }
]

// Crea una función que recibe una lista de personas y devuelve la edad promedio.
let promedioEdad personas =
    personas
    |> List.map (fun p -> p.Edad) // Extrae la edad de cada persona.
    |> List.average // Calcula el promedio de las edades.

// Imprime la edad promedio de las personas.
printfn "La edad promedio de las personas es: %f" (promedioEdad personas)

// Crea una función que recibe una lista de personas y devuelve la persona más joven.
let personaMasJoven personas =
    personas
    |> List.minBy (fun p -> p.Edad) // Obtiene la persona con la edad más baja.

// Imprime el nombre de la persona más joven.
printfn "La persona más joven es: %s" (personaMasJoven personas).Nombre

// Crea una función que recibe una lista de personas y devuelve la persona más vieja.
let personaMasVieja personas =
    personas
    |> List.maxBy (fun p -> p.Edad) // Obtiene la persona con la edad más alta.

// Imprime el nombre de la persona más vieja.
printfn "La persona más vieja es: %s" (personaMasVieja personas).Nombre

// Crea una función que recibe una lista de personas y devuelve una lista de los nombres de las personas.
let obtenerNombres personas =
    personas
    |> List.map (fun p -> p.Nombre) // Extrae el nombre de cada persona.

// Imprime los nombres de las personas.
printfn "Los nombres de las personas son:"
personas
|> obtenerNombres
|> List.iter (printfn "%s") // Imprime cada nombre en una línea nueva.
```

Este código crea un nuevo tipo de datos llamado "Persona" con dos propiedades: "nombre" y "edad". Luego, crea una lista de personas y utiliza funciones para calcular la edad promedio, obtener la persona más joven y la persona más vieja de la lista, y obtener una lista de los nombres de las personas. Finalmente, imprime estos resultados en la consola.