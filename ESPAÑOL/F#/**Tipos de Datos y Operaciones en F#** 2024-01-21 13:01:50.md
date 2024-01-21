```f#
// Definición de tipos

type Persona = { Nombre: string; Edad: int; Profesión: string }
type Ciudad = { Nombre: string; Población: int; Alcalde: Persona }

// Creación de instancias

let persona1 = { Nombre = "Juan"; Edad = 30; Profesión = "Ingeniero" }
let persona2 = { Nombre = "María"; Edad = 25; Profesión = "Médica" }

let ciudad1 = { Nombre = "Madrid"; Población = 3200000; Alcalde = persona1 }
let ciudad2 = { Nombre = "Barcelona"; Población = 1600000; Alcalde = persona2 }

// Operaciones con tipos

let imprimirPersona (persona: Persona) =
    printfn "%s, %d años, %s" persona.Nombre persona.Edad persona.Profesión

let imprimirCiudad (ciudad: Ciudad) =
    printfn "%s, población: %d, alcalde: %s" ciudad.Nombre ciudad.Población ciudad.Alcalde.Nombre

// Búsqueda de elementos en una lista

let buscarPersona (lista: Persona list) (nombre: string) =
    list |> List.tryFind (fun p -> p.Nombre = nombre)

// Filtrado de elementos en una lista

let filtrarPersonas (lista: Persona list) (profesión: string) =
    list |> List.filter (fun p -> p.Profesión = profesión)

// Ordenación de elementos en una lista

let ordenarPersonas (lista: Persona list) (comparador: (Persona -> Persona -> int)) =
    list |> List.sortWith comparador

// Agrupación de elementos en una lista

let agruparPersonas (lista: Persona list) (clave: (Persona -> string)) =
    list |> List.groupBy clave

// Proyección de elementos en una lista

let proyectarPersonas (lista: Persona list) (selector: (Persona -> 'T)) =
    list |> List.map selector

// Reducción de elementos en una lista

let reducirPersonas (lista: Persona list) (operador: ('T -> 'T -> 'T)) (semilla: 'T) =
    list |> List.fold operador semilla

// Programa principal

let personas = [persona1; persona2]
let ciudades = [ciudad1; ciudad2]

// Impresión de información

printfn "Personas:"
personas |> List.iter imprimirPersona

printfn "Ciudades:"
ciudades |> List.iter imprimirCiudad

// Búsqueda de una persona por nombre

let personaBuscada = buscarPersona personas "María"
printfn "Persona buscada: %s" personaBuscada.Nombre

// Filtrado de personas por profesión

let personasFiltradas = filtrarPersonas personas "Ingeniero"
printfn "Personas filtradas:"
personasFiltradas |> List.iter imprimirPersona

// Ordenación de personas por edad

let personasOrdenadas = ordenarPersonas personas (fun p1 p2 -> p1.Edad - p2.Edad)
printfn "Personas ordenadas por edad:"
personasOrdenadas |> List.iter imprimirPersona

// Agrupación de personas por profesión

let personasAgrupadas = agruparPersonas personas (fun p -> p.Profesión)
printfn "Personas agrupadas por profesión:"
personasAgrupadas |> List.iter (fun (profesión, personas) -> printf "%s:\n" profesión |> List.iter imprimirPersona)

// Proyección de personas a sus nombres

let nombresPersonas = proyectarPersonas personas (fun p -> p.Nombre)
printfn "Nombres de las personas:"
nombresPersonas |> List.iter printfn

// Reducción de personas a la edad media

let edadMedia = reducirPersonas personas (fun edad1 edad2 -> edad1 + edad2) 0.0 |> (fun suma -> suma / float personas.Length)
printfn "Edad media: %.2f" edadMedia
```

Explicación del código:

1. Definición de tipos: Se definen los tipos `Persona` y `Ciudad` usando el tipo `record`.

2. Creación de instancias: Se crean dos instancias del tipo `Persona` y dos instancias del tipo `Ciudad`.

3. Operaciones con tipos: Se definen las funciones `imprimirPersona` e `imprimirCiudad` para imprimir información sobre una persona o una ciudad, respectivamente.

4. Búsqueda de elementos en una lista: Se define la función `buscarPersona` para buscar una persona en una lista por su nombre.

5. Filtrado de elementos en una lista: Se define la función `filtrarPersonas` para filtrar una lista de personas por su profesión.

6. Ordenación de elementos en una lista: Se define la función `ordenarPersonas` para ordenar una lista de personas por un comparador especificado.

7. Agrupación de elementos en una lista: Se define la función `agruparPersonas` para agrupar una lista de personas por una clave especificada.

8. Proyección de elementos en una lista: Se define la función `proyectarPersonas` para proyectar una lista de personas a una lista de valores de otro tipo.

9. Reducción de elementos en una lista: Se define la función `reducirPersonas` para reducir una lista de personas a un único valor usando un operador especificado.

10. Programa principal: Se crean listas de personas y ciudades, se imprimen, se realizan búsquedas, filtrados, ordenaciones, agrupaciones, proyecciones y reducciones en las listas, y se imprimen los resultados.