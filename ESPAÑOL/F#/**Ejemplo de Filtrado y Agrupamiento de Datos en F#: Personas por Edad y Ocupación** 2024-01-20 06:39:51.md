```f#

// Definir un tipo de datos personalizado para representar una persona
type Persona = {
    Nombre: string
    Edad: int
    Ocupacion: string
}

// Crear una lista de personas
let personas = [
    { Nombre = "Juan"; Edad = 25; Ocupacion = "Ingeniero" }
    { Nombre = "María"; Edad = 22; Ocupacion = "Doctora" }
    { Nombre = "Pedro"; Edad = 30; Ocupacion = "Abogado" }
]

// Filtrar la lista de personas para obtener aquellas que son mayores de 25 años
let personasMayoresDe25 =
    personas
    |> List.filter (fun persona -> persona.Edad > 25)

// Mostrar las personas mayores de 25 años
personasMayoresDe25
|> List.iter (fun persona ->
    printfn "%s (%d años) es %s" persona.Nombre persona.Edad persona.Ocupacion)

// Agrupar las personas por su ocupación
let personasPorOcupacion =
    personas
    |> List.groupBy (fun persona -> persona.Ocupacion)

// Mostrar las personas agrupadas por su ocupación
personasPorOcupacion
|> Seq.iter (fun (ocupacion, personas) ->
    printfn "%s:" ocupacion
    personas
    |> List.iter (fun persona ->
        printfn "\t%s (%d años)" persona.Nombre persona.Edad))
```

Explicación:

1. Definimos un tipo de datos personalizado llamado `Persona` para representar a una persona con sus características: `Nombre`, `Edad` y `Ocupación`.

2. Creamos una lista de personas llamada `personas` que contiene información de tres personas.

3. Utilizamos la función `List.filter` para filtrar la lista de personas y obtener aquellas que son mayores de 25 años, y las almacenamos en `personasMayoresDe25`.

4. Mostramos las personas mayores de 25 años utilizando una función `List.iter` para iterar sobre la lista y mostrar la información de cada persona.

5. Utilizamos la función `List.groupBy` para agrupar las personas por su ocupación, y almacenamos el resultado en `personasPorOcupacion`.

6. Mostramos las personas agrupadas por su ocupación utilizando una función `Seq.iter` para iterar sobre la secuencia de grupos y mostrar la información de cada grupo.

7. Dentro de cada grupo, utilizamos otra función `List.iter` para iterar sobre la lista de personas y mostrar la información de cada persona.

Este código muestra cómo utilizar tipos de datos personalizados, filtrado de listas, agrupamiento de listas y iteración sobre listas y secuencias en F#.