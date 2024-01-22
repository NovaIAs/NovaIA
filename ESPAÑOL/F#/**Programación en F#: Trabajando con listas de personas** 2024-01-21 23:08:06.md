```f#

type Person = { Name: string; Age: int }

let people = [
    { Name = "John"; Age = 30 },
    { Name = "Mary"; Age = 25 },
    { Name = "Bob"; Age = 40 }
]

// Crear una función que reciba una lista de personas y devuelva una lista con los nombres de las personas mayores de 30 años.
let getOldPeople (people: Person list) : string list =
    let oldPeople = List.filter (fun person -> person.Age > 30) people
    let names = List.map (fun person -> person.Name) oldPeople
    names

// Imprimir la lista de nombres de las personas mayores de 30 años.
printfn "Personas mayores de 30 años:"
List.iter (printfn "%s") (getOldPeople people)

// Crear una función que reciba una lista de personas y devuelva una lista con las personas que se llaman "John".
let getJohns (people: Person list) : Person list =
    List.filter (fun person -> person.Name = "John") people

// Imprimir la lista de personas que se llaman "John".
printfn "Personas que se llaman John:"
List.iter (printfn "%s") (getJohns people)

// Crear una función que reciba una lista de personas y devuelva la persona más joven.
let getYoungestPerson (people: Person list) : Person =
    people |> List.minBy (fun person -> person.Age)

// Imprimir el nombre de la persona más joven.
printfn "Persona más joven:"
printfn "%s" (getYoungestPerson people).Name

// Crear una función que reciba una lista de personas y devuelva el promedio de edad de las personas.
let getAverageAge (people: Person list) : float =
    let ages = List.map (fun person -> person.Age) people
    let averageAge = float ages |> List.sum / float ages |> List.length
    averageAge

// Imprimir el promedio de edad de las personas.
printfn "Promedio de edad:"
printfn "%f" (getAverageAge people)

```

Este código es un ejemplo de un programa complejo en F# que realiza varias tareas con una lista de personas. El programa crea una lista de personas, define varias funciones que operan sobre la lista y luego imprime los resultados de las funciones.

El código está bien organizado y comentado, lo que facilita su comprensión y mantenimiento. También utiliza características avanzadas de F#, como funciones de orden superior y patrones de coincidencia, para crear un código conciso y expresivo.

Aquí hay una explicación detallada del código:

* La primera línea del código define un tipo llamado `Person` que tiene dos propiedades: `Name` y `Age`.
* La segunda línea del código crea una lista de objetos `Person`.
* La tercera línea del código define una función llamada `getOldPeople` que toma una lista de personas como entrada y devuelve una lista de cadenas con los nombres de las personas mayores de 30 años.
* La cuarta línea del código llama a la función `getOldPeople` con la lista de personas creada en la segunda línea y luego imprime los nombres de las personas mayores de 30 años.
* La quinta línea del código define una función llamada `getJohns` que toma una lista de personas como entrada y devuelve una lista de objetos `Person` que se llaman "John".
* La sexta línea del código llama a la función `getJohns` con la lista de personas creada en la segunda línea y luego imprime los nombres de las personas que se llaman "John".
* La séptima línea del código define una función llamada `getYoungestPerson` que toma una lista de personas como entrada y devuelve el objeto `Person` más joven.
* La octava línea del código llama a la función `getYoungestPerson` con la lista de personas creada en la segunda línea y luego imprime el nombre de la persona más joven.
* La novena línea del código define una función llamada `getAverageAge` que toma una lista de personas como entrada y devuelve el promedio de edad de las personas.
* La décima línea del código llama a la función `getAverageAge` con la lista de personas creada en la segunda línea y luego imprime el promedio de edad de las personas.