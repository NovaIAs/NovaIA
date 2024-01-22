```f#
// Importación de bibliotecas necesarias
open System
open System.Collections.Generic

// Definición de tipos y funciones auxiliares
type Persona = { Nombre: string, Edad: int }
let imprimirPersona (persona: Persona) =
    printfn "%s tiene %d años" persona.Nombre persona.Edad

// Definición de la función principal
let main argv =
    // Crear una lista de personas
    let personas =
        [
            { Nombre = "Juan", Edad = 20 };
            { Nombre = "María", Edad = 25 };
            { Nombre = "Pedro", Edad = 30 }
        ]

    // Iterar sobre la lista de personas
    personas
    |> Seq.iter imprimirPersona

    // Retornar código de salida 0 (éxito)
    0

// Ejecutar la función principal
[<EntryPoint>]
let _ = main (Array.empty<string>)
```

**Explicación del código:**

* La primera línea importa la biblioteca `System` que contiene tipos y funciones básicas del lenguaje .NET.
* La segunda línea importa la biblioteca `System.Collections.Generic` que contiene tipos y funciones genéricas para trabajar con colecciones.
* La tercera línea define un tipo llamado `Persona` que tiene dos propiedades: `Nombre` y `Edad`.
* La cuarta línea define una función llamada `imprimirPersona` que toma un objeto de tipo `Persona` y lo imprime en la consola.
* La quinta línea crea una lista de objetos `Persona` con los nombres y edades de tres personas.
* La sexta línea utiliza el operador `|>` para canalizar la lista de personas a la función `Seq.iter` que itera sobre cada elemento de la lista y llama a la función `imprimirPersona` para cada elemento.
* La séptima línea retorna el código de salida 0, que indica que el programa se ejecutó correctamente.
* La octava línea indica que la función `main` es el punto de entrada del programa.
* La novena línea ejecuta la función `main` con un arreglo vacío de argumentos.