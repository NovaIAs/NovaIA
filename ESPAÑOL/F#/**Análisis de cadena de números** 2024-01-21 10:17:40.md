```f#
// Importar las bibliotecas necesarias
open System
open System.Text.RegularExpressions
open System.Collections.Generic

// Definir una función para extraer los números de una cadena
let extraerNumeros cadena =
    let regex = new Regex("[0-9]+")
    regex.Matches(cadena)
        |> Seq.map (fun m -> int m.Value)
        |> List.ofSeq

// Definir una función para calcular la suma de los números de una lista
let sumarNumeros lista =
    lista |> Seq.sum

// Definir una función para encontrar el número mayor de una lista
let encontrarNumeroMayor lista =
    lista |> Seq.max

// Definir una función para encontrar el número menor de una lista
let encontrarNumeroMenor lista =
    lista |> Seq.min

// Definir una función para calcular la media de los números de una lista
let calcularMedia lista =
    let suma = lista |> Seq.sum
    let count = lista |> Seq.length
    suma / count

// Definir una función para generar una lista de números aleatorios
let generarListaNumerosAleatorios n =
    let random = new Random()
    let lista =
        [| for _ in 1..n -> random.Next(1, 1000) |]
    lista

// Definir una función para imprimir los resultados
let imprimirResultados lista =
    printfn "Números extraídos: %A" lista
    printfn "Suma de los números: %i" (sumarNumeros lista)
    printfn "Número mayor: %i" (encontrarNumeroMayor lista)
    printfn "Número menor: %i" (encontrarNumeroMenor lista)
    printfn "Media de los números: %f" (calcularMedia lista)

// Definir la función principal
let main args =
    // Obtener la cadena de entrada del usuario
    printfn "Introduzca una cadena de números separados por espacios:"
    let cadena = Console.ReadLine()

    // Extraer los números de la cadena
    let lista = extraerNumeros cadena

    // Imprimir los resultados
    imprimirResultados lista

    // Devolver el código de salida
    0

// Iniciar el programa
[<EntryPoint>]
let _ = main []
```

Este código es una aplicación en F# que solicita al usuario que introduzca una cadena de números separados por espacios. A continuación, extrae los números de la cadena, calcula su suma, su número mayor, su número menor y su media, y los imprime por consola.

El código utiliza varias funciones definidas por el usuario, que se explican a continuación:

* `extraerNumeros`: Esta función utiliza la clase `Regex` de la biblioteca `System.Text.RegularExpressions` para extraer los números de una cadena. La expresión regular utilizada es `[0-9]+`, que coincide con una o más ocurrencias de un dígito decimal.
* `sumarNumeros`: Esta función calcula la suma de los números de una lista utilizando la función `Seq.sum` de la biblioteca `System.Collections.Generic`.
* `encontrarNumeroMayor`: Esta función encuentra el número mayor de una lista utilizando la función `Seq.max` de la biblioteca `System.Collections.Generic`.
* `encontrarNumeroMenor`: Esta función encuentra el número menor de una lista utilizando la función `Seq.min` de la biblioteca `System.Collections.Generic`.
* `calcularMedia`: Esta función calcula la media de los números de una lista dividiendo la suma de los números por el número de números de la lista.
* `generarListaNumerosAleatorios`: Esta función genera una lista de números aleatorios utilizando la clase `Random` de la biblioteca `System`.
* `imprimirResultados`: Esta función imprime los resultados del análisis de la cadena de números introducida por el usuario.

La función principal del programa, `main`, primero solicita al usuario que introduzca una cadena de números separados por espacios. A continuación, llama a la función `extraerNumeros` para extraer los números de la cadena, y a la función `imprimirResultados` para imprimir los resultados del análisis de la cadena de números.

El programa finaliza devolviendo el código de salida `0`, que indica que el programa se ha ejecutado correctamente.