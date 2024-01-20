```f#
// Definir una función para calcular la potencia de un número
let elevarPotencia x n =
    if n = 0 then 1
    elif n > 0 then x * elevarPotencia x (n-1)
    else 1.0 / elevarPotencia x (-n)

// Definir una función para generar una lista de números de 1 a n
let generarLista n =
    [1..n]

// Definir una función para calcular la suma de los cuadrados de los números de una lista
let sumaCuadrados lista =
    list.Sum(fun x -> x * x)

// Definir una función para calcular la media de una lista de números
let calcularMedia lista =
    list.Average()

// Definir una función para calcular la desviación estándar de una lista de números
let calcularDesviacionEstandar lista =
    let media = calcularMedia lista
    let cuadradosDiferencias = list.Sum(fun x -> (x - media) * (x - media))
    Math.Sqrt(cuadradosDiferencias / (float list.Length - 1.0))

// Definir una función para imprimir los valores de una lista de números
let imprimirLista lista =
    printfn "%A" lista

// Definir una función para imprimir los valores de una lista de strings
let imprimirStrings lista =
    printfn "%A" lista

// Definir una función para convertir una lista de números a una lista de strings
let convertirNumerosAStrings lista =
    list.Map(fun x -> x.ToString())

// Definir una función para imprimir un mensaje de bienvenida
let imprimirBienvenida =
    printfn "Bienvenido al programa de estadísticas"

// Definir una función para imprimir un mensaje de despedida
let imprimirDespedida =
    printfn "Gracias por utilizar el programa de estadísticas"

// Definir la función principal del programa
let main argv =
    // Imprimir el mensaje de bienvenida
    imprimirBienvenida

    // Generar una lista de números de 1 a 10
    let listaNumeros = generarLista 10

    // Imprimir la lista de números
    imprimirLista listaNumeros

    // Calcular la suma de los cuadrados de los números de la lista
    let sumaCuadrados = sumaCuadrados listaNumeros

    // Imprimir la suma de los cuadrados
    printfn "Suma de los cuadrados: %d" sumaCuadrados

    // Calcular la media de la lista de números
    let media = calcularMedia listaNumeros

    // Imprimir la media
    printfn "Media: %f" media

    // Calcular la desviación estándar de la lista de números
    let desviacionEstandar = calcularDesviacionEstandar listaNumeros

    // Imprimir la desviación estándar
    printfn "Desviación estándar: %f" desviacionEstandar

    // Convertir la lista de números a una lista de strings
    let listaStrings = convertirNumerosAStrings listaNumeros

    // Imprimir la lista de strings
    imprimirStrings listaStrings

    // Imprimir el mensaje de despedida
    imprimirDespedida

    // Salir del programa
    0

// Llamar a la función principal del programa
main argv
```