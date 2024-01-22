```f#
// Creación de una lista de enteros
let numeros = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

// Creación de una función para encontrar el máximo de una lista
let maximo (lista: int list) =
    // Definición del valor máximo inicial como el primer elemento de la lista
    let maximo = lista.[0]
    // Recorrido de la lista desde el segundo elemento
    for i = 1 to lista.Length - 1 do
        // Actualización del valor máximo si el elemento actual es mayor que el máximo actual
        if lista.[i] > maximo then
            maximo <- lista.[i]
    // Retorno del valor máximo
    maximo

// Creación de una función para encontrar el mínimo de una lista
let minimo (lista: int list) =
    // Definición del valor mínimo inicial como el primer elemento de la lista
    let minimo = lista.[0]
    // Recorrido de la lista desde el segundo elemento
    for i = 1 to lista.Length - 1 do
        // Actualización del valor mínimo si el elemento actual es menor que el mínimo actual
        if lista.[i] < minimo then
            minimo <- lista.[i]
    // Retorno del valor mínimo
    minimo

// Creación de una función para calcular la suma de una lista
let suma (lista: int list) =
    // Definición de la suma inicial como 0
    let suma = 0
    // Recorrido de la lista
    for i = 0 to lista.Length - 1 do
        // Actualización de la suma con el elemento actual
        suma <- suma + lista.[i]
    // Retorno de la suma
    suma

// Creación de una función para calcular el promedio de una lista
let promedio (lista: int list) =
    // Cálculo de la suma de la lista
    let suma = suma lista
    // División de la suma por el número de elementos de la lista
    suma / (float lista.Length)

// Creación de una función para encontrar la mediana de una lista
let mediana (lista: int list) =
    // Ordenación de la lista en orden ascendente
    let listaOrdenada = lista.OrderBy(fun x -> x)
    // Cálculo del índice del elemento central de la lista
    let indiceCentral = listaOrdenada.Length / 2
    // Retorno del elemento central de la lista
    listaOrdenada.[indiceCentral]

// Impresión de los resultados en la consola
printfn "Máximo: %A" (maximo numeros)
printfn "Mínimo: %A" (minimo numeros)
printfn "Suma: %A" (suma numeros)
printfn "Promedio: %A" (promedio numeros)
printfn "Mediana: %A" (mediana numeros)
```

Este código es un ejemplo de código complejo en F# que realiza varias operaciones numéricas en una lista de enteros. El código define una lista de enteros, y luego define cinco funciones para realizar las siguientes operaciones:

- Máximo: encuentra el valor máximo de la lista.
- Mínimo: encuentra el valor mínimo de la lista.
- Suma: calcula la suma de todos los elementos de la lista.
- Promedio: calcula el promedio de todos los elementos de la lista.
- Mediana: encuentra la mediana de la lista.

El código utiliza la sintaxis de F# para definir funciones, bucles y expresiones lambda. También utiliza la función `printfn` para imprimir los resultados en la consola.

Este código es un ejemplo de código complejo en F# que demuestra las capacidades del lenguaje para realizar operaciones matemáticas en listas de datos.