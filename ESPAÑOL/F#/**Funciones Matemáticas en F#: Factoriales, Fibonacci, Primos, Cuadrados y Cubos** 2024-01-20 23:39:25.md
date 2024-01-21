```f#
// Definir una función para calcular el factorial de un número
let factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n - 1)

// Definir una función para calcular la secuencia de Fibonacci
let fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

// Definir una función para comprobar si un número es primo
let isPrime n =
    if n <= 1 then
        false
    else if n = 2 then
        true
    else
        seq { 2 .. n - 1 }
        |> Seq.filter (fun x -> n % x = 0)
        |> Seq.isEmpty

// Definir una función para generar una lista de números primos hasta un número dado
let primeNumbersTo n =
    seq { 2 .. n }
    |> Seq.filter isPrime
    |> Seq.toList

// Definir una función para generar una lista de cuadrados de los números hasta un número dado
let squareNumbersTo n =
    seq { 1 .. n }
    |> Seq.map (fun x -> x * x)
    |> Seq.takeWhile (fun x -> x <= n)
    |> Seq.toList

// Definir una función para generar una lista de cubos de los números hasta un número dado
let cubeNumbersTo n =
    seq { 1 .. n }
    |> Seq.map (fun x -> x * x * x)
    |> Seq.takeWhile (fun x -> x <= n)
    |> Seq.toList

// Imprimir los resultados
printfn "Factorial de 10: %d" (factorial 10)
printfn "Secuencia Fibonacci: %A" (seq { 0 .. 10 } |> Seq.map fibonacci |> Seq.toList)
printfn "Números primos hasta 100: %A" (primeNumbersTo 100)
printfn "Cuadrados de los números hasta 100: %A" (squareNumbersTo 100)
printfn "Cubos de los números hasta 100: %A" (cubeNumbersTo 100)
```

Explicación del código:

* La función `factorial` calcula el factorial de un número dado. El factorial de un número es el producto de todos los números enteros positivos desde 1 hasta ese número. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.
* La función `fibonacci` calcula el n-ésimo número de la secuencia de Fibonacci. La secuencia de Fibonacci es una serie de números en la que cada número es la suma de los dos números anteriores. Los dos primeros números de la secuencia son 0 y 1, y los siguientes números se generan sumando los dos números anteriores. Por ejemplo, el quinto número de la secuencia es 5, que se calcula sumando los dos números anteriores, 3 y 2.
* La función `isPrime` comprueba si un número dado es primo. Un número primo es un número natural mayor que 1 que tiene exactamente dos divisores distintos: 1 y sí mismo. Por ejemplo, el número 7 es primo porque sus únicos divisores son 1 y 7.
* La función `primeNumbersTo` genera una lista de números primos hasta un número dado. La función utiliza la función `isPrime` para comprobar si cada número es primo y luego incluye los números primos en la lista.
* La función `squareNumbersTo` genera una lista de cuadrados de los números hasta un número dado. La función utiliza la función `map` para aplicar la función de cuadrado a cada número y luego incluye los cuadrados de los números en la lista.
* La función `cubeNumbersTo` genera una lista de cubos de los números hasta un número dado. La función utiliza la función `map` para aplicar la función de cubo a cada número y luego incluye los cubos de los números en la lista.

El código utiliza las funciones anteriores para calcular y mostrar los factoriales, la secuencia de Fibonacci, los números primos, los cuadrados de los números y los cubos de los números hasta el número 100.