```f#
// Importación de las bibliotecas necesarias
open System
open System.Text
open System.Collections.Generic

// Definición de una función para calcular el factorial de un número
let factorial n =
    if n = 0 then 1
    else n * factorial (n - 1)

// Definición de una función para generar una lista de números primos hasta un límite
let primes max =
    let mutable result = []
    for i = 2 to max do
        if isPrime i then result.Add(i)
    result

// Función para determinar si un número es primo
and isPrime n =
    if n <= 1 then false
    else
        let limit = (int)Math.Floor(Math.Sqrt(float n))
        for i = 2 to limit do
            if n % i = 0 then false
        true

// Definición de una función para calcular el máximo común divisor de dos números
let gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Definición de una función para calcular el mínimo común múltiplo de dos números
let lcm a b =
    (a * b) / gcd a b

// Definición de una función para calcular la matriz inversa de una matriz dada
let inverse matrix =
    let n = matrix.GetLength(0)
    let result = Array2D.init n n 0.0
    for i in 0..(n - 1) do
        for j in 0..(n - 1) do
            if i = j then result.[i, j] <- 1.0
    let x = Array2D.init n n 0.0
    for i in 0..(n - 1) do
        x.[i, i] <- 1.0 / matrix.[i, i]
        for j in (i + 1)..(n - 1) do
            let factor = (-1.0) * matrix.[j, i] / matrix.[i, i]
            for k in 0..(n - 1) do
                result.[j, k] <- result.[j, k] + factor * result.[i, k]
                x.[j, k] <- x.[j, k] + factor * x.[i, k]
    result

// Definición de una función para resolver un sistema de ecuaciones lineales
let solveSystem matrix b =
    let n = matrix.GetLength(0)
    let result = Array.zeroCreate n
    for i in 0..(n - 1) do
        let factor = b.[i] / matrix.[i, i]
        for j in 0..(n - 1) do
            if j <> i then result.[j] <- result.[j] - factor * matrix.[i, j]
        result.[i] <- factor
    result

// Impresión de los resultados de las funciones
printfn "Factorial de 5: %d" (factorial 5)
printfn "Números primos hasta 100:"
for prime in primes 100 do
    printfn "%d" prime
printfn "Máximo común divisor de 12 y 18: %d" (gcd 12 18)
printfn "Mínimo común múltiplo de 12 y 18: %d" (lcm 12 18)
let matrix = Array2D.init 3 3 0.0
matrix.[0, 0] <- 2.0
matrix.[0, 1] <- 1.0
matrix.[0, 2] <- -1.0
matrix.[1, 0] <- 3.0
matrix.[1, 1] <- 2.0
matrix.[1, 2] <- 1.0
matrix.[2, 0] <- 1.0
matrix.[2, 1] <- 1.0
matrix.[2, 2] <- 4.0
let b = Array.init 3 (fun _ -> 0.0)
b.[0] <- 1.0
b.[1] <- 2.0
b.[2] <- 3.0
let result = solveSystem matrix b
printfn "Solución al sistema de ecuaciones:"
for i in 0..(result.Length - 1) do
    printfn "x%d = %f" (i + 1) result.[i]
```