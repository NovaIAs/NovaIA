```f#
// Definir una función para encontrar el máximo común divisor de dos números
let rec mcd a b =
    match b with
    | 0 -> a
    | _ -> mcd b (a % b)

// Definir una función para encontrar el mínimo común múltiplo de dos números
let rec mcm a b =
    a * b / mcd a b

// Definir una función para encontrar todos los factores de un número
let factores n =
    let rec factores_aux n factores =
        match n with
        | 1 -> factores
        | _ ->
            let divisor = 2
            while divisor <= n / divisor do
                if n % divisor = 0 then
                    factores_aux (n / divisor) (factores @ [divisor])
                else
                    divisor <- divisor + 1
            done
    factores_aux n []

// Definir una función para encontrar todos los divisores de un número
let divisores n =
    let rec divisores_aux n divisores =
        match n with
        | 1 -> divisores
        | _ ->
            let divisor = 2
            while divisor <= n do
                if n % divisor = 0 then
                    divisores_aux (n / divisor) (divisores @ [divisor; n / divisor])
                else
                    divisor <- divisor + 1
            done
    divisores_aux n []

// Definir una función para encontrar todos los números primos hasta un número dado
let primos n =
    let rec primos_aux n primos =
        match n with
        | 1 -> primos
        | _ ->
            let divisor = 2
            while divisor <= n / divisor do
                if n % divisor = 0 then
                    primos_aux (n / divisor) primos
                else
                    divisor <- divisor + 1
            done
            if n > 1 then
                primos_aux (n - 1) (primos @ [n])
    primos_aux n []

// Definir una función para encontrar todos los números perfectos hasta un número dado
let perfectos n =
    let rec perfectos_aux n perfectos =
        match n with
        | 1 -> perfectos
        | _ ->
            let suma_divisores = List.sum (divisores n)
            if suma_divisores = n then
                perfectos_aux (n - 1) (perfectos @ [n])
            else
                perfectos_aux (n - 1) perfectos
    perfectos_aux n []

// Definir una función para encontrar todos los números amigos hasta un número dado
let amigos n =
    let rec amigos_aux n amigos =
        match n with
        | 1 -> amigos
        | _ ->
            let suma_divisores_propios = List.sum (divisores n) - n
            let suma_divisores_amigo = List.sum (divisores suma_divisores_propios)
            if suma_divisores_amigo = n and suma_divisores_propios <> n then
                amigos_aux (n - 1) (amigos @ [(n, suma_divisores_propios)])
            else
                amigos_aux (n - 1) amigos
    amigos_aux n []

// Definir una función para encontrar todos los números de Fibonacci hasta un número dado
let fibonacci n =
    let rec fibonacci_aux n fibs =
        match n with
        | 0 -> fibs
        | 1 -> fibs
        | _ ->
            let fib_n = List.last fibs + List.nth fibs (List.length fibs - 2)
            fibonacci_aux (n - 1) (fibs @ [fib_n])
    fibonacci_aux n [0; 1]
```

Este código es un conjunto de funciones matemáticas que realizan diversas operaciones con números. Incluye funciones para encontrar el máximo común divisor, el mínimo común múltiplo, los factores, los divisores, los números primos, los números perfectos, los números amigos y los números de Fibonacci.

El código está escrito en F#, un lenguaje de programación funcional. F# es un lenguaje poderoso y expresivo que es particularmente adecuado para escribir código matemático.

El código está bien documentado con comentarios que explican lo que hace cada función.

Aquí hay algunos ejemplos de cómo se puede usar el código:

```f#
// Encontrar el máximo común divisor de 12 y 18
let mcd_12_18 = mcd 12 18
printfn "%d" mcd_12_18

// Encontrar el mínimo común múltiplo de 12 y 18
let mcm_12_18 = mcm 12 18
printfn "%d" mcm_12_18

// Encontrar todos los factores de 12
let factores_12 = factores 12
printfn "%A" factores_12

// Encontrar todos los divisores de 12
let divisores_12 = divisores 12
printfn "%A" divisores_12

// Encontrar todos los números primos hasta 100
let primos_100 = primos 100
printfn "%A" primos_100

// Encontrar todos los números perfectos hasta 100
let perfectos_100 = perfectos 100
printfn "%A" perfectos_100

// Encontrar todos los números amigos hasta 100
let amigos_100 = amigos 100
printfn "%A" amigos_100

// Encontrar todos los números de Fibonacci hasta 100
let fibonacci_100 = fibonacci 100
printfn "%A" fibonacci_100
```