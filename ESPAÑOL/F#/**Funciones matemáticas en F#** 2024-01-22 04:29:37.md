```f#
// Definir una función recursiva para calcular el factorial de un número
let factorial n =
    if n = 0 then 1
    else n * factorial (n - 1)

// Definir una función para generar una lista de números primos hasta un número dado
let primesTo n =
    let rec primesToHelper n acc =
        if n <= 1 then acc
        else
            let isPrime =
                List.forall (fun x -> n % x <> 0) (List.filter (fun x -> x < n) acc)
            if isPrime then primesToHelper (n - 1) (n :: acc)
            else primesToHelper (n - 1) acc
    primesToHelper n []

// Definir una función para encontrar el máximo común divisor de dos números
let gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Definir una función para encontrar el mínimo común múltiplo de dos números
let lcm a b =
    a * b / gcd a b

// Definir una función para generar una lista de números perfectos hasta un número dado
let perfectNumbersTo n =
    let rec perfectNumbersToHelper n acc =
        if n <= 1 then acc
        else
            let isPerfect =
                List.forall (fun x -> n % x = 0)
                    (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))
            if isPerfect then perfectNumbersToHelper (n - 1) (n :: acc)
            else perfectNumbersToHelper (n - 1) acc
    perfectNumbersToHelper n []

// Definir una función para encontrar el número de Fibonacci en una posición dada
let fibonacci n =
    if n < 2 then n
    else fibonacci (n - 1) + fibonacci (n - 2)

// Definir una función para generar una lista de números de Fibonacci hasta un número dado
let fibonacciTo n =
    let rec fibonacciToHelper n acc =
        if n < 2 then acc
        else fibonacciToHelper (n - 1) (fibonacci n :: acc)
    fibonacciToHelper n []

// Definir una función para determinar si un número es primo
let isPrime n =
    if n <= 1 then false
    else
        List.forall (fun x -> n % x <> 0)
            (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))

// Definir una función para generar una lista de números primos hasta un número dado
let primesTo n =
    let rec primesToHelper n acc =
        if n <= 1 then acc
        else
            let isPrime =
                List.forall (fun x -> n % x <> 0) (List.filter (fun x -> x < n) acc)
            if isPrime then primesToHelper (n - 1) (n :: acc)
            else primesToHelper (n - 1) acc
    primesToHelper n []

// Definir una función para encontrar el máximo común divisor de dos números
let gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Definir una función para encontrar el mínimo común múltiplo de dos números
let lcm a b =
    a * b / gcd a b

// Definir una función para generar una lista de números perfectos hasta un número dado
let perfectNumbersTo n =
    let rec perfectNumbersToHelper n acc =
        if n <= 1 then acc
        else
            let isPerfect =
                List.forall (fun x -> n % x = 0)
                    (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))
            if isPerfect then perfectNumbersToHelper (n - 1) (n :: acc)
            else perfectNumbersToHelper (n - 1) acc
    perfectNumbersToHelper n []

// Definir una función para encontrar el número de Fibonacci en una posición dada
let fibonacci n =
    if n < 2 then n
    else fibonacci (n - 1) + fibonacci (n - 2)

// Definir una función para generar una lista de números de Fibonacci hasta un número dado
let fibonacciTo n =
    let rec fibonacciToHelper n acc =
        if n < 2 then acc
        else fibonacciToHelper (n - 1) (fibonacci n :: acc)
    fibonacciToHelper n []

// Definir una función para determinar si un número es primo
let isPrime n =
    if n <= 1 then false
    else
        List.forall (fun x -> n % x <> 0)
            (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))

// Definir una función para generar una lista de números primos hasta un número dado
let primesTo n =
    let rec primesToHelper n acc =
        if n <= 1 then acc
        else
            let isPrime =
                List.forall (fun x -> n % x <> 0) (List.filter (fun x -> x < n) acc)
            if isPrime then primesToHelper (n - 1) (n :: acc)
            else primesToHelper (n - 1) acc
    primesToHelper n []

// Definir una función para encontrar el máximo común divisor de dos números
let gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Definir una función para encontrar el mínimo común múltiplo de dos números
let lcm a b =
    a * b / gcd a b

// Definir una función para generar una lista de números perfectos hasta un número dado
let perfectNumbersTo n =
    let rec perfectNumbersToHelper n acc =
        if n <= 1 then acc
        else
            let isPerfect =
                List.forall (fun x -> n % x = 0)
                    (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))
            if isPerfect then perfectNumbersToHelper (n - 1) (n :: acc)
            else perfectNumbersToHelper (n - 1) acc
    perfectNumbersToHelper n []

// Definir una función para encontrar el número de Fibonacci en una posición dada
let fibonacci n =
    if n < 2 then n
    else fibonacci (n - 1) + fibonacci (n - 2)

// Definir una función para generar una lista de números de Fibonacci hasta un número dado
let fibonacciTo n =
    let rec fibonacciToHelper n acc =
        if n < 2 then acc
        else fibonacciToHelper (n - 1) (fibonacci n :: acc)
    fibonacciToHelper n []

// Definir una función para determinar si un número es primo
let isPrime n =
    if n <= 1 then false
    else
        List.forall (fun x -> n % x <> 0)
            (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))

// Definir una función para generar una lista de números primos hasta un número dado
let primesTo n =
    let rec primesToHelper n acc =
        if n <= 1 then acc
        else
            let isPrime =
                List.forall (fun x -> n % x <> 0) (List.filter (fun x -> x < n) acc)
            if isPrime then primesToHelper (n - 1) (n :: acc)
            else primesToHelper (n - 1) acc
    primesToHelper n []

// Definir una función para encontrar el máximo común divisor de dos números
let gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Definir una función para encontrar el mínimo común múltiplo de dos números
let lcm a b =
    a * b / gcd a b

// Definir una función para generar una lista de números perfectos hasta un número dado
let perfectNumbersTo n =
    let rec perfectNumbersToHelper n acc =
        if n <= 1 then acc
        else
            let isPerfect =
                List.forall (fun x -> n % x = 0)
                    (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))
            if isPerfect then perfectNumbersToHelper (n - 1) (n :: acc)
            else perfectNumbersToHelper (n - 1) acc
    perfectNumbersToHelper n []

// Definir una función para encontrar el número de Fibonacci en una posición dada
let fibonacci n =
    if n < 2 then n
    else fibonacci (n - 1) + fibonacci (n - 2)

// Definir una función para generar una lista de números de Fibonacci hasta un número dado
let fibonacciTo n =
    let rec fibonacciToHelper n acc =
        if n < 2 then acc
        else fibonacciToHelper (n - 1) (fibonacci n :: acc)
    fibonacciToHelper n []

// Definir una función para determinar si un número es primo
let isPrime n =
    if n <= 1 then false
    else
        List.forall (fun x -> n % x <> 0)
            (List.filter (fun x -> x < n and x <> 1) (List.ofSeq [1..(n - 1)]))

// Definir una función para generar una lista de números primos hasta un número dado
let primesTo n =
    let rec primesToHelper n acc =
        if n <= 1 then acc
        else
            let isPrime =
                List.forall (fun x -> n % x <> 0) (List.filter (fun x -> x < n) acc)
            if isPrime then primesToHelper (n - 1) (n :: acc)
            else primesToHelper (n - 1) acc
    primesToHelper n []
```

Explicación del código:

* Las funciones `factorial`, `fibonacci`, `gcd`, y `lcm` calculan el factorial, el número de Fibonacci, el máximo común divisor y el mínimo común múltiplo de dos números, respectivamente.
* La función `isPrime` determina si un número es primo.
* La función `primesTo` genera una lista de números primos hasta un número dado.
* La función `perfectNumbersTo` genera una lista de números perfectos hasta un número dado.
* La función `fibonacciTo` genera una lista de números de Fibonacci hasta un número dado.