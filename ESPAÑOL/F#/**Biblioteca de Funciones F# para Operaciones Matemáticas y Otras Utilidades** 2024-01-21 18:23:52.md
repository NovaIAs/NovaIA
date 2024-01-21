```f#
// Función recursiva que calcula el factorial de un número entero.
let rec factorial n =
    match n with
    | 0 -> 1
    | _ -> n * factorial (n - 1)

// Función que calcula la suma de los cuadrados de los primeros n números naturales.
let sumOfSquares n =
    let rec sumOfSquaresAux n acc =
        match n with
        | 0 -> acc
        | _ -> sumOfSquaresAux (n - 1) (acc + n * n)
    sumOfSquaresAux n 0

// Función que calcula la media aritmética de una lista de números reales.
let mean xs =
    let sum = xs |> List.sum
    let count = xs |> List.length
    sum / count

// Función que ordena una lista de números reales en orden ascendente.
let sort xs =
    xs |> List.sort

// Función que devuelve una lista con los números primos menores o iguales a un número entero dado.
let primes n =
    let rec sieve primes n =
        match n with
        | 0 | 1 -> []
        | _ -> primes @ sieve (primes |> List.filter (fun p -> p * p > n)) (n - 1)
    sieve [] n

// Función que devuelve el mayor divisor común de dos números enteros.
let gcd a b =
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

// Función que devuelve el mínimo común múltiplo de dos números enteros.
let lcm a b =
    a * b / gcd a b

// Función que devuelve un código QR con un mensaje dado.
let qrCode message =
    let qr = QrCode.encode message
    let bitmap = QrCode.draw qr
    bitmap |> GdiBitmap.save "qrcode.png"

// Función que devuelve una lista con los primeros n números de Fibonacci.
let fibonacci n =
    let rec fibonacciAux a b n acc =
        match n with
        | 0 -> acc
        | _ -> fibonacciAux b (a + b) (n - 1) (acc @ [a])
    fibonacciAux 0 1 n []

// Función que devuelve una lista con los primeros n números perfectos.
let perfectNumbers n =
    let rec perfectNumbersAux n acc =
        match n with
        | 0 -> acc
        | _ ->
            let p = primes n |> List.head
            if p * p > n then acc
            else if n % (p - 1) = 0 then perfectNumbersAux (n / p) (acc @ [n])
            else perfectNumbersAux (n - 1) acc
    perfectNumbersAux n []
```

Explicación del código:

* La función `factorial` calcula el factorial de un número entero mediante recursión.
* La función `sumOfSquares` calcula la suma de los cuadrados de los primeros `n` números naturales.
* La función `mean` calcula la media aritmética de una lista de números reales.
* La función `sort` ordena una lista de números reales en orden ascendente.
* La función `primes` devuelve una lista con los números primos menores o iguales a un número entero dado.
* La función `gcd` devuelve el mayor divisor común de dos números enteros.
* La función `lcm` devuelve el mínimo común múltiplo de dos números enteros.
* La función `qrCode` devuelve un código QR con un mensaje dado.
* La función `fibonacci` devuelve una lista con los primeros `n` números de Fibonacci.
* La función `perfectNumbers` devuelve una lista con los primeros `n` números perfectos.