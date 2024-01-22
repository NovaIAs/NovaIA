```f#
// Definición de funciones

// Función para encontrar el máximo común divisor (MCD) de dos números
function mcd(x, y) =
    if x = 0 then y
    else mcd(y % x, x)

// Función para encontrar el mínimo común múltiplo (MCM) de dos números
function mcm(x, y) =
    (x * y) / mcd(x, y)

// Función para generar una lista de números primos, hasta un número específico
function primos(n) =
    let rec sieve primes =
        match primes with
        | [] -> [2]
        | x :: xs -> x :: sieve (filter (\p -> p mod x <> 0) xs)
    sieve [2 .. n]

// Función para factorizar un número en sus factores primos
function factoresPrimos(n) =
    let rec factorizar n factores =
        if n = 1 then factores
        else
            let p = List.head (primos (int sqrt n))
            if n mod p = 0 then factorizar (n / p) (p :: factores)
            else factorizar n factores
    factorizar n []

// Función para encontrar el número de divisores de un número
function divisores(n) =
    let factores = factoresPrimos n
    let potencias = List.map (fun f -> factores |> List.count (fun x -> x = f)) factores
    potencias |> List.sum |> (fun x -> x + 1)

// Función para encontrar el suma de los divisores propios de un número
function sumaDivisoresPropios(n) =
    let divisoresPropios = List.filter ((<>) 0) (divisores n)
    divisoresPropios |> List.sum

// Función para encontrar el número perfecto más pequeño mayor que una cantidad específica
function perfectoDesde(n) =
    let rec encontrarPerfecto n =
        if divisores n |> sumaDivisoresPropios = n then n
        else encontrarPerfecto (n + 1)
    encontrarPerfecto (n + 1)

// Uso de las funciones

// Imprimir el MCD y MCM de dos números
let x = 12
let y = 18
printfn "MCD(%d, %d) = %d" x y (mcd x y)
printfn "MCM(%d, %d) = %d" x y (mcm x y)

// Imprimir la lista de números primos hasta 100
printfn "Números primos hasta 100:"
List.iter (printfn "%d") (primos 100)

// Imprimir los factores primos de un número
let n = 12345
printfn "Factores primos de %d:" n
List.iter (printfn "%d") (factoresPrimos n)

// Imprimir el número de divisores de un número
let n = 12345
printfn "Número de divisores de %d:" n
printfn "%d" (divisores n)

// Imprimir la suma de los divisores propios de un número
let n = 12345
printfn "Suma de los divisores propios de %d:" n
printfn "%d" (sumaDivisoresPropios n)

// Imprimir el número perfecto más pequeño mayor que 1000
let n = 1000
printfn "Número perfecto más pequeño mayor que %d:" n
printfn "%d" (perfectoDesde n)

```

Explicación del código:

* Se definen varias funciones, que se explican en sus respectivos comentarios.
* Se utilizan las funciones para calcular el MCD, MCM, los números primos, los factores primos, el número de divisores, la suma de los divisores propios y el número perfecto más pequeño mayor que un número específico.
* Se utiliza la función `printfn` para imprimir los resultados en la consola.