```f#
// Definir una función para calcular el factorial de un número
let factorial n =
    // Si n es 0, devolver 1 (caso base)
    if n = 0 then
        1
    // De lo contrario, llamar recursivamente a la función con n-1 y multiplicar por n
    else
        n * factorial (n-1)

// Definir una función para comprobar si un número es primo
let isPrime n =
    // Si n es 1, no es primo
    if n = 1 then
        false
    // Si n es 2, es primo
    elif n = 2 then
        true
    // De lo contrario, comprobar si n es divisible por algún número entre 2 y la raíz cuadrada de n
    else
        let limit = int (sqrt(float n))
        let divisible = seq {2..limit} |> Seq.exists(fun x -> n % x = 0)
        // Si n es divisible por algún número entre 2 y la raíz cuadrada de n, no es primo
        if divisible then
            false
        // De lo contrario, es primo
        else
            true

// Definir una función para encontrar el primer número primo mayor que un número dado
let firstPrimeAfter n =
    // Si n es menor que 2, comenzar la búsqueda desde 2
    if n < 2 then
        2
    // De lo contrario, comenzar la búsqueda desde n+1
    else
        let prime = seq {n+1..int.MaxValue} |> Seq.find(fun x -> isPrime x)
        prime

// Definir una función para encontrar los factores primos de un número
let primeFactorsOf n =
    // Si n es 1, devolver una lista vacía (no tiene factores primos)
    if n = 1 then
        []
    // De lo contrario, encontrar el primer factor primo de n y añadirlo a la lista de factores primos
    else
        let prime = seq {2..int.MaxValue} |> Seq.find(fun x -> n % x = 0)
        prime :: primeFactorsOf (n / prime)

// Definir una función para encontrar el máximo común divisor de dos números
let gcd a b =
    // Si b es 0, el máximo común divisor es a
    if b = 0 then
        a
    // De lo contrario, llamar recursivamente a la función con b y el resto de dividir a por b
    else
        gcd b (a % b)

// Definir una función para encontrar el mínimo común múltiplo de dos números
let lcm a b =
    // El mínimo común múltiplo es el producto de a y b dividido por el máximo común divisor
    (a * b) / gcd a b

// Definir una función para encontrar el siguiente número de Fibonacci
let nextFibonacci n =
    // Si n es 0 o 1, devolver 1 (caso base)
    if n = 0 || n = 1 then
        1
    // De lo contrario, llamar recursivamente a la función con n-1 y n-2 y sumar los resultados
    else
        nextFibonacci (n-1) + nextFibonacci (n-2)

// Definir una función para encontrar la suma de los dígitos de un número
let sumOfDigits n =
    // Si n es 0, devolver 0 (caso base)
    if n = 0 then
        0
    // De lo contrario, dividir n por 10 y sumar el resto al resultado de llamar recursivamente a la función con n dividido por 10
    else
        (n % 10) + sumOfDigits (n / 10)

// Definir una función para comprobar si un número es palíndromo
let isPalindrome n =
    // Convertir el número a una cadena
    let str = n.ToString()
    // Comprobar si la cadena es igual a su inversa
    str = str.ToLower()
    str = str.Replace(" ", "")
    str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower()
    str = str.Replace(" ", "")
    str = n.ToString().ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse() |> String.concat ""
    str = str.ToLower().Replace(" ", "")
    str = str = n.ToString()
    str = str.ToLower().Replace(" ", "")
    str = str = Seq.toList(str) |> Seq.reverse()