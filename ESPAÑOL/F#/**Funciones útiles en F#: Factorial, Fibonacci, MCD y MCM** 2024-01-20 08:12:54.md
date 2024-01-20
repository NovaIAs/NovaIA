```f#
// Función para calcular el factorial de un número
let factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n - 1)

// Función para calcular la serie de Fibonacci
let fibonacci n =
    if n <= 1 then
        n
    else
        fibonacci (n - 1) + fibonacci (n - 2)

// Función para encontrar el máximo común divisor de dos números
let gcd a b =
    if b = 0 then
        a
    else
        gcd b (a % b)

// Función para encontrar el mínimo común múltiplo de dos números
let lcm a b =
    a * b / gcd a b

// Función para imprimir un saludo personalizado
let greet name =
    printfn "Hola, %s! ¿Cómo estás?" name

// Función principal del programa
let main args =
    // Calcular el factorial de 5
    let factorial5 = factorial 5
    printfn "%d! = %d" 5 factorial5

    // Calcular el número de Fibonacci en la posición 10
    let fibonacci10 = fibonacci 10
    printfn "El número de Fibonacci en la posición 10 es %d" fibonacci10

    // Encontrar el máximo común divisor de 12 y 18
    let gcd1218 = gcd 12 18
    printfn "El máximo común divisor de 12 y 18 es %d" gcd1218

    // Encontrar el mínimo común múltiplo de 9 y 15
    let lcm915 = lcm 9 15
    printfn "El mínimo común múltiplo de 9 y 15 es %d" lcm915

    // Imprimir un saludo personalizado para "Juan"
    greet "Juan"

    // Devolver 0 para indicar que el programa se ejecutó correctamente
    0

// Iniciar el programa
main []
```

Explicación del código:

* La función `factorial` calcula el factorial de un número utilizando la recursividad.
* La función `fibonacci` calcula el número de Fibonacci en una posición determinada utilizando la recursividad.
* La función `gcd` encuentra el máximo común divisor de dos números utilizando el algoritmo de Euclides.
* La función `lcm` encuentra el mínimo común múltiplo de dos números utilizando la fórmula `lcm(a, b) = a * b / gcd(a, b)`.
* La función `greet` imprime un saludo personalizado para un nombre dado.
* La función `main` es la función principal del programa. Llama a las otras funciones para calcular el factorial de 5, el número de Fibonacci en la posición 10, el máximo común divisor de 12 y 18, el mínimo común múltiplo de 9 y 15, e imprime un saludo personalizado para "Juan".

El código está bien documentado y utiliza un estilo de programación funcional. También utiliza algunas características avanzadas de F#, como la recursividad y los tipos genéricos.