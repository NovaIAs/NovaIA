```f#
// Ejemplo de una solución compleja en F#

// Función para calcular el factorial de un número
let factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)

// Función para calcular la combinación de n elementos tomados de k en k
let combinacion n k =
    factorial n / (factorial k * factorial (n-k))

// Función para calcular el valor de la distribución binomial
// con parámetros n, k y p
let binomial n k p =
    combinacion n k * (p ** k) * ((1.-p) ** (n-k))

// Función para calcular el valor de la distribución normal
// con parámetros media y desviación estándar
let normal media desviacionEstandar x =
    (1. / (sqrt (2. * pi) * desviacionEstandar)) *
    (exp ((-(x - media)**2) / (2. * (desviacionEstandar**2))))

// Programa principal
let main args =
    // Se solicita al usuario que ingrese el valor de n
    printfn "Ingrese el valor de n:"
    let n = int (readLine())

    // Se solicita al usuario que ingrese el valor de k
    printfn "Ingrese el valor de k:"
    let k = int (readLine())

    // Se solicita al usuario que ingrese el valor de p
    printfn "Ingrese el valor de p:"
    let p = float (readLine())

    // Se calcula el valor de la distribución binomial
    let valorBinomial = binomial n k p

    // Se imprime el valor de la distribución binomial
    printfn "El valor de la distribución binomial es: %f" valorBinomial

    // Se solicita al usuario que ingrese el valor de la media
    printfn "Ingrese el valor de la media:"
    let media = float (readLine())

    // Se solicita al usuario que ingrese el valor de la desviación estándar
    printfn "Ingrese el valor de la desviación estándar:"
    let desviacionEstandar = float (readLine())

    // Se solicita al usuario que ingrese el valor de x
    printfn "Ingrese el valor de x:"
    let x = float (readLine())

    // Se calcula el valor de la distribución normal
    let valorNormal = normal media desviacionEstandar x

    // Se imprime el valor de la distribución normal
    printfn "El valor de la distribución normal es: %f" valorNormal

    // Se termina la ejecución del programa
    0

// Se llama a la función principal
main []
```

Este código es un ejemplo de una solución compleja en F#. El código calcula el valor de la distribución binomial y el valor de la distribución normal. El código solicita al usuario que ingrese los valores de los parámetros de las distribuciones y luego calcula y muestra los valores de las distribuciones.

El código está dividido en varias funciones. La función `factorial` calcula el factorial de un número. La función `combinacion` calcula la combinación de n elementos tomados de k en k. La función `binomial` calcula el valor de la distribución binomial con parámetros n, k y p. La función `normal` calcula el valor de la distribución normal con parámetros media y desviación estándar.

El programa principal llama a las funciones `factorial`, `combinacion`, `binomial` y `normal` para calcular los valores de las distribuciones binomial y normal. El programa solicita al usuario que ingrese los valores de los parámetros de las distribuciones y luego calcula y muestra los valores de las distribuciones.