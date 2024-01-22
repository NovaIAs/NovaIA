```f#

// Definición de la función factorial

let factorial n =
    if n < 0 then failwith "El número debe ser no negativo"
    else if n = 0 then 1
    else n * factorial (n-1)

// Definición de la función que calcula el número de combinaciones de n elementos tomados de k en k

let combinaciones n k =
    if k < 0 || k > n then failwith "Los valores de n y k deben ser no negativos y k <= n"
    else factorial n / (factorial k * factorial (n-k))

// Definición de la función que calcula el número de permutaciones de n elementos tomados de k en k

let permutaciones n k =
    if k < 0 || k > n then failwith "Los valores de n y k deben ser no negativos y k <= n"
    else factorial n / factorial (n-k)

// Ejemplo de uso de las funciones

let resultadoFactorial = factorial 5
printfn "El factorial de 5 es %d" resultadoFactorial

let resultadoCombinaciones = combinaciones 10 4
printfn "El número de combinaciones de 10 elementos tomados de 4 en 4 es %d" resultadoCombinaciones

let resultadoPermutaciones = permutaciones 10 4
printfn "El número de permutaciones de 10 elementos tomados de 4 en 4 es %d" resultadoPermutaciones

```

Explicación del código:

* La función `factorial` calcula el factorial de un número. El factorial de un número entero $n$ se define como el producto de todos los números enteros positivos desde 1 hasta $n$. Por ejemplo, $5! = 5 * 4 * 3 * 2 * 1 = 120$.
* La función `combinaciones` calcula el número de combinaciones de n elementos tomados de k en k. Una combinación es un subconjunto de elementos de un conjunto sin tener en cuenta el orden de los elementos. Por ejemplo, el número de combinaciones de 5 elementos tomados de 3 en 3 es 10.
* La función `permutaciones` calcula el número de permutaciones de n elementos tomados de k en k. Una permutación es una disposición de los elementos de un conjunto en un orden determinado. Por ejemplo, el número de permutaciones de 5 elementos tomados de 3 en 3 es 60.
* El ejemplo de uso del código muestra cómo utilizar las funciones `factorial`, `combinaciones` y `permutaciones` para calcular el factorial de 5, el número de combinaciones de 10 elementos tomados de 4 en 4 y el número de permutaciones de 10 elementos tomados de 4 en 4.