```f#
// Importamos las librerías necesarias
open System
open System.Collections.Generic
open System.Linq

// Definimos una función para generar una lista de números primos hasta un número dado
let primes n =
  // Creamos una lista de números desde 2 hasta n
  let numbers = List.range 2 n
  // Filtramos la lista para obtener solo los números primos
  numbers |> List.filter (fun x ->
    // Comprobamos si el número es divisible por algún número entre 2 y su raíz cuadrada
    let limit = int (Math.sqrt (float x))
    List.forall (fun y -> x % y <> 0) (List.range 2 limit))

// Definimos una función para convertir un número decimal a binario
let toBinary n =
  // Creamos una lista vacía para almacenar los dígitos binarios
  let binary = []
  // Mientras el número sea mayor que 0, añadimos el dígito binario menos significativo a la lista
  while n > 0 do
    binary <- (n % 2) :: binary
    n <- n / 2
  // Invertimos la lista para obtener el número binario correcto
  binary |> List.rev

// Definimos una función para calcular el factorial de un número
let factorial n =
  // Si el número es 0 o 1, devolvemos 1
  if n = 0 || n = 1 then 1
  // En caso contrario, multiplicamos el número por el factorial del número anterior
  else n * factorial (n - 1)

// Definimos una función para calcular el máximo común divisor de dos números
let gcd a b =
  // Si b es 0, devolvemos a
  if b = 0 then a
  // En caso contrario, llamamos recursivamente a la función con b y el resto de la división de a entre b
  else gcd b (a % b)

// Definimos una función para calcular el mínimo común múltiplo de dos números
let lcm a b =
  // Calculamos el máximo común divisor de a y b
  let divisor = gcd a b
  // Devolvemos el producto de a y b dividido por el máximo común divisor
  (a * b) / divisor

// Definimos una función para calcular la suma de los dígitos de un número
let sumOfDigits n =
  // Convertimos el número a una lista de dígitos
  let digits = List.map (fun x -> x - '0') (toBinary n)
  // Sumamos los dígitos
  digits |> List.sum

// Definimos una función para calcular la media de una lista de números
let mean xs =
  // Si la lista está vacía, devolvemos 0
  if xs.Length = 0 then 0.0
  // En caso contrario, sumamos todos los números de la lista y dividimos por el número de elementos
  else xs |> List.sum / float xs.Length

// Definimos una función para calcular la desviación estándar de una lista de números
let standardDeviation xs =
  // Calculamos la media de la lista
  let meanValue = mean xs
  // Calculamos la suma de las desviaciones cuadradas de la media
  let sumOfSquaredDeviations =
    xs |> List.map (fun x -> (x - meanValue) ** 2) |> List.sum
  // Dividimos la suma de las desviaciones cuadradas por el número de elementos y calculamos la raíz cuadrada
  Math.sqrt (sumOfSquaredDeviations / float xs.Length)

// Creamos una lista de números
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Imprimimos los números primos hasta 10
printfn "Números primos hasta 10: %A" (primes 10)

// Imprimimos la conversión binaria de 10
printfn "Conversión binaria de 10: %A" (toBinary 10)

// Imprimimos el factorial de 10
printfn "Factorial de 10: %i" (factorial 10)

// Imprimimos el máximo común divisor de 12 y 18
printfn "Máximo común divisor de 12 y 18: %i" (gcd 12 18)

// Imprimimos el mínimo común múltiplo de 12 y 18
printfn "Mínimo común múltiplo de 12 y 18: %i" (lcm 12 18)

// Imprimimos la suma de los dígitos de 1024
printfn "Suma de los dígitos de 1024: %i" (sumOfDigits 1024)

// Imprimimos la media de la lista de números
printfn "Media de la lista de números: %f" (mean numbers)

// Imprimimos la desviación estándar de la lista de números
printfn "Desviación estándar de la lista de números: %f" (standardDeviation numbers)
```

Este código realiza una serie de operaciones matemáticas complejas utilizando varias funciones definidas por el usuario. Estas operaciones incluyen la generación de números primos, la conversión de números decimales a binarios, el cálculo del factorial de un número, el cálculo del máximo común divisor y del mínimo común múltiplo de dos números, el cálculo de la suma de los dígitos de un número, el cálculo de la media y la desviación estándar de una lista de números.

El código también utiliza la función `printfn` para imprimir los resultados de las operaciones en la consola.