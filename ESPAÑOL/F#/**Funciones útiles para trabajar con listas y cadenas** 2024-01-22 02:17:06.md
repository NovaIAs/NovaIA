```f#
// Función que recibe una lista de números y devuelve la suma de los pares.
let sumaPares (numeros: int list) =
    numeros |> List.filter (fun n -> n % 2 = 0) |> List.sum

// Función que recibe una cadena de caracteres y devuelve sus letras en orden inverso.
let invertirCadena (cadena: string) =
    cadena |> Seq.toList |> List.reverse |> String.concat ""

// Función que recibe un número y devuelve su factorial.
let factorial (numero: int) =
    let rec fact n =
        if n = 0 then 1
        else n * fact (n - 1)
    fact numero

// Función que recibe una lista de listas de números y devuelve una lista con todos los elementos de todas las listas.
let concatenarListas (listas: int list list) =
    listas |> List.concat

// Función que recibe un número y devuelve una lista con sus dígitos.
let digitos (numero: int) =
    let rec digits n =
        if n = 0 then []
        else n % 10 :: digits (n / 10)
    digits numero

// Función que recibe una lista de números y devuelve su desviación estándar.
let desviacionEstandar (numeros: int list) =
    let media = numeros |> List.average
    let varianza = numeros |> List.map (fun n -> (n - media) ** 2) |> List.average
    Math.Sqrt varianza

// Función que recibe una lista de números y devuelve el número más grande.
let numeroMasGrande (numeros: int list) =
    numeros |> List.max

// Función que recibe una lista de números y devuelve el número más pequeño.
let numeroMasPequeño (numeros: int list) =
    numeros |> List.min

// Función que recibe una lista de números y devuelve su media.
let media (numeros: int list) =
    numeros |> List.average

// Función que recibe una lista de números y devuelve su mediana.
let mediana (numeros: int list) =
    let sortedNumbers = numeros |> List.sort
    if sortedNumbers.Length % 2 = 0 then
        (sortedNumbers.[sortedNumbers.Length / 2 - 1] + sortedNumbers.[sortedNumbers.Length / 2]) / 2.0
    else
        sortedNumbers.[sortedNumbers.Length / 2]
```

Este código contiene una variedad de funciones útiles para trabajar con listas de números y cadenas de caracteres. Las funciones incluyen:

* `sumaPares`: Suma los números pares de una lista.
* `invertirCadena`: Invierte el orden de los caracteres de una cadena.
* `factorial`: Calcula el factorial de un número.
* `concatenarListas`: Concatena una lista de listas en una sola lista.
* `digitos`: Obtiene los dígitos de un número como una lista.
* `desviacionEstandar`: Calcula la desviación estándar de una lista de números.
* `numeroMasGrande`: Encuentra el número más grande en una lista.
* `numeroMasPequeño`: Encuentra el número más pequeño en una lista.
* `media`: Calcula la media de una lista de números.
* `mediana`: Calcula la mediana de una lista de números.