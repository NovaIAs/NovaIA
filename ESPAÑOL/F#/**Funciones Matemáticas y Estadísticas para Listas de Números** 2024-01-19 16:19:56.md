```f#
// Definir una función para calcular el factorial de un número
let factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial (n - 1)

// Definir una función para generar una lista de números primos hasta un límite dado
let primeNumbersTo n =
    // Utilizar la función interna 'seq' para generar una secuencia de números de 2 a n
    seq { for i in 2 .. n do yield i }
    // Filtrar la secuencia para incluir sólo los números primos usando la función interna 'filter'
    |> Seq.filter (fun x ->
        // Comprobar si el número es primo usando la función interna 'Seq.forall' para verificar si todos sus divisores son mayores que 1
        Seq.forall (fun y -> x mod y > 1) { for y in 2 .. (int(Math.sqrt(float x))) }
    )
    // Convertir la secuencia en una lista usando la función interna 'toList'
    |> Seq.toList

// Definir una función para calcular la suma de los dígitos de un número
let sumDigits n =
    // Utilizar la función interna 'toString' para convertir el número en una cadena
    n.ToString()
    // Utilizar la función interna 'Seq.map' para convertir cada carácter de la cadena en un entero
    |> Seq.map int
    // Sumar los enteros usando la función interna 'Seq.sum'
    |> Seq.sum

// Definir una función para encontrar el número más grande en una lista de números
let maxNumber list =
    // Utilizar la función interna 'Seq.max' para encontrar el número más grande en la lista
    Seq.max list

// Definir una función para encontrar el número más pequeño en una lista de números
let minNumber list =
    // Utilizar la función interna 'Seq.min' para encontrar el número más pequeño en la lista
    Seq.min list

// Definir una función para encontrar el número medio de una lista de números
let averageNumber list =
    // Utilizar la función interna 'Seq.average' para encontrar el número medio de la lista
    Seq.average list

// Definir una función para encontrar la mediana de una lista de números
let medianNumber list =
    // Ordenar la lista en orden ascendente usando la función interna 'Seq.sort'
    list |> Seq.sort
    // Obtener el índice del elemento medio de la lista
    |> Seq.length / 2
    // Obtener el elemento medio de la lista usando el índice
    |> Seq.nth

// Definir una función para encontrar la moda de una lista de números
let modeNumber list =
    // Agrupar los elementos de la lista por su valor usando la función interna 'Seq.groupBy'
    list |> Seq.groupBy id
    // Convertir los grupos en pares clave-valor usando la función interna 'Seq.map'
    |> Seq.map (fun (key, value) -> (key, value.Count))
    // Encontrar el par con el valor más alto usando la función interna 'Seq.maxBy'
    |> Seq.maxBy snd
    // Obtener la clave del par con el valor más alto
    |> fst

// Definir una función para encontrar la varianza de una lista de números
let varianceNumber list =
    // Calcular la media de la lista usando la función 'averageNumber'
    let average = averageNumber list
    // Utilizar la función interna 'Seq.map' para calcular la desviación de cada elemento de la lista respecto de la media
    list |> Seq.map (fun x -> (x - average) ** 2)
    // Sumar las desviaciones cuadradas usando la función interna 'Seq.sum'
    |> Seq.sum
    // Dividir la suma de las desviaciones cuadradas por el número de elementos de la lista
    |> (/) (float list.Length)

// Definir una función para encontrar la desviación estándar de una lista de números
let standardDeviationNumber list =
    // Calcular la varianza de la lista usando la función 'varianceNumber'
    let variance = varianceNumber list
    // Tomar la raíz cuadrada de la varianza
    Math.Sqrt(variance)

// Definir una función para encontrar la correlación entre dos listas de números
let correlationNumber list1 list2 =
    // Calcular la media de las dos listas usando la función 'averageNumber'
    let average1 = averageNumber list1
    let average2 = averageNumber list2
    // Utilizar la función interna 'Seq.zip' para crear una secuencia de pares de elementos de las dos listas
    list1 |> Seq.zip list2
    // Calcular la desviación de cada par de elementos respecto de sus medias correspondientes
    |> Seq.map (fun (x, y) -> ((x - average1) * (y - average2)))
    // Sumar las desviaciones de los pares de elementos usando la función interna 'Seq.sum'
    |> Seq.sum
    // Dividir la suma de las desviaciones por el producto de las desviaciones estándar de las dos listas
    |> (/) (standardDeviationNumber list1 * standardDeviationNumber list2)

// Definir una función para encontrar la regresión lineal entre dos listas de números
let linearRegressionNumber list1 list2 =
    // Calcular la correlación entre las dos listas usando la función 'correlationNumber'
    let correlation = correlationNumber list1 list2
    // Calcular la media de las dos listas usando la función 'averageNumber'
    let average1 = averageNumber list1
    let average2 = averageNumber list2
    // Calcular la pendiente de la recta de regresión lineal
    let slope = correlation * standardDeviationNumber list1 / standardDeviationNumber list2
    // Calcular la ordenada en el origen de la recta de regresión lineal
    let intercept = average1 - (slope * average2)
    // Devolver la pendiente y la ordenada en el origen como una tupla
    (slope, intercept)

// Definir una función para encontrar la ecuación de la recta de regresión lineal entre dos listas de números
let linearRegressionEquationNumber list1 list2 =
    // Calcular la pendiente y la ordenada en el origen de la recta de regresión lineal usando la función 'linearRegressionNumber'
    let (slope, intercept) = linearRegressionNumber list1 list2
    // Formatear la ecuación de la recta de regresión lineal como una cadena
    sprintf "y = %.2fx + %.2f", slope, intercept

// Definir una función para encontrar la curva de regresión polinomial entre dos listas de números
let polynomialRegressionNumber list1 list2 =
    // Calcular el grado del polinomio de regresión
    let degree = 2
    // Crear una matriz de diseño de Vandermonde
    let designMatrix =
        // Crear una secuencia de potencias de 0 a 'degree' para cada elemento de la lista1
        list1 |> Seq.map (fun x -> Seq.init (degree + 1) (fun i -> x ** i))
        // Convertir la secuencia de secuencias en una matriz
        |> Array.ofSeq
    // Crear un vector de valores objetivo
    let targetVector =
        // Convertir la lista2 en un vector
        Array.ofList list2
    // Calcular los coeficientes del polinomio de regresión
    let coefficients =
        // Utilizar la función interna 'linalg' para calcular la pseudoinversa de la matriz de diseño
        let pseudoInverse = linalg.pinv designMatrix
        // Multiplicar la pseudoinversa por el vector de valores objetivo
        pseudoInverse * targetVector
    // Devolver los coeficientes del polinomio de regresión
    coefficients

// Definir una función para encontrar la ecuación de la curva de regresión polinomial entre dos listas de números
let polynomialRegressionEquationNumber list1 list2 =
    // Calcular los coeficientes del polinomio de regresión usando la función 'polynomialRegressionNumber'
    let coefficients = polynomialRegressionNumber list1 list2
    // Formatear la ecuación de la curva de regresión polinomial como una cadena
    let equation =
        sprintf "y = %.2fx^2 + %.2fx + %.2f", coefficients.[2], coefficients.[1], coefficients.[0]
    // Devolver la ecuación de la curva de regresión polinomial
    equation

// Definir una función para encontrar la mejor ecuación de regresión entre dos listas de números
let bestRegressionEquationNumber list1 list2 =
    // Calcular la ecuación de la recta de regresión lineal y de la curva de regresión polinomial
    let linearEquation = linearRegressionEquationNumber list1 list2
    let polynomialEquation = polynomialRegressionEquationNumber list1 list2
    // Calcular el coeficiente de determinación de la recta de regresión lineal y de la curva de regresión polinomial
    let linearR2 = 1.0 - (varianceNumber list2 / varianceNumber (polynomialRegressionNumber list1 list2))
    let polynomialR2 = 1.0 - (varianceNumber list2 / varianceNumber (polynomialRegressionNumber list1 list2))
    // Devolver la ecuación de regresión con el coeficiente de determinación más alto
    if linearR2 > polynomialR2 then linearEquation else polynomialEquation

// Probar las funciones definidas
list1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
list2 = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

printfn "Suma de los dígitos: %d" (sumDigits 12345)
printfn "Número máximo: %d" (maxNumber list1)
printfn "Número mínimo: %d" (minNumber list1)
printfn "Número medio: %f" (averageNumber list1)
printfn "Mediana: %d" (medianNumber list1)
printfn "Moda: %d" (modeNumber list1)
printfn "Varianza: %f" (varianceNumber list1)
printfn "Desviación estándar: %f" (standardDeviationNumber list1)
printfn "Correlación: %f" (correlationNumber list1 list2)
printfn "Ecuación de regresión lineal: %s" (linearRegressionEquationNumber list1 list2)
printfn "Ecuación de regresión polinomial: %s" (polynomialRegressionEquationNumber list1 list2)
printfn "Mejor ecuación de regresión: %s" (bestRegressionEquationNumber list1 list2)
```

Este código realiza una serie de operaciones matemáticas y estadísticas sobre dos listas de números.

* Calcula la suma de los dígitos de un número.
* Encuentra el número máximo y mínimo de una lista.
* Calcula la media, la mediana y la moda de una lista.
* Calcula la varianza y la desviación estándar de una lista.
* Encuentra la correlación entre dos listas.
* Calcula la ecuación de la recta de regresión lineal y de la curva de regresión polinomial entre dos listas.
* Encuentra la mejor ecuación de regresión entre dos listas.

El código está dividido en varias funciones, cada una de las cuales realiza una operación específica. Las funciones son utilizadas por la función `main` para probar las operaciones matemáticas y estadísticas sobre las dos listas de números.

El código es bastante complejo, pero está bien documentado y organizado, lo que lo hace fácil de entender y modificar.