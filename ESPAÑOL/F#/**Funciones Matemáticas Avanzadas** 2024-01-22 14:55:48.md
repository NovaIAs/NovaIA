```f#

// Definición de la función recursiva para calcular el factorial de un número.
let rec factorial (n: int) =
    if n = 0 then 1
    else n * factorial (n-1)

// Definición de la función para calcular el cociente de dos números.
let cociente (x: int) (y: int) =
    if y = 0 then invalid_arg "División por cero"
    else x / y

// Definición de la función para calcular el producto punto de dos vectores.
let productoPunto (x: float[]) (y: float[]) =
    if Array.length x <> Array.length y then invalid_arg "Vectores de longitud diferente"
    else
        Seq.zip x y |> Seq.map (fun (a, b) -> a * b) |> Seq.sum

// Definición de la función para calcular la matriz transpuesta de una matriz.
let transpuesta (m: float[][]) =
    let filas = Array.length m
    let columnas = Array.length (m.[0])
    let resultado = Array.zeroCreate (columnas, filas)
    for i in 0..(filas-1) do
        for j in 0..(columnas-1) do
            resultado.[j].[i] <- m.[i].[j]
    resultado

// Definición de la función para calcular los valores propios de una matriz.
let valoresPropios (m: float[][]) =
    let n = Array.length m
    let a = Array.zeroCreate (n, n)
    let b = Array.zeroCreate (n, n)
    let c = Array.zeroCreate (n, n)
    for i in 0..(n-1) do
        for j in 0..(n-1) do
            a.[i].[j] <- m.[i].[j]
    for i in 0..(n-1) do
        for j in 0..(n-1) do
            b.[i].[j] <- 0.0
            c.[i].[j] <- 0.0
    let eps = 1e-6
    while true do
        // Calcular la matriz H.
        for i in 0..(n-1) do
            for j in 0..(n-1) do
                if i = j then
                    c.[i].[j] <- a.[i].[j]
                else
                    c.[i].[j] <- 0.0
        // Calcular la matriz Q.
        for i in 0..(n-1) do
            for j in 0..(n-1) do
                b.[i].[j] <- 0.0
                for k in 0..(n-1) do
                    b.[i].[j] <- b.[i].[j] + a.[i].[k] * c.[k].[j]
        // Calcular la matriz R.
        for i in 0..(n-1) do
            for j in 0..(n-1) do
                a.[i].[j] <- 0.0
                for k in 0..(n-1) do
                    a.[i].[j] <- a.[i].[j] + b.[i].[k] * c.[k].[j]
        // Comprobar si la matriz H ha convergido.
        let converged = true
        for i in 0..(n-1) do
            for j in 0..(n-1) do
                if abs (c.[i].[j] - h.[i].[j]) > eps then
                    converged <- false
        if converged then
            break

    // Extraer los valores propios de la matriz H.
    let valoresPropios = Array.zeroCreate n
    for i in 0..(n-1) do
        valoresPropios.[i] <- h.[i].[i]
    valoresPropios

// Definición de la función para calcular los vectores propios de una matriz.
let vectoresPropios (m: float[][]) =
    let n = Array.length m
    let valoresPropios = valoresPropios m
    let vectoresPropios = Array.zeroCreate (n, n)
    for i in 0..(n-1) do
        // Calcular el vector propio correspondiente al valor propio `valoresPropios.[i]`.
        let v = Array.zeroCreate n
        for j in 0..(n-1) do
            v.[j] <- 0.0
        v.[i] <- 1.0

        // Resolver el sistema de ecuaciones (A - valoresPropios.[i] * I) * v = 0.
        let a = Array.zeroCreate (n, n)
        for j in 0..(n-1) do
            for k in 0..(n-1) do
                a.[j].[k] <- m.[j].[k] - valoresPropios.[i] * (if j = k then 1.0 else 0.0)

        // Obtener el vector propio.
        let v = resolverSistemaEcuaciones a v
        // Normalizar el vector propio.
        let norma = sqrt (productoPunto v v)
        for j in 0..(n-1) do
            v.[j] <- v.[j] / norma

        // Almacenar el vector propio en la matriz de vectores propios.
        for j in 0..(n-1) do
            vectoresPropios.[j].[i] <- v.[j]
    vectoresPropios

// Definición de la función para resolver un sistema de ecuaciones lineales.
let resolverSistemaEcuaciones (a: float[][]) (b: float[]) =
    // Comprobar que la matriz A es cuadrada.
    if Array.length a <> Array.length b then invalid_arg "La matriz A no es cuadrada"

    // Comprobar que la matriz A es invertible.
    let det = determinant a
    if det = 0.0 then invalid_arg "La matriz A es singular"

    // Calcular la matriz adjunta de A.
    let adj = adjunta a

    // Calcular la inversa de A.
    let inv = inversa a

    // Calcular la solución del sistema de ecuaciones.
    let sol = inv |> Array.map (fun v -> productoPunto v b)

    sol

// Definición de la función para calcular el determinante de una matriz.
let determinant (m: float[][]) =
    let n = Array.length m
    if n = 1 then
        m.[0].[0]
    else
        let det = 0.0
        for i in 0..(n-1) do
            let submatriz = Array.zeroCreate (n-1, n-1)
            for j in 1..(n-1) do
                for k in 0..(n-1) do
                    if k < i then
                        submatriz.[j-1].[k] <- m.[j].[k]
                    else if k > i then
                        submatriz.[j-1].[k-1] <- m.[j].[k]

            det <- det + (-1.0)**i * m.[0].[i] * determinant submatriz
        det

// Definición de la función para calcular la matriz adjunta de una matriz.
let adjunta (m: float[][]) =
    let n = Array.length m
    let adj = Array.zeroCreate (n, n)
    for i in 0..(n-1) do
        for j in 0..(n-1) do
            let submatriz = Array.zeroCreate (n-1, n-1)
            for k in 0..(n-1) do
                if k <> i then
                    for l in 0..(n-1) do
                        if l <> j then
                            submatriz.[k-if k > i then 1 else 0].[l-if l > j then 1 else 0] <- m.[k].[l]

            adj.[j].[i] <- (-1.0)**(i + j) * determinant submatriz
    adj

// Definición de la función para calcular la inversa de una matriz.
let inversa (m: float[][]) =
    let n = Array.length m
    let adj = adjunta m
    let det = determinant m
    if det = 0.0 then invalid_arg "La matriz no es invertible"
    let inv = Array.zeroCreate (n, n)
    for i in 0..(n-1) do
        for j in 0..(n-1) do
            inv.[i].[j] <- adj.[i].[j] / det
    inv

```