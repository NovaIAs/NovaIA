```f#
// Definimos un tipo para representar una matriz de números enteros
type Matriz = int[][]

// Definimos una función para crear una matriz de números enteros
let crearMatriz (filas: int) (columnas: int) =
    let matriz = Array.init filas (fun _ -> Array.init columnas (fun _ -> 0))
    matriz

// Definimos una función para sumar dos matrices de números enteros
let sumarMatrices (matriz1: Matriz) (matriz2: Matriz) =
    let filas = matriz1.Length
    let columnas = matriz1.[0].Length
    let matrizSuma = crearMatriz filas columnas
    for i in 0 .. filas - 1 do
        for j in 0 .. columnas - 1 do
            matrizSuma.[i].[j] <- matriz1.[i].[j] + matriz2.[i].[j]
    matrizSuma

// Definimos una función para multiplicar dos matrices de números enteros
let multiplicarMatrices (matriz1: Matriz) (matriz2: Matriz) =
    let filas1 = matriz1.Length
    let columnas1 = matriz1.[0].Length
    let filas2 = matriz2.Length
    let columnas2 = matriz2.[0].Length
    if columnas1 <> filas2 then failwith "Las matrices no se pueden multiplicar"
    let matrizProducto = crearMatriz filas1 columnas2
    for i in 0 .. filas1 - 1 do
        for j in 0 .. columnas2 - 1 do
            for k in 0 .. columnas1 - 1 do
                matrizProducto.[i].[j] <- matrizProducto.[i].[j] + matriz1.[i].[k] * matriz2.[k].[j]
    matrizProducto

// Definimos una función para calcular la transpuesta de una matriz de números enteros
let transpuestaMatriz (matriz: Matriz) =
    let filas = matriz.Length
    let columnas = matriz.[0].Length
    let matrizTranspuesta = crearMatriz columnas filas
    for i in 0 .. filas - 1 do
        for j in 0 .. columnas - 1 do
            matrizTranspuesta.[j].[i] <- matriz.[i].[j]
    matrizTranspuesta

// Definimos una función para calcular la determinante de una matriz de números enteros
let determinanteMatriz (matriz: Matriz) =
    let filas = matriz.Length
    let columnas = matriz.[0].Length
    if filas <> columnas then failwith "La matriz no es cuadrada"
    if filas = 1 then matriz.[0].[0]
    if filas = 2 then matriz.[0].[0] * matriz.[1].[1] - matriz.[0].[1] * matriz.[1].[0]
    let determinante = 0
    for i in 0 .. columnas - 1 do
        let submatriz = Array.init (filas - 1) (fun _ -> Array.init (columnas - 1) (fun _ -> 0))
        for j in 1 .. filas - 1 do
            for k in 0 .. columnas - 1 do
                if k <> i then
                    submatriz.[j - 1].[k] <- matriz.[j].[k]
        let cofactor = (-1) ** i * matriz.[0].[i] * determinanteMatriz submatriz
        determinante <- determinante + cofactor
    determinante

// Definimos una función para calcular la inversa de una matriz de números enteros
let inversaMatriz (matriz: Matriz) =
    let filas = matriz.Length
    let columnas = matriz.[0].Length
    if filas <> columnas then failwith "La matriz no es cuadrada"
    let determinante = determinanteMatriz matriz
    if determinante = 0 then failwith "La matriz no tiene inversa"
    let matrizInversa = crearMatriz filas columnas
    for i in 0 .. filas - 1 do
        for j in 0 .. columnas - 1 do
            let submatriz = Array.init (filas - 1) (fun _ -> Array.init (columnas - 1) (fun _ -> 0))
            for k in 0 .. filas - 1 do
                if k <> i then
                    for l in 0 .. columnas - 1 do
                        if l <> j then
                            submatriz.[k - 1].[l] <- matriz.[k].[l]
            let cofactor = (-1) ** (i + j) * determinanteMatriz submatriz
            matrizInversa.[i].[j] <- cofactor / determinante
    matrizInversa

// Definimos una función para imprimir una matriz de números enteros
let imprimirMatriz (matriz: Matriz) =
    for fila in matriz do
        for columna in fila do
            printf "%d " columna
        printfn ""

// Creamos una matriz de números enteros
let matriz1 = crearMatriz 3 3
matriz1.[0].[0] <- 1
matriz1.[0].[1] <- 2
matriz1.[0].[2] <- 3
matriz1.[1].[0] <- 4
matriz1.[1].[1] <- 5
matriz1.[1].[2] <- 6
matriz1.[2].[0] <- 7
matriz1.[2