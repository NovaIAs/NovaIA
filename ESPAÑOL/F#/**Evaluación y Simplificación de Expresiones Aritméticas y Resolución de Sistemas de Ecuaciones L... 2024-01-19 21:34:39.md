```f#
// Definimos un tipo de datos algebraico para representar expresiones aritméticas.

type Expr =
    | Constante de float
    | Variable de string
    | Suma de Expr * Expr
    | Resta de Expr * Expr
    | Multiplicacion de Expr * Expr
    | Division de Expr * Expr
    | Exponenciacion de Expr * Expr

// Creamos una función que evalúa una expresión aritmética dada.

let evaluarExpr (expr: Expr): float =
    match expr with
        | Constante x -> x
        | Variable var -> raise (InvalidOperationException("Variable no definida"))
        | Suma (x, y) -> evaluarExpr x + evaluarExpr y
        | Resta (x, y) -> evaluarExpr x - evaluarExpr y
        | Multiplicacion (x, y) -> evaluarExpr x * evaluarExpr y
        | Division (x, y) -> evaluarExpr x / evaluarExpr y
        | Exponenciacion (x, y) -> pow (evaluarExpr x) (evaluarExpr y)

// Creamos una función que simplifica una expresión aritmética dada.

let simplificarExpr (expr: Expr): Expr =
    match expr with
        | Constante x -> Constante x
        | Variable var -> Variable var
        | Suma (x, y) -> Suma (simplificarExpr x, simplificarExpr y)
        | Resta (x, y) -> Resta (simplificarExpr x, simplificarExpr y)
        | Multiplicacion (x, y) -> Multiplicacion (simplificarExpr x, simplificarExpr y)
        | Division (x, y) -> Division (simplificarExpr x, simplificarExpr y)
        | Exponenciacion (x, y) -> Exponenciacion (simplificarExpr x, simplificarExpr y)

// Creamos un tipo de datos para representar un sistema de ecuaciones lineales.

type SistemaEcuaciones =
    { ecuaciones: Expr list }

// Creamos una función que resuelve un sistema de ecuaciones lineales dado.

let resolverSistemaEcuaciones (sistema: SistemaEcuaciones): float list =
    // Obtenemos las variables del sistema.
    let variables = (List.map (fun eq -> List.filter (fun v -> v = Variable "x") eq) sistema.ecuaciones)
        |> List.flatten

    // Creamos una matriz con los coeficientes de las variables.
    let matrizCoeficientes =
        List.map (fun eq -> List.map (fun v -> if v = eq then 1.0 else 0.0) variables) sistema.ecuaciones

    // Creamos un vector con los términos independientes de las ecuaciones.
    let vectorTerminosIndependientes = List.map (fun eq -> evaluarExpr (List.filter (fun v -> v <> Variable "x") eq)) sistema.ecuaciones

    // Resolvemos el sistema de ecuaciones lineales.
    let solucion = GaussianElimination.GaussianElimination.solve (matrizCoeficientes, vectorTerminosIndependientes)

    // Devolvemos la solución del sistema de ecuaciones lineales.
    solucion

// Creamos un sistema de ecuaciones lineales de ejemplo.

let sistemaEjemplo =
    { ecuaciones = [
        Suma (Variable "x", Constante 2.0);
        Suma (Variable "y", Constante 3.0);
        Suma (Variable "z", Constante 4.0);
    ] }

// Resolvemos el sistema de ecuaciones lineales de ejemplo.

let solucionEjemplo = resolverSistemaEcuaciones sistemaEjemplo

// Imprimimos la solución del sistema de ecuaciones lineales de ejemplo.

printfn "Solución del sistema de ecuaciones lineales de ejemplo: %A" solucionEjemplo
```

Explicación:

* El código define un tipo de datos algebraico para representar expresiones aritméticas.
* El código define una función que evalúa una expresión aritmética dada.
* El código define una función que simplifica una expresión aritmética dada.
* El código define un tipo de datos para representar un sistema de ecuaciones lineales.
* El código define una función que resuelve un sistema de ecuaciones lineales dado.
* El código crea un sistema de ecuaciones lineales de ejemplo.
* El código resuelve el sistema de ecuaciones lineales de ejemplo.
* El código imprime la solución del sistema de ecuaciones lineales de ejemplo.