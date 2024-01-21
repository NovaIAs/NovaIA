```f#

// Definimos un tipo de datos algebraico para representar las operaciones aritméticas básicas.
type Operacion =
    | Suma
    | Resta
    | Multiplicacion
    | Division

// Definimos un tipo de datos algebraico para representar los operandos de las operaciones aritméticas.
type Operando =
    | NumeroReal de float
    | NumeroComplejo de complex

// Definimos una función para crear una expresión aritmética a partir de una operación y dos operandos.
let crearExpresion (operacion: Operacion) (operando1: Operando) (operando2: Operando) =
    match operacion with
    | Suma -> (operando1, operando2)
    | Resta -> (operando1, operando2)
    | Multiplicacion -> (operando1, operando2)
    | Division -> (operando1, operando2)

// Definimos una función para evaluar una expresión aritmética.
let evaluarExpresion (expresion: (Operando, Operando)) =
    let (operando1, operando2) = expresion
    match operando1, operando2 with
    | NumeroReal x, NumeroReal y ->
        match expresion with
        | Suma -> x + y
        | Resta -> x - y
        | Multiplicacion -> x * y
        | Division -> x / y
    | NumeroReal x, NumeroComplejo y ->
        match expresion with
        | Suma -> x + y
        | Resta -> x - y
        | Multiplicacion -> x * y
        | Division -> x / y
    | NumeroComplejo x, NumeroReal y ->
        match expresion with
        | Suma -> x + y
        | Resta -> x - y
        | Multiplicacion -> x * y
        | Division -> x / y
    | NumeroComplejo x, NumeroComplejo y ->
        match expresion with
        | Suma -> x + y
        | Resta -> x - y
        | Multiplicacion -> x * y
        | Division -> x / y

// Definimos una función para imprimir una expresión aritmética.
let imprimirExpresion (expresion: (Operando, Operando)) =
    let (operando1, operando2) = expresion
    match operando1, operando2 with
    | NumeroReal x, NumeroReal y ->
        match expresion with
        | Suma -> printf "%f + %f" x y
        | Resta -> printf "%f - %f" x y
        | Multiplicacion -> printf "%f * %f" x y
        | Division -> printf "%f / %f" x y
    | NumeroReal x, NumeroComplejo y ->
        match expresion with
        | Suma -> printf "%f + (%f, %f)" x (real y) (imag y)
        | Resta -> printf "%f - (%f, %f)" x (real y) (imag y)
        | Multiplicacion -> printf "%f * (%f, %f)" x (real y) (imag y)
        | Division -> printf "%f / (%f, %f)" x (real y) (imag y)
    | NumeroComplejo x, NumeroReal y ->
        match expresion with
        | Suma -> printf "(%f, %f) + %f" (real x) (imag x) y
        | Resta -> printf "(%f, %f) - %f" (real x) (imag x) y
        | Multiplicacion -> printf "(%f, %f) * %f" (real x) (imag x) y
        | Division -> printf "(%f, %f) / %f" (real x) (imag x) y
    | NumeroComplejo x, NumeroComplejo y ->
        match expresion with
        | Suma -> printf "(%f, %f) + (%f, %f)" (real x) (imag x) (real y) (imag y)
        | Resta -> printf "(%f, %f) - (%f, %f)" (real x) (imag x) (real y) (imag y)
        | Multiplicacion -> printf "(%f, %f) * (%f, %f)" (real x) (imag x) (real y) (imag y)
        | Division -> printf "(%f, %f) / (%f, %f)" (real x) (imag x) (real y) (imag y)

// Ejemplo de uso

let expresion = crearExpresion Suma (NumeroReal 2.5) (NumeroComplejo (3.0, 4.0))

imprimirExpresion expresion

printfn "%f" (evaluarExpresion expresion)

```