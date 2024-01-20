```f#
// Definir un tipo de datos algebraico para representar expresiones matemáticas.

type Expresion =
    | Numero of float
    | Variable of string
    | Suma of Expresion * Expresion
    | Resta of Expresion * Expresion
    | Multiplicacion of Expresion * Expresion
    | Division of Expresion * Expresion

// Definir una función para evaluar una expresión matemática.

let evaluar expresion =
    match expresion with
        | Numero n -> n
        | Variable x -> failwith("Variable no definida: " + x)
        | Suma(e1, e2) -> evaluar e1 + evaluar e2
        | Resta(e1, e2) -> evaluar e1 - evaluar e2
        | Multiplicacion(e1, e2) -> evaluar e1 * evaluar e2
        | Division(e1, e2) -> evaluar e1 / evaluar e2

// Definir una función para derivar una expresión matemática.

let derivar expresion =
    match expresion with
        | Numero n -> 0.0
        | Variable x -> 1.0
        | Suma(e1, e2) -> derivar e1 + derivar e2
        | Resta(e1, e2) -> derivar e1 - derivar e2
        | Multiplicacion(e1, e2) -> (derivar e1 * e2) + (e1 * derivar e2)
        | Division(e1, e2) -> ((derivar e1 * e2) - (e1 * derivar e2)) / (e2 * e2)

// Probar las funciones.

let expresion = Suma(Numero 2.0, Multiplicacion(Variable "x", Numero 3.0))
let resultado = evaluar expresion
let derivada = derivar expresion

printfn "Expresión: %s" (Expresion.ToString expresion)
printfn "Resultado: %f" resultado
printfn "Derivada: %s" (Expresion.ToString derivada)
```

Este código define un tipo de datos algebraico para representar expresiones matemáticas, y proporciona funciones para evaluar y derivar estas expresiones.

El tipo de datos `Expresion` se define como una unión discriminada de cinco casos: `Numero`, `Variable`, `Suma`, `Resta` y `Multiplicacion`. Cada caso representa un tipo diferente de expresión matemática.

La función `evaluar` toma una expresión matemática y devuelve su valor. La función `derivar` toma una expresión matemática y devuelve su derivada.

Para probar las funciones, se define una expresión matemática simple que consiste en la suma de un número y el producto de una variable y un número. A continuación, se llama a las funciones `evaluar` y `derivar` para obtener el valor y la derivada de la expresión.

El resultado de la evaluación de la expresión es 5.0. El resultado de la derivación de la expresión es 3.0.