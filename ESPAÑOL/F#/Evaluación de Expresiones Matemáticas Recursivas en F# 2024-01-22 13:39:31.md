```f#
// Definición de un tipo de datos recursivo para representar expresiones matemáticas.
type Expresion =
    | Numero of float // Representa un número real.
    | Variable of string // Representa una variable.
    | Suma of Expresion * Expresion // Representa la suma de dos expresiones.
    | Resta of Expresion * Expresion // Representa la resta de dos expresiones.
    | Multiplicacion of Expresion * Expresion // Representa la multiplicación de dos expresiones.
    | Division of Expresion * Expresion // Representa la división de dos expresiones.

// Definición de una función para evaluar una expresión matemática.
let evaluar expresion =
    match expresion with
    | Numero n -> n // Si es un número, lo devuelve directamente.
    | Variable v -> failwith "Variable no definida" // Si es una variable, lanza una excepción.
    | Suma(e1, e2) -> evaluar e1 + evaluar e2 // Si es una suma, evalúa los operandos y suma los resultados.
    | Resta(e1, e2) -> evaluar e1 - evaluar e2 // Si es una resta, evalúa los operandos y resta los resultados.
    | Multiplicacion(e1, e2) -> evaluar e1 * evaluar e2 // Si es una multiplicación, evalúa los operandos y multiplica los resultados.
    | Division(e1, e2) -> evaluar e1 / evaluar e2 // Si es una división, evalúa los operandos y divide los resultados.

// Definición de una función para imprimir una expresión matemática en forma de cadena.
let rec imprimir expresion =
    match expresion with
    | Numero n -> n.ToString() // Si es un número, lo convierte a cadena.
    | Variable v -> v // Si es una variable, la devuelve directamente.
    | Suma(e1, e2) -> "(" + imprimir e1 + " + " + imprimir e2 + ")" // Si es una suma, imprime los operandos encerrados entre paréntesis.
    | Resta(e1, e2) -> "(" + imprimir e1 + " - " + imprimir e2 + ")" // Si es una resta, imprime los operandos encerrados entre paréntesis.
    | Multiplicacion(e1, e2) -> "(" + imprimir e1 + " * " + imprimir e2 + ")" // Si es una multiplicación, imprime los operandos encerrados entre paréntesis.
    | Division(e1, e2) -> "(" + imprimir e1 + " / " + imprimir e2 + ")" // Si es una división, imprime los operandos encerrados entre paréntesis.

// Ejemplo de uso de las funciones definidas anteriormente.
let expresion = Suma(Multiplicacion(Numero 2.0, Variable "x"), Resta(Numero 3.0, Numero 1.0)) // Definimos una expresión matemática.
let resultado = evaluar expresion // Evaluamos la expresión.
printfn "Resultado: %f" resultado // Imprimimos el resultado.
```

Explicación:

Este código define un tipo de datos recursivo llamado `Expresion` para representar expresiones matemáticas. El tipo de datos tiene seis constructores: `Numero`, `Variable`, `Suma`, `Resta`, `Multiplicacion` y `Division`. Estos constructores se utilizan para construir expresiones matemáticas a partir de números, variables y otras expresiones.

También define dos funciones llamadas `evaluar` e `imprimir`. La función `evaluar` toma una expresión matemática como argumento y devuelve su valor. La función `imprimir` toma una expresión matemática como argumento y devuelve una cadena que representa la expresión en forma de texto.

El ejemplo de uso al final del código muestra cómo definir una expresión matemática, evaluarla e imprimirla.