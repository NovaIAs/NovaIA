```f#

// Definición de un tipo de dato algebraico para representar expresiones matemáticas
type Expresion =
    | Número of float
    | Variable of string
    | Suma of Expresion * Expresion
    | Resta of Expresion * Expresión
    | Multiplicación of Expresión * Expresión
    | División of Expresión * Expresión

// Definición de una función recursiva para evaluar expresiones matemáticas
let evaluar expresion =
    match expresion with
    | Número n -> n
    | Variable v -> failwith "Variable no definida: " + v
    | Suma (e1, e2) -> evaluar e1 + evaluar e2
    | Resta (e1, e2) -> evaluar e1 - evaluar e2
    | Multiplicación (e1, e2) -> evaluar e1 * evaluar e2
    | División (e1, e2) -> evaluar e1 / evaluar e2

// Definición de una función para imprimir expresiones matemáticas en una forma legible
let imprimir expresion =
    match expresion with
    | Número n -> n.ToString()
    | Variable v -> v
    | Suma (e1, e2) -> "(" + imprimir e1 + " + " + imprimir e2 + ")"
    | Resta (e1, e2) -> "(" + imprimir e1 + " - " + imprimir e2 + ")"
    | Multiplicación (e1, e2) -> "(" + imprimir e1 + " * " + imprimir e2 + ")"
    | División (e1, e2) -> "(" + imprimir e1 + " / " + imprimir e2 + ")"

// Ejemplo de uso
let expresion = Suma (Número 2.5, Multiplicación (Número 3.0, Variable "x"))
let resultado = evaluar expresion
printfn "Expresión: %s" (imprimir expresion)
printfn "Resultado: %f" resultado

```

Explicación del código:

1. **Definición del tipo de dato algebraico `Expresión`**: Este tipo de dato representa expresiones matemáticas. Cada expresión se puede representar como un número, una variable, una suma, una resta, una multiplicación o una división.
2. **Definición de la función `evaluar`**: Esta función es recursiva y evalúa expresiones matemáticas. La función funciona de la siguiente manera:
    * Si la expresión es un número, entonces devuelve el número.
    * Si la expresión es una variable, entonces devuelve un error porque las variables no están definidas en el código.
    * Si la expresión es una suma, entonces devuelve la suma de los resultados de evaluar las subexpresiones izquierda y derecha.
    * Si la expresión es una resta, entonces devuelve la resta de los resultados de evaluar las subexpresiones izquierda y derecha.
    * Si la expresión es una multiplicación, entonces devuelve el producto de los resultados de evaluar las subexpresiones izquierda y derecha.
    * Si la expresión es una división, entonces devuelve el cociente de los resultados de evaluar las subexpresiones izquierda y derecha.
3. **Definición de la función `imprimir`**: Esta función imprime expresiones matemáticas en una forma legible. La función funciona de la siguiente manera:
    * Si la expresión es un número, entonces imprime el número.
    * Si la expresión es una variable, entonces imprime el nombre de la variable.
    * Si la expresión es una suma, entonces imprime la suma de las cadenas de caracteres que representan las subexpresiones izquierda y derecha.
    * Si la expresión es una resta, entonces imprime la resta de las cadenas de caracteres que representan las subexpresiones izquierda y derecha.
    * Si la expresión es una multiplicación, entonces imprime la multiplicación de las cadenas de caracteres que representan las subexpresiones izquierda y derecha.
    * Si la expresión es una división, entonces imprime la división de las cadenas de caracteres que representan las subexpresiones izquierda y derecha.
4. **Ejemplo de uso**: El código define una expresión matemática simple, luego evalúa la expresión y finalmente imprime la expresión y el resultado.