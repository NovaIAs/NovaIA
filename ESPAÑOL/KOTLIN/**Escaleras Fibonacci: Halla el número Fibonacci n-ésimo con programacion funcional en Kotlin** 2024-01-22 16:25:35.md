```kotlin
// Programación funcional: Escaleras de Fibonacci
// Definir una función que calcule el n-ésimo número de Fibonacci utilizando programación funcional
fun fibonacci(n: Int): Int = when (n) {
    0 -> 0
    1 -> 1
    else -> fibonacci(n - 1) + fibonacci(n - 2)
}

// Validación de entrada: Comprobar si el número es positivo
fun validarEntrada(n: Int) {
    if (n < 0) {
        throw IllegalArgumentException("El número debe ser positivo")
    }
}

// Interfaz de usuario: Obtener la entrada del usuario
fun obtenerEntrada(): Int {
    println("Ingrese el número de Fibonacci que desea calcular:")
    val input = readLine()!!.toInt()
    validarEntrada(input)
    return input
}

// Mostrar el resultado
fun mostrarResultado(n: Int, resultado: Int) {
    println("El $n-ésimo número de Fibonacci es $resultado")
}

// Main Function
fun main(args: Array<String>) {
    // Obtener la entrada del usuario
    val n = obtenerEntrada()

    // Calcular el n-ésimo número de Fibonacci
    val resultado = fibonacci(n)

    // Mostrar el resultado
    mostrarResultado(n, resultado)
}
```

Explicación:

1. `fibonacci(n)`: Esta función utilizando programación funcional calcula el n-ésimo número de Fibonacci de manera recursiva. Utiliza el patrón de Fibonacci, que define los dos primeros números como 0 y 1, y cada número posterior es la suma de los dos números anteriores.

2. `validarEntrada(n)`: Esta función valida la entrada del usuario para asegurarse de que el número es positivo. Si el número es negativo, se lanza una excepción `IllegalArgumentException`.

3. `obtenerEntrada()`: Esta función obtiene la entrada del usuario solicitando el número de Fibonacci que desea calcular.

4. `mostrarResultado(n, resultado)`: Esta función muestra el resultado al usuario, indicando el número de Fibonacci que se calculó y su valor.

5. En la función `main`, se llama a `obtenerEntrada()` para obtener la entrada del usuario y a `fibonacci(n)` para calcular el número de Fibonacci correspondiente. Finalmente, se muestra el resultado utilizando `mostrarResultado(n, resultado)`.