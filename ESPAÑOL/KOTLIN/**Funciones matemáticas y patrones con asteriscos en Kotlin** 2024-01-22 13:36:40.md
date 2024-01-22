```kotlin
// Función para calcular el factorial de un número
fun factorial(n: Int): Int {
    if (n == 0) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Función para imprimir los números primos hasta un número dado
fun imprimirPrimos(n: Int) {
    for (i in 2..n) {
        var esPrimo = true
        for (j in 2 until i) {
            if (i % j == 0) {
                esPrimo = false
                break
            }
        }
        if (esPrimo) {
            print("$i ")
        }
    }
    println()
}

// Función para imprimir los números de Fibonacci hasta un número dado
fun imprimirFibonacci(n: Int) {
    var a = 0
    var b = 1
    for (i in 1..n) {
        print("$a ")
        val c = a + b
        a = b
        b = c
    }
    println()
}

// Función para imprimir el patrón de triángulo de asteriscos
fun imprimirTrianguloAsteriscos(n: Int) {
    for (i in 1..n) {
        for (j in 1..i) {
            print("* ")
        }
        println()
    }
}

// Función para imprimir el patrón de diamante de asteriscos
fun imprimirDiamanteAsteriscos(n: Int) {
    // Imprimir la mitad superior del diamante
    for (i in 1..n) {
        for (j in 1..(n - i)) {
            print("  ")
        }
        for (j in 1..(2 * i - 1)) {
            print("* ")
        }
        println()
    }
    // Imprimir la mitad inferior del diamante
    for (i in (n - 1) downTo 1) {
        for (j in 1..(n - i)) {
            print("  ")
        }
        for (j in 1..(2 * i - 1)) {
            print("* ")
        }
        println()
    }
}

// Función para imprimir el patrón de árbol de Navidad de asteriscos
fun imprimirArbolNavidadAsteriscos(n: Int) {
    // Imprimir el tronco del árbol
    for (i in 1..(n - 1)) {
        for (j in 1..(n - i)) {
            print("  ")
        }
        print("*")
        println()
    }
    // Imprimir las ramas del árbol
    for (i in 1..n) {
        for (j in 1..(n - i)) {
            print("  ")
        }
        for (j in 1..(2 * i - 1)) {
            print("* ")
        }
        println()
    }
}

// Función principal
fun main(args: Array<String>) {
    // Calcular y mostrar el factorial de un número
    println("Factorial de 5: ${factorial(5)}")

    // Imprimir los números primos hasta un número dado
    println("Números primos hasta 100:")
    imprimirPrimos(100)

    // Imprimir los números de Fibonacci hasta un número dado
    println("Números de Fibonacci hasta 10:")
    imprimirFibonacci(10)

    // Imprimir el patrón de triángulo de asteriscos
    println("Patrón de triángulo de asteriscos:")
    imprimirTrianguloAsteriscos(5)

    // Imprimir el patrón de diamante de asteriscos
    println("Patrón de diamante de asteriscos:")
    imprimirDiamanteAsteriscos(5)

    // Imprimir el patrón de árbol de Navidad de asteriscos
    println("Patrón de árbol de Navidad de asteriscos:")
    imprimirArbolNavidadAsteriscos(5)
}
```

**Explicación del código:**

Este código contiene varias funciones para realizar diferentes tareas:

1. **`factorial`**: Esta función calcula el factorial de un número dado. El factorial de un número `n` se define como el producto de todos los números enteros positivos menores o iguales a `n`. Por ejemplo, el factorial de 5 es `5 * 4 * 3 * 2 * 1 = 120`.


2. **`imprimirPrimos`**: Esta función imprime los números primos hasta un número dado. Un número primo es un número entero mayor que 1 que no se puede escribir como el producto de dos números enteros más pequeños. Por ejemplo, los primeros 10 números primos son 2, 3, 5, 7, 11, 13, 17, 19, 23 y 29.


3. **`imprimirFibonacci`**: Esta función imprime los números de Fibonacci hasta un número dado. Los números de Fibonacci son una secuencia de números en la que cada número es la suma de los dos números anteriores. Por ejemplo, los primeros 10 números de Fibonacci son 0, 1, 1, 2, 3, 5, 8, 13, 21 y 34.


4. **`imprimirTrianguloAsteriscos`**: Esta función imprime un patrón de triángulo de asteriscos. El triángulo tendrá `n` filas, y cada fila tendrá `i` asteriscos, donde `i` es el número de fila. Por ejemplo, el siguiente es un triángulo de asteriscos de 5 filas:

    ```
    *
    **
    ***
    ****
    *****
    ```


5. **`imprimirDiamanteAsteriscos`**: Esta función imprime un patrón de diamante de asteriscos. El diamante tendrá `n` filas, y cada fila tendrá `i` asteriscos, donde `i` es el número de fila. El diamante se verá simétrico, con una fila de asteriscos en el medio. Por ejemplo, el siguiente es un diamante de asteriscos de 5 filas:

    ```
      *
     ***
    *****
   *******
  *********
   *******
    *****
     ***
      *
    ```


6. **`imprimirArbolNavidadAsteriscos`**: Esta función imprime un patrón de árbol de Navidad de asteriscos. El árbol tendrá `n` filas, y cada fila tendrá `i` asteriscos, donde `i` es el número de fila. El árbol se verá como un triángulo, con una fila de asteriscos en la parte superior y una fila de asteriscos en la parte inferior. Por ejemplo, el siguiente es un árbol de Navidad de asteriscos de 5 filas:

    ```
        *
       ***
      *****
     *******
    *********
       ***
      *****
     *******
    *********
    ```

La función `main` del programa llama a cada una de estas funciones para demostrar su funcionalidad.