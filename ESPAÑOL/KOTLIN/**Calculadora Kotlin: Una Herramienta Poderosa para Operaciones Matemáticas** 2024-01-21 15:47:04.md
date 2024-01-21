```kotlin
// Clase que representa una calculadora
class Calculadora {

    // Método para sumar dos números
    fun sumar(a: Int, b: Int): Int {
        return a + b
    }

    // Método para restar dos números
    fun restar(a: Int, b: Int): Int {
        return a - b
    }

    // Método para multiplicar dos números
    fun multiplicar(a: Int, b: Int): Int {
        return a * b
    }

    // Método para dividir dos números
    fun dividir(a: Int, b: Int): Double {
        return a / b.toDouble()
    }

    // Método para calcular el factorial de un número
    fun factorial(n: Int): Int {
        if (n == 0) {
            return 1
        }
        return n * factorial(n - 1)
    }

    // Método para calcular el máximo común divisor de dos números
    fun mcd(a: Int, b: Int): Int {
        if (b == 0) {
            return a
        }
        return mcd(b, a % b)
    }

    // Método para calcular el mínimo común múltiplo de dos números
    fun mcm(a: Int, b: Int): Int {
        return (a * b) / mcd(a, b)
    }
}

// Función principal que crea una instancia de la calculadora y la utiliza para realizar algunas operaciones
fun main(args: Array<String>) {
    val calculadora = Calculadora()

    // Suma de dos números
    val suma = calculadora.sumar(10, 20)
    println("La suma de 10 y 20 es: $suma")

    // Resta de dos números
    val resta = calculadora.restar(20, 10)
    println("La resta de 20 y 10 es: $resta")

    // Multiplicación de dos números
    val multiplicacion = calculadora.multiplicar(10, 20)
    println("La multiplicación de 10 y 20 es: $multiplicacion")

    // División de dos números
    val division = calculadora.dividir(20, 10)
    println("La división de 20 entre 10 es: $division")

    // Factorial de un número
    val factorial = calculadora.factorial(5)
    println("El factorial de 5 es: $factorial")

    // Máximo común divisor de dos números
    val mcd = calculadora.mcd(10, 20)
    println("El máximo común divisor de 10 y 20 es: $mcd")

    // Mínimo común múltiplo de dos números
    val mcm = calculadora.mcm(10, 20)
    println("El mínimo común múltiplo de 10 y 20 es: $mcm")
}
```

Este código es una implementación de una calculadora en Kotlin. La calculadora tiene varios métodos para realizar operaciones matemáticas básicas, como sumar, restar, multiplicar y dividir. También tiene métodos para calcular el factorial de un número, el máximo común divisor de dos números y el mínimo común múltiplo de dos números.

La función principal del código crea una instancia de la calculadora y la utiliza para realizar algunas operaciones. Estas operaciones se imprimen en la consola.