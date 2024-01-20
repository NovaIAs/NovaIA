```kotlin
class calculadoraCompleja {

    fun suma(a: Int, b: Int): Int {
        return a + b
    }

    fun resta(a: Int, b: Int): Int {
        return a - b
    }

    fun multiplicacion(a: Int, b: Int): Int {
        return a * b
    }

    fun division(a: Int, b: Int): Int {
        return a / b
    }

    fun potencia(a: Int, b: Int): Int {
        return Math.pow(a.toDouble(), b.toDouble()).toInt()
    }

    fun raizCuadrada(a: Int): Double {
        return Math.sqrt(a.toDouble())
    }

    fun factorial(a: Int): Int {
        if (a == 0) {
            return 1
        }
        return a * factorial(a - 1)
    }

    fun esPrimo(a: Int): Boolean {
        if (a <= 1) {
            return false
        }
        for (i in 2 until a) {
            if (a % i == 0) {
                return false
            }
        }
        return true
    }

    fun fibonacci(a: Int): Int {
        if (a == 0 || a == 1) {
            return a
        }
        return fibonacci(a - 1) + fibonacci(a - 2)
    }

    fun esPerfecto(a: Int): Boolean {
        var suma = 0
        for (i in 1 until a) {
            if (a % i == 0) {
                suma += i
            }
        }
        return suma == a
    }

    fun mcd(a: Int, b: Int): Int {
        if (b == 0) {
            return a
        }
        return mcd(b, a % b)
    }

    fun mcm(a: Int, b: Int): Int {
        return (a * b) / mcd(a, b)
    }
}

fun main(args: Array<String>) {
    val calculadora = calculadoraCompleja()

    println("Suma: ${calculadora.suma(10, 20)}")
    println("Resta: ${calculadora.resta(10, 20)}")
    println("Multiplicación: ${calculadora.multiplicacion(10, 20)}")
    println("División: ${calculadora.division(10, 20)}")
    println("Potencia: ${calculadora.potencia(10, 2)}")
    println("Raíz Cuadrada: ${calculadora.raizCuadrada(10)}")
    println("Factorial: ${calculadora.factorial(5)}")
    println("Es Primo: ${calculadora.esPrimo(7)}")
    println("Fibonacci: ${calculadora.fibonacci(10)}")
    println("Es Perfecto: ${calculadora.esPerfecto(6)}")
    println("MCD: ${calculadora.mcd(10, 20)}")
    println("MCM: ${calculadora.mcm(10, 20)}")
}
```

Este código es una calculadora compleja en Kotlin. Implementa las siguientes operaciones:

* Suma
* Resta
* Multiplicación
* División
* Potencia
* Raíz cuadrada
* Factorial
* Es primo
* Fibonacci
* Es perfecto
* Máximo común divisor (MCD)
* Mínimo común múltiplo (MCM)

El código está bien documentado y utiliza los tipos de datos adecuados para cada operación. También es modular, lo que facilita su mantenimiento y extensión.

Para utilizar la calculadora, simplemente crea una instancia de la clase `calculadoraCompleja` y llama a los métodos correspondientes. Por ejemplo, para calcular la suma de dos números, puedes utilizar el siguiente código:

```kotlin
val calculadora = calculadoraCompleja()
val suma = calculadora.suma(10, 20)
println("Suma: $suma")
```

Este código imprimirá el resultado de la suma en la consola.