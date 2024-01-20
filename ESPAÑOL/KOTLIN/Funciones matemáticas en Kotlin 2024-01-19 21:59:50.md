```kotlin
// Importamos las librerías necesarias
import java.util.*

// Definimos una función para generar un número aleatorio entre dos valores
fun generarNumeroAleatorio(min: Int, max: Int): Int {
    val random = Random()
    return random.nextInt(max - min + 1) + min
}

// Definimos una función para verificar si un número es primo
fun esPrimo(numero: Int): Boolean {
    if (numero <= 1) {
        return false
    }

    for (i in 2 until numero) {
        if (numero % i == 0) {
            return false
        }
    }

    return true
}

// Definimos una función para encontrar todos los números primos entre dos valores
fun encontrarPrimosEntre(min: Int, max: Int): List<Int> {
    val primos = mutableListOf<Int>()

    for (i in min..max) {
        if (esPrimo(i)) {
            primos.add(i)
        }
    }

    return primos
}

// Definimos una función para calcular el factorial de un número
fun factorial(numero: Int): Int {
    if (numero == 0) {
        return 1
    }

    var factorial = 1

    for (i in 1..numero) {
        factorial *= i
    }

    return factorial
}

// Definimos una función para calcular el número de combinaciones de n elementos tomados de m en m
fun combinaciones(n: Int, m: Int): Int {
    if (n < 0 || m < 0 || n > m) {
        return 0
    }

    return factorial(m) / (factorial(n) * factorial(m - n))
}

// Definimos una función para calcular el número de permutaciones de n elementos tomados de m en m
fun permutaciones(n: Int, m: Int): Int {
    if (n < 0 || m < 0 || n > m) {
        return 0
    }

    return factorial(m) / factorial(m - n)
}

// Definimos una función para calcular el mayor común divisor de dos números
fun mcd(a: Int, b: Int): Int {
    var resto = a % b

    while (resto != 0) {
        a = b
        b = resto
        resto = a % b
    }

    return b
}

// Definimos una función para calcular el mínimo común múltiplo de dos números
fun mcm(a: Int, b: Int): Int {
    return (a * b) / mcd(a, b)
}

// Definimos una función para calcular la raíz cuadrada de un número
fun raizCuadrada(numero: Double): Double {
    if (numero < 0) {
        return Double.NaN
    }

    var raiz = numero / 2

    while (Math.abs(raiz * raiz - numero) > 0.001) {
        raiz = (raiz + numero / raiz) / 2
    }

    return raiz
}

// Definimos una función para calcular la potencia de un número
fun potencia(base: Double, exponente: Int): Double {
    if (exponente == 0) {
        return 1.0
    }

    var resultado = base

    for (i in 1 until exponente) {
        resultado *= base
    }

    return resultado
}

// Definimos una función para calcular el logaritmo de un número
fun logaritmo(numero: Double, base: Double): Double {
    if (numero <= 0 || base <= 0 || base == 1.0) {
        return Double.NaN
    }

    return Math.log(numero) / Math.log(base)
}

// Definimos una función para calcular el seno de un ángulo
fun seno(angulo: Double): Double {
    return Math.sin(angulo)
}

// Definimos una función para calcular el coseno de un ángulo
fun coseno(angulo: Double): Double {
    return Math.cos(angulo)
}

// Definimos una función para calcular la tangente de un ángulo
fun tangente(angulo: Double): Double {
    return Math.tan(angulo)
}

// Definimos una función para calcular la secante de un ángulo
fun secante(angulo: Double): Double {
    return 1 / coseno(angulo)
}

// Definimos una función para calcular la cosecante de un ángulo
fun cosecante(angulo: Double): Double {
    return 1 / seno(angulo)
}

// Definimos una función para calcular la cotangente de un ángulo
fun cotangente(angulo: Double): Double {
    return 1 / tangente(angulo)
}

// Definimos una función para calcular el área de un triángulo
fun areaTriangulo(base: Double, altura: Double): Double {
    return 0.5 * base * altura
}

// Definimos una función para calcular el perímetro de un triángulo
fun perimetroTriangulo(lado1: Double, lado2: Double, lado3: Double): Double {
    return lado1 + lado2 + lado3
}

// Definimos una función para calcular el volumen de un cubo
fun volumenCubo(lado: Double): Double {
    return lado * lado * lado
}

// Definimos una función para calcular la superficie de un cubo
fun superficieCubo(lado: Double): Double {
    return 6 * lado * lado
}

// Definimos una función para calcular el área de un círculo
fun areaCirculo(radio: Double