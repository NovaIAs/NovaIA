```kotlin
// Función para calcular el máximo común divisor de dos números usando el algoritmo de Euclides
fun mcm(a: Int, b: Int): Int {
    var num1 = a
    var num2 = b

    while (num2 != 0) {
        val resto = num1 % num2
        num1 = num2
        num2 = resto
    }

    return num1
}

// Función para generar una lista de números primos hasta un límite dado
fun primos(limite: Int): List<Int> {
    val primos = mutableListOf<Int>()

    for (i in 2..limite) {
        var esPrimo = true

        for (j in 2..i / 2) {
            if (i % j == 0) {
                esPrimo = false
                break
            }
        }

        if (esPrimo) {
            primos.add(i)
        }
    }

    return primos
}

// Función para calcular la factorización prima de un número
fun factorizacionPrima(numero: Int): List<Pair<Int, Int>> {
    val factores = mutableListOf<Pair<Int, Int>>()

    var num = numero
    var factor = 2

    while (num > 1) {
        if (num % factor == 0) {
            var cuenta = 0

            while (num % factor == 0) {
                num /= factor
                cuenta++
            }

            factores.add(Pair(factor, cuenta))
        }

        factor++
    }

    return factores
}

// Función para calcular el mínimo común múltiplo de dos números usando sus factorizaciones primas
fun mcmFactorizacionPrima(a: Int, b: Int): Int {
    val factoresA = factorizacionPrima(a)
    val factoresB = factorizacionPrima(b)

    val factoresComunes = mutableListOf<Pair<Int, Int>>()

    for (factorA in factoresA) {
        for (factorB in factoresB) {
            if (factorA.first == factorB.first) {
                factoresComunes.add(Pair(factorA.first, max(factorA.second, factorB.second)))
            }
        }
    }

    var mcm = 1

    for (factor in factoresComunes) {
        mcm *= factor.first.toInt().pow(factor.second)
    }

    return mcm
}

// Función para calcular la raíz cuadrada de un número usando el método de Newton-Raphson
fun raizCuadrada(numero: Double, precision: Double = 0.001): Double {
    var x0 = numero / 2

    while (Math.abs(x0 * x0 - numero) > precision) {
        x0 = (x0 + numero / x0) / 2
    }

    return x0
}

// Función para calcular el área de un triángulo usando la fórmula de Herón
fun areaTriangulo(a: Double, b: Double, c: Double): Double {
    val s = (a + b + c) / 2
    return Math.sqrt(s * (s - a) * (s - b) * (s - c))
}

// Función para calcular la probabilidad de un evento binomial usando la distribución binomial
fun probabilidadBinomial(n: Int, k: Int, p: Double): Double {
    return Math.pow(p, k.toDouble()) * Math.pow(1 - p, (n - k).toDouble()) * factorial(n) / (factorial(k) * factorial(n - k))
}

// Función para calcular el factorial de un número
fun factorial(n: Int): Int {
    var factorial = 1

    for (i in 2..n) {
        factorial *= i
    }

    return factorial
}

// Función para calcular la media de una lista de números
fun media(numeros: List<Double>): Double {
    var suma = 0.0

    for (numero in numeros) {
        suma += numero
    }

    return suma / numeros.size
}

// Función para calcular la desviación estándar de una lista de números
fun desviacionEstandar(numeros: List<Double>): Double {
    val media = media(numeros)

    var varianza = 0.0

    for (numero in numeros) {
        varianza += Math.pow(numero - media, 2.0)
    }

    varianza /= numeros.size

    return Math.sqrt(varianza)
}

// Función para calcular la mediana de una lista de números
fun mediana(numeros: List<Double>): Double {
    val numerosOrdenados = numeros.sorted()

    return if (numerosOrdenados.size % 2 == 0) {
        (numerosOrdenados[numerosOrdenados.size / 2] + numerosOrdenados[numerosOrdenados.size / 2 - 1]) / 2
    } else {
        numerosOrdenados[numerosOrdenados.size / 2]
    }
}

// Función para calcular la moda de una lista de números
fun moda(numeros: List<Double>): List<Double> {
    val frecuencias = mutableMapOf<Double, Int>()

    for (numero in numeros) {
        val frecuencia = frecuencias.getOrDefault(numero, 0) + 1
        frecuencias[numero] = frecuencia
    }

    val moda = frecuencias.maxByOrNull { it.value }!!.key

    return frecuencias.filter { it.value == moda.value }.map { it.key }
}
```

Este código contiene una colección de funciones matemáticas útiles escritas en Kotlin. Las funciones cubren una variedad de temas, incluyendo teoría de números, álgebra, probabilidad y estadística. Cada función está documentada con una breve descripción de lo que hace y cómo se usa.

Algunas de las funciones más notables son:

* `mcm()`: Calcula el máximo común divisor de dos números usando el algoritmo de Euclides.
* `primos()`: Genera una lista de números primos hasta un límite dado.
* `factorizacionPrima()`: Calcula la factorización prima de un número.
* `mcmFactorizacionPrima()`: Calcula el mínimo común múltiplo de dos números usando sus factorizaciones primas.
* `raizCuadrada()`: Calcula la raíz cuadrada de un número usando el método de Newton-Raphson.
* `areaTriangulo()`: Calcula el área de un triángulo usando la fórmula de Herón.
* `probabilidadBinomial()`: Calcula la probabilidad de un evento binomial usando la distribución binomial.
* `factorial()`: Calcula el factorial de un número.
* `media()`: Calcula la media de una lista de números.
* `desviacionEstandar()`: Calcula la desviación estándar de una lista de números.
* `mediana()`: Calcula la mediana de una lista de números.
* `moda()`: Calcula la moda de una lista de números.

Este código es un recurso valioso para cualquier programador que trabaje con matemáticas en Kotlin. Puede usarse para resolver una variedad de problemas, desde cálculos básicos hasta análisis estadístico avanzado.