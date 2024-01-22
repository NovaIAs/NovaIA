```kotlin
// Definimos una función para calcular el área de un círculo.
fun areaCirculo(radio: Double): Double {
    // Calculamos el área del círculo utilizando la fórmula πr².
    return Math.PI * radio * radio
}

// Definimos una función para calcular el volumen de un cilindro.
fun volumenCilindro(radio: Double, altura: Double): Double {
    // Calculamos el volumen del cilindro utilizando la fórmula πr²h.
    return Math.PI * radio * radio * altura
}

// Definimos una función para calcular la longitud de la hipotenusa de un triángulo rectángulo.
fun hipotenusa(lado1: Double, lado2: Double): Double {
    // Calculamos la longitud de la hipotenusa utilizando la fórmula √(a² + b²).
    return Math.sqrt(lado1 * lado1 + lado2 * lado2)
}

// Definimos una función para calcular la raíz cuadrada de un número.
fun raizCuadrada(numero: Double): Double {
    // Calculamos la raíz cuadrada del número utilizando la función Math.sqrt().
    return Math.sqrt(numero)
}

// Definimos una función para calcular el factorial de un número.
fun factorial(numero: Int): Int {
    // Calculamos el factorial del número utilizando la fórmula n!.
    var factorial = 1
    for (i in 1..numero) {
        factorial *= i
    }
    return factorial
}

// Definimos una función para calcular el máximo común divisor de dos números.
fun maximoComunDivisor(numero1: Int, numero2: Int): Int {
    // Calculamos el máximo común divisor de los dos números utilizando el algoritmo de Euclides.
    var resto = numero2
    var divisor = numero1
    while (resto != 0) {
        resto = divisor % numero2
        divisor = numero2
        numero2 = resto
    }
    return divisor
}

// Definimos una función para calcular el mínimo común múltiplo de dos números.
fun minimoComunMultiplo(numero1: Int, numero2: Int): Int {
    // Calculamos el mínimo común múltiplo de los dos números utilizando la fórmula mcd * mcm.
    var mcd = maximoComunDivisor(numero1, numero2)
    var mcm = (numero1 * numero2) / mcd
    return mcm
}

// Definimos una función para comprobar si un número es primo.
fun esPrimo(numero: Int): Boolean {
    // Comprobamos si el número es primo utilizando el algoritmo de Miller-Rabin.
    if (numero <= 1) {
        return false
    }
    if (numero <= 3) {
        return true
    }
    if (numero % 2 == 0 || numero % 3 == 0) {
        return false
    }
    var i = 5
    while (i * i <= numero) {
        if (numero % i == 0 || numero % (i + 2) == 0) {
            return false
        }
        i += 6
    }
    return true
}

// Definimos una función para generar una lista de números primos hasta un límite determinado.
fun generarPrimos(limite: Int): List<Int> {
    // Generamos una lista de números primos utilizando el algoritmo de la criba de Eratóstenes.
    val primos = mutableListOf<Int>()
    val numeros = BooleanArray(limite + 1)
    numeros[0] = true
    numeros[1] = true
    for (i in 2..limite) {
        if (!numeros[i]) {
            primos.add(i)
            for (j in i * i..limite step i) {
                numeros[j] = true
            }
        }
    }
    return primos
}

// Definimos una función para ordenar una lista de números en orden ascendente.
fun ordenarAscendente(lista: List<Int>): List<Int> {
    // Ordenamos la lista de números utilizando el algoritmo de clasificación por burbuja.
    var ordenada = lista.toMutableList()
    for (i in 0 until ordenada.size - 1) {
        for (j in 0 until ordenada.size - i - 1) {
            if (ordenada[j] > ordenada[j + 1]) {
                val temp = ordenada[j]
                ordenada[j] = ordenada[j + 1]
                ordenada[j + 1] = temp
            }
        }
    }
    return ordenada
}

// Definimos una función para ordenar una lista de números en orden descendente.
fun ordenarDescendente(lista: List<Int>): List<Int> {
    // Ordenamos la lista de números utilizando el algoritmo de clasificación por burbuja.
    var ordenada = lista.toMutableList()
    for