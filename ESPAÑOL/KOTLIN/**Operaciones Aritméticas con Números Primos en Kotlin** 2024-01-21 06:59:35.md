```kotlin
// Función para encontrar el máximo común divisor de dos números.
fun mcd(a: Int, b: Int): Int {
    var a = a
    var b = b
    while (b != 0) {
        val tmp = a % b
        a = b
        b = tmp
    }
    return a
}

// Función para encontrar el mínimo común múltiplo de dos números.
fun mcm(a: Int, b: Int): Int {
    return (a * b) / mcd(a, b)
}

// Función para generar una lista de números primos hasta un límite especificado.
fun primosHasta(limite: Int): List<Int> {
    val primos = mutableListOf<Int>()
    primos.add(2)
    for (i in 3 until limite + 1 step 2) {
        var esPrimo = true
        for (j in primos) {
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

// Función para factorizar un número en sus factores primos.
fun factoresPrimos(numero: Int): List<Int> {
    val factores = mutableListOf<Int>()
    var numero = numero
    val primos = mutableListOf<Int>()
    var divisor = 2
    while (divisor <= numero) {
        if (numero % divisor == 0) {
            numero /= divisor
            factores.add(divisor)
            primos.add(divisor)
        } else {
            divisor++
        }
    }
    return factores
}

fun main(args: Array<String>) {
    // Demostrar mcd, mcm y descomposición en factores
    val a = 24
    val b = 18
    println("MCD de $a y $b: ${mcd(a, b)}")
    println("MCM de $a y $b: ${mcm(a, b)}")
    println("Factores primos de $a: ${factoresPrimos(a)}")

    // Demostrar primosHasta
    val limite = 100
    println("Primos hasta $limite: ${primosHasta(limite)}")
}
```

Explicación del código:

1. La función `mcd` calcula el máximo común divisor de dos números usando el algoritmo euclidiano.

2. La función `mcm` calcula el mínimo común múltiplo de dos números usando la fórmula `mcm = (a * b) / mcd(a, b)`.

3. La función `primosHasta` genera una lista de números primos hasta un límite especificado.

4. La función `factoresPrimos` factoriza un número en sus factores primos.

5. La función `main` demuestra el uso de las funciones `mcd`, `mcm`, `primosHasta` y `factoresPrimos`.