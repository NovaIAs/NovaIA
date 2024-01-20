```kotlin
// Definir una lista de números para trabajar
val numeros = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// Crear una función para calcular la suma de dos números
fun suma(a: Int, b: Int): Int {
    return a + b
}

// Crear una función para calcular el producto de dos números
fun producto(a: Int, b: Int): Int {
    return a * b
}

// Crear una función para calcular el factorial de un número
fun factorial(n: Int): Int {
    if (n == 0) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Crear una función para calcular el máximo común divisor de dos números
fun maximoComunDivisor(a: Int, b: Int): Int {
    if (b == 0) {
        return a
    } else {
        return maximoComunDivisor(b, a % b)
    }
}

// Crear una función para calcular el mínimo común múltiplo de dos números
fun minimoComunMultiplo(a: Int, b: Int): Int {
    return (a * b) / maximoComunDivisor(a, b)
}

// Crear una función para calcular la media aritmética de una lista de números
fun mediaAritmetica(lista: List<Int>): Double {
    var suma = 0.0
    for (numero in lista) {
        suma += numero
    }
    return suma / lista.size
}

// Crear una función para calcular la mediana de una lista de números
fun mediana(lista: List<Int>): Double {
    val listaOrdenada = lista.sorted()
    val mitad = listaOrdenada.size / 2
    return if (listaOrdenada.size % 2 == 0) {
        (listaOrdenada[mitad - 1] + listaOrdenada[mitad]) / 2.0
    } else {
        listaOrdenada[mitad]
    }
}

// Crear una función para calcular la moda de una lista de números
fun moda(lista: List<Int>): Int {
    val mapa = mutableMapOf<Int, Int>()
    for (numero in lista) {
        val frecuencia = mapa.getOrDefault(numero, 0)
        mapa[numero] = frecuencia + 1
    }
    var moda = 0
    var frecuenciaMaxima = 0
    for (numero in mapa.keys) {
        val frecuencia = mapa[numero]!!
        if (frecuencia > frecuenciaMaxima) {
            moda = numero
            frecuenciaMaxima = frecuencia
        }
    }
    return moda
}

// Crear una función para calcular la desviación estándar de una lista de números
fun desviacionEstandar(lista: List<Int>): Double {
    val media = mediaAritmetica(lista)
    var sumaCuadradosDiferencias = 0.0
    for (numero in lista) {
        val diferencia = numero - media
        sumaCuadradosDiferencias += diferencia * diferencia
    }
    val varianza = sumaCuadradosDiferencias / lista.size
    return Math.sqrt(varianza)
}

// Imprimir los resultados de las funciones en la consola
println("Suma de 3 y 4: ${suma(3, 4)}")
println("Producto de 3 y 4: ${producto(3, 4)}")
println("Factorial de 5: ${factorial(5)}")
println("Máximo común divisor de 12 y 18: ${maximoComunDivisor(12, 18)}")
println("Mínimo común múltiplo de 12 y 18: ${minimoComunMultiplo(12, 18)}")
println("Media aritmética de la lista [1, 2, 3, 4, 5]: ${mediaAritmetica(listOf(1, 2, 3, 4, 5))}")
println("Mediana de la lista [1, 2, 3, 4, 5]: ${mediana(listOf(1, 2, 3, 4, 5))}")
println("Moda de la lista [1, 2, 3, 4, 5, 1, 2, 3]: ${moda(listOf(1, 2, 3, 4, 5, 1, 2, 3))}")
println("Desviación estándar de la lista [1, 2, 3, 4, 5]: ${desviacionEstandar(listOf(1, 2, 3, 4, 5))}")
```

Explicación:

* La lista `numeros` contiene una serie de enteros del 1 al 10.
* Las funciones `suma`, `producto`, `factorial`, `maximoComunDivisor`, `minimoComunMultiplo`, `mediaAritmetica`, `mediana`, `moda` y `desviacionEstandar` calculan diferentes propiedades estadísticas de una lista de números.
* Las funciones `suma` y `producto` calculan la suma y el producto de dos números, respectivamente.
* La función `factorial` calcula el factorial de un número.
* La función `maximoComunDivisor` calcula el máximo común divisor de dos números.
* La función `minimoComunMultiplo` calcula el mínimo común múltiplo de dos números.
* La función `mediaAritmetica` calcula la media aritmética de una lista de números.
* La función `mediana` calcula la mediana de una lista de números.
* La función `moda` calcula la moda de una lista de números.
* La función `desviacionEstandar` calcula la desviación estándar de una lista de números.
* La función `println` se utiliza para imprimir los resultados de las funciones en la consola.