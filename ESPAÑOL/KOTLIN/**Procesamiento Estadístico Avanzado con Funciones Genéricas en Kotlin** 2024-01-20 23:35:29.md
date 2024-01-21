```kotlin
// Código complejo en Kotlin

// Función para encontrar el elemento más grande en una lista
fun <T : Comparable<T>> encontrarMayor(lista: List<T>): T {
    var mayor = lista[0]
    for (elemento in lista) {
        if (elemento > mayor) {
            mayor = elemento
        }
    }
    return mayor
}

// Función para encontrar el elemento más pequeño en una lista
fun <T : Comparable<T>> encontrarMenor(lista: List<T>): T {
    var menor = lista[0]
    for (elemento in lista) {
        if (elemento < menor) {
            menor = elemento
        }
    }
    return menor
}

// Función para encontrar la media de una lista de números
fun encontrarMedia(lista: List<Double>): Double {
    var suma = 0.0
    for (numero in lista) {
        suma += numero
    }
    return suma / lista.size
}

// Función para encontrar la desviación estándar de una lista de números
fun encontrarDesviacionEstandar(lista: List<Double>): Double {
    var media = encontrarMedia(lista)
    var desviacionEstandar = 0.0
    for (numero in lista) {
        desviacionEstandar += Math.pow(numero - media, 2.0)
    }
    desviacionEstandar /= lista.size
    return Math.sqrt(desviacionEstandar)
}

// Función para encontrar la correlación entre dos listas de números
fun encontrarCorrelacion(lista1: List<Double>, lista2: List<Double>): Double {
    var media1 = encontrarMedia(lista1)
    var media2 = encontrarMedia(lista2)
    var desviacionEstandar1 = encontrarDesviacionEstandar(lista1)
    var desviacionEstandar2 = encontrarDesviacionEstandar(lista2)
    var correlacion = 0.0
    for (i in 0 until lista1.size) {
        correlacion += (lista1[i] - media1) * (lista2[i] - media2)
    }
    correlacion /= (lista1.size - 1) * desviacionEstandar1 * desviacionEstandar2
    return correlacion
}

// Función para encontrar la regresión lineal entre dos listas de números
fun encontrarRegresionLineal(lista1: List<Double>, lista2: List<Double>): DoubleArray {
    var media1 = encontrarMedia(lista1)
    var media2 = encontrarMedia(lista2)
    var desviacionEstandar1 = encontrarDesviacionEstandar(lista1)
    var desviacionEstandar2 = encontrarDesviacionEstandar(lista2)
    var correlacion = encontrarCorrelacion(lista1, lista2)
    var pendiente = correlacion * desviacionEstandar2 / desviacionEstandar1
    var intercepto = media2 - pendiente * media1
    return doubleArrayOf(pendiente, intercepto)
}

// Función principal
fun main(args: Array<String>) {
    // Lista de números
    val lista1 = listOf(1.0, 2.0, 3.0, 4.0, 5.0)
    val lista2 = listOf(1.0, 2.0, 3.0, 4.0, 5.0)

    // Imprimir el elemento más grande y el más pequeño de la lista
    println("Elemento más grande: ${encontrarMayor(lista1)}")
    println("Elemento más pequeño: ${encontrarMenor(lista1)}")

    // Imprimir la media y la desviación estándar de la lista
    println("Media: ${encontrarMedia(lista1)}")
    println("Desviación estándar: ${encontrarDesviacionEstandar(lista1)}")

    // Imprimir la correlación entre las dos listas
    println("Correlación: ${encontrarCorrelacion(lista1, lista2)}")

    // Imprimir la regresión lineal entre las dos listas
    val regresionLineal = encontrarRegresionLineal(lista1, lista2)
    println("Pendiente: ${regresionLineal[0]}")
    println("Intercepto: ${regresionLineal[1]}")
}
```

Este código es un ejemplo de código complejo en Kotlin. El código realiza una serie de operaciones matemáticas en dos listas de números, utilizando funciones genéricas. Las funciones genéricas permiten trabajar con diferentes tipos de datos, lo que hace que el código sea más versátil.

El código comienza definiendo una serie de funciones genéricas para encontrar el elemento más grande, el elemento más pequeño, la media, la desviación estándar, la correlación y la regresión lineal de una lista de números.

A continuación, se definen dos listas de números y se llaman a las funciones genéricas para realizar las operaciones matemáticas deseadas.

Por último, se imprimen los resultados de las operaciones matemáticas en la consola.

El código es complejo, pero está bien organizado y documentado, lo que lo hace fácil de entender y mantener.