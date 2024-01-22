```kotlin
// Función para calcular el factorial de un número
fun factorial(n: Int): Int {
    // Caso base: el factorial de 0 es 1
    if (n == 0) {
        return 1
    }
    // Caso recursivo: el factorial de un número es igual al número multiplicado por el factorial del número anterior
    else {
        return n * factorial(n - 1)
    }
}

// Función para calcular la combinación de n elementos tomados de k en k
fun combinacion(n: Int, k: Int): Int {
    // Caso base: La combinación de n elementos tomados de 0 o n es igual a 1
    if (k == 0 || k == n) {
        return 1
    }
    // Caso recursivo: La combinación de n elementos tomados de k es igual a la combinación de n-1 elementos tomados de k-1 más la combinación de n-1 elementos tomados de k
    else {
        return combinacion(n-1, k-1) + combinacion(n-1, k)
    }
}

// Función para calcular la permutación de n elementos tomados de k en k
fun permutacion(n: Int, k: Int): Int {
    // Caso base: La permutación de n elementos tomados de 0 o n es igual a 1
    if (k == 0 || k == n) {
        return 1
    }
    // Caso recursivo: La permutación de n elementos tomados de k es igual a la permutación de n-1 elementos tomados de k-1 multiplicada por n
    else {
        return permutacion(n-1, k-1) * n
    }
}

// Función para calcular la suma de los dígitos de un número entero positivo
fun sumaDigitos(n: Int): Int {
    // Caso base: Si el número es 0, la suma de sus dígitos es 0
    if (n == 0) {
        return 0
    }
    // Caso recursivo: La suma de los dígitos de un número es igual a la suma de los dígitos del número sin el último dígito más el último dígito
    else {
        return sumaDigitos(n / 10) + n % 10
    }
}

// Función para calcular el máximo común divisor de dos números enteros positivos usando el algoritmo de Euclides
fun maximoComunDivisor(a: Int, b: Int): Int {
    // Caso base: Si b es 0, el máximo común divisor es a
    if (b == 0) {
        return a
    }
    // Caso recursivo: El máximo común divisor de a y b es igual al máximo común divisor de b y el resto de la división de a entre b
    else {
        return maximoComunDivisor(b, a % b)
    }
}

// Función para calcular el mínimo común múltiplo de dos números enteros positivos
fun minimoComunMultiplo(a: Int, b: Int): Int {
    // El mínimo común múltiplo de a y b es igual al producto de a y b dividido por el máximo común divisor de a y b
    return (a * b) / maximoComunDivisor(a, b)
}

// Función para comprobar si un número entero positivo es primo
fun esPrimo(n: Int): Boolean {
    // Caso base: 1 no es primo
    if (n == 1) {
        return false
    }
    // Caso base: 2 es primo
    if (n == 2) {
        return true
    }
    // Caso recursivo: Un número es primo si no es divisible por ningún número entre 2 y la raíz cuadrada del número
    for (i in 2..Math.sqrt(n.toDouble()).toInt()) {
        if (n % i == 0) {
            return false
        }
    }
    return true
}

// Función para encontrar todos los números primos entre 1 y n
fun encontrarPrimos(n: Int): List<Int> {
    // Crear una lista vacía para almacenar los números primos
    val primos = mutableListOf<Int>()
    // Iterar sobre todos los números entre 2 y n
    for (i in 2..n) {
        // Si el número es primo, añadirlo a la lista
        if (esPrimo(i)) {
            primos.add(i)
        }
    }
    // Devolver la lista de números primos
    return primos
}
```

Este código contiene varias funciones matemáticas útiles, incluyendo:

* Función para calcular el factorial de un número.
* Función para calcular la combinación de n elementos tomados de k en k.
* Función para calcular la permutación de n elementos tomados de k en k.
* Función para calcular la suma de los dígitos de un número entero positivo.
* Función para calcular el máximo común divisor de dos números enteros positivos.
* Función para calcular el mínimo común múltiplo de dos números enteros positivos.
* Función para comprobar si un número entero positivo es primo.
* Función para encontrar todos los números primos entre 1 y n.

Estas funciones se pueden utilizar para resolver una amplia variedad de problemas matemáticos.