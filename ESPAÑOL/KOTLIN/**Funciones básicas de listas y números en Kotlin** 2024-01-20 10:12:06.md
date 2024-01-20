```kotlin
// Definimos una función para calcular el máximo común divisor (MCD) de dos números.
fun mcd(a: Int, b: Int): Int {
    // Si b es igual a 0, entonces el MCD es a.
    if (b == 0) {
        return a
    }

    // Si a es igual a 0, entonces el MCD es b.
    if (a == 0) {
        return b
    }

    // Mientras b no sea igual a 0, calculamos el resto de la división de a entre b y lo asignamos a a.
    while (b != 0) {
        val resto = a % b
        a = b
        b = resto
    }

    // Devolvemos el valor de a, que es el MCD de los dos números.
    return a
}

// Definimos una función para calcular el mínimo común múltiplo (MCM) de dos números.
fun mcm(a: Int, b: Int): Int {
    // Calculamos el producto de los dos números.
    val producto = a * b

    // Calculamos el MCD de los dos números.
    val mcd = mcd(a, b)

    // Devolvemos el producto dividido por el MCD, que es el MCM de los dos números.
    return producto / mcd
}

// Definimos una función para encontrar el valor máximo de una lista de números.
fun max(list: List<Int>): Int {
    // Definimos una variable para almacenar el valor máximo.
    var max = list[0]

    // Recorremos la lista y actualizamos el valor máximo si encontramos un número mayor que el valor máximo actual.
    for (number in list) {
        if (number > max) {
            max = number
        }
    }

    // Devolvemos el valor máximo.
    return max
}

// Definimos una función para encontrar el valor mínimo de una lista de números.
fun min(list: List<Int>): Int {
    // Definimos una variable para almacenar el valor mínimo.
    var min = list[0]

    // Recorremos la lista y actualizamos el valor mínimo si encontramos un número menor que el valor mínimo actual.
    for (number in list) {
        if (number < min) {
            min = number
        }
    }

    // Devolvemos el valor mínimo.
    return min
}

// Definimos una función para calcular la suma de los números de una lista.
fun sum(list: List<Int>): Int {
    // Definimos una variable para almacenar la suma.
    var sum = 0

    // Recorremos la lista y sumamos cada número a la suma.
    for (number in list) {
        sum += number
    }

    // Devolvemos la suma.
    return sum
}

// Definimos una función para calcular el promedio de los números de una lista.
fun average(list: List<Int>): Double {
    // Calculamos la suma de los números de la lista.
    val sum = sum(list)

    // Calculamos el número de elementos de la lista.
    val count = list.count()

    // Devolvemos la suma dividida por el número de elementos, que es el promedio.
    return sum / count
}

// Definimos una función para encontrar el elemento máximo de una lista de cadenas.
fun max(list: List<String>): String {
    // Definimos una variable para almacenar el elemento máximo.
    var max = list[0]

    // Recorremos la lista y actualizamos el elemento máximo si encontramos un elemento mayor que el elemento máximo actual.
    for (string in list) {
        if (string > max) {
            max = string
        }
    }

    // Devolvemos el elemento máximo.
    return max
}

// Definimos una función para encontrar el elemento mínimo de una lista de cadenas.
fun min(list: List<String>): String {
    // Definimos una variable para almacenar el elemento mínimo.
    var min = list[0]

    // Recorremos la lista y actualizamos el elemento mínimo si encontramos un elemento menor que el elemento mínimo actual.
    for (string in list) {
        if (string < min) {
            min = string
        }
    }

    // Devolvemos el elemento mínimo.
    return min
}

// Definimos una función para concatenar dos listas.
fun <T> concat(list1: List<T>, list2: List<T>): List<T> {
    // Creamos una lista mutable para almacenar la lista concatenada.
    val concatenatedList = mutableListOf<T>()

    // Agregamos todos los elementos de la primera lista a la lista concatenada.
    concatenatedList.addAll(list1)

    // Agregamos todos los elementos de la segunda lista a la lista concatenada.
    concatenatedList.addAll(list2)

    // Devolvemos la lista concatenada.
    return concatenatedList