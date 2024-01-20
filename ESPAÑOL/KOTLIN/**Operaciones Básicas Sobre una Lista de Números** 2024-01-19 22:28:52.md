```kotlin
// Creamos la clase Principal.
class Principal {

    // Creamos una lista de números enteros.
    val listaNumeros = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // Creamos una función para imprimir la lista de números.
    fun imprimirListaNumeros() {
        println("La lista de números es:")
        for (numero in listaNumeros) {
            println(numero)
        }
    }

    // Creamos una función para encontrar el número mayor de la lista.
    fun encontrarNumeroMayor(): Int {
        var numeroMayor = listaNumeros[0]
        for (numero in listaNumeros) {
            if (numero > numeroMayor) {
                numeroMayor = numero
            }
        }
        return numeroMayor
    }

    // Creamos una función para encontrar el número menor de la lista.
    fun encontrarNumeroMenor(): Int {
        var numeroMenor = listaNumeros[0]
        for (numero in listaNumeros) {
            if (numero < numeroMenor) {
                numeroMenor = numero
            }
        }
        return numeroMenor
    }

    // Creamos una función para calcular la suma de los números de la lista.
    fun calcularSumaNumeros(): Int {
        var sumaNumeros = 0
        for (numero in listaNumeros) {
            sumaNumeros += numero
        }
        return sumaNumeros
    }

    // Creamos una función para calcular el promedio de los números de la lista.
    fun calcularPromedioNumeros(): Double {
        var sumaNumeros = 0
        for (numero in listaNumeros) {
            sumaNumeros += numero
        }
        return sumaNumeros / listaNumeros.size.toDouble()
    }

    // Creamos una función para crear un mapa que contenga los números de la lista como claves y sus cuadrados como valores.
    fun crearMapaNumerosCuadrados(): Map<Int, Int> {
        val mapaNumerosCuadrados = mutableMapOf<Int, Int>()
        for (numero in listaNumeros) {
            mapaNumerosCuadrados[numero] = numero * numero
        }
        return mapaNumerosCuadrados
    }
}

// Creamos un objeto de la clase Principal.
val principal = Principal()

// Imprimimos la lista de números.
principal.imprimirListaNumeros()

// Encontramos el número mayor de la lista.
val numeroMayor = principal.encontrarNumeroMayor()
println("El número mayor de la lista es: $numeroMayor")

// Encontramos el número menor de la lista.
val numeroMenor = principal.encontrarNumeroMenor()
println("El número menor de la lista es: $numeroMenor")

// Calculamos la suma de los números de la lista.
val sumaNumeros = principal.calcularSumaNumeros()
println("La suma de los números de la lista es: $sumaNumeros")

// Calculamos el promedio de los números de la lista.
val promedioNumeros = principal.calcularPromedioNumeros()
println("El promedio de los números de la lista es: $promedioNumeros")

// Creamos un mapa que contenga los números de la lista como claves y sus cuadrados como valores.
val mapaNumerosCuadrados = principal.crearMapaNumerosCuadrados()
println("El mapa de números y cuadrados es:")
for (entrada in mapaNumerosCuadrados) {
    println("${entrada.key} -> ${entrada.value}")
}
```

**Explicación del código:**

1. Creamos la clase `Principal` que contiene una lista de números enteros y diversas funciones para manipular y procesar la lista.
2. En la función `imprimirListaNumeros()` iteramos sobre la lista de números y los imprimimos en la consola.
3. En la función `encontrarNumeroMayor()` iteramos sobre la lista de números y mantenemos un registro del número mayor encontrado hasta ahora. Devolvemos el número mayor al final de la función.
4. En la función `encontrarNumeroMenor()` iteramos sobre la lista de números y mantenemos un registro del número menor encontrado hasta ahora. Devolvemos el número menor al final de la función.
5. En la función `calcularSumaNumeros()` iteramos sobre la lista de números y mantenemos un registro de la suma de los números encontrados hasta ahora. Devolvemos la suma total al final de la función.
6. En la función `calcularPromedioNumeros()` primero calculamos la suma de los números de la lista utilizando la función `calcularSumaNumeros()`. Luego dividimos la suma por el número total de elementos en la lista para obtener el promedio.
7. En la función `crearMapaNumerosCuadrados()` creamos un mapa mutable que contiene los números de la lista como claves y sus cuadrados como valores. Iteramos sobre la lista de números y añadimos cada número y su cuadrado al mapa. Devolvemos el mapa al final de la función.
8. Creamos un objeto de la clase `Principal` llamado `principal`.
9. Llamamos a la función `imprimirListaNumeros()` para imprimir la lista de números.
10. Llamamos a la función `encontrarNumeroMayor()` para encontrar el número mayor de la lista y lo imprimimos en la consola.
11. Llamamos a la función `encontrarNumeroMenor()` para encontrar el número menor de la lista y lo imprimimos en la consola.
12. Llamamos a la función `calcularSumaNumeros()` para calcular la suma de los números de la lista y la imprimimos en la consola.
13. Llamamos a la función `calcularPromedioNumeros()` para calcular el promedio de los números de la lista y lo imprimimos en la consola.
14. Llamamos a la función `crearMapaNumerosCuadrados()` para crear un mapa que contenga los números de la lista como claves y sus cuadrados como valores. Iteramos sobre el mapa y lo imprimimos en la consola.