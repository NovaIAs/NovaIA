```kotlin
// Clase principal del programa
class ProgramaPrincipal {

    // Función principal del programa
    fun main(args: Array<String>) {

        // Crear una lista de números enteros
        val numeros = listOf(1, 2, 3, 4, 5)

        // Crear un mapa de claves y valores
        val mapa = mapOf("uno" to 1, "dos" to 2, "tres" to 3)

        // Crear una función que devuelva el cuadrado de un número
        val cuadrado: (Int) -> Int = { numero -> numero * numero }

        // Crear una función que devuelva el doble de un número
        val doble: (Int) -> Int = { numero -> numero * 2 }

        // Aplicar la función cuadrado a cada elemento de la lista
        val cuadrados = numeros.map(cuadrado)

        // Aplicar la función doble a cada elemento de la lista
        val dobles = numeros.map(doble)

        // Filtrar los elementos de la lista que sean mayores que 2
        val mayoresQueDos = numeros.filter { numero -> numero > 2 }

        // Ordenar la lista en orden ascendente
        val ordenadosAscendente = numeros.sorted()

        // Ordenar la lista en orden descendente
        val ordenadosDescendente = numeros.sortedDescending()

        // Imprimir los resultados
        println("Cuadrados: $cuadrados")
        println("Dobles: $dobles")
        println("Mayores que dos: $mayoresQueDos")
        println("Ordenados ascendente: $ordenadosAscendente")
        println("Ordenados descendente: $ordenadosDescendente")
    }
}
```

**Explicación del código:**

* La clase `ProgramaPrincipal` es la clase principal del programa.
* La función `main` es la función principal del programa.
* La línea `val numeros = listOf(1, 2, 3, 4, 5)` crea una lista de números enteros.
* La línea `val mapa = mapOf("uno" to 1, "dos" to 2, "tres" to 3)` crea un mapa de claves y valores.
* La línea `val cuadrado: (Int) -> Int = { numero -> numero * numero }` crea una función que devuelve el cuadrado de un número.
* La línea `val doble: (Int) -> Int = { numero -> numero * 2 }` crea una función que devuelve el doble de un número.
* La línea `val cuadrados = numeros.map(cuadrado)` aplica la función cuadrado a cada elemento de la lista.
* La línea `val dobles = numeros.map(doble)` aplica la función doble a cada elemento de la lista.
* La línea `val mayoresQueDos = numeros.filter { numero -> numero > 2 }` filtra los elementos de la lista que sean mayores que 2.
* La línea `val ordenadosAscendente = numeros.sorted()` ordena la lista en orden ascendente.
* La línea `val ordenadosDescendente = numeros.sortedDescending()` ordena la lista en orden descendente.
* Las líneas `println("Cuadrados: $cuadrados")`, `println("Dobles: $dobles")`, `println("Mayores que dos: $mayoresQueDos")`, `println("Ordenados ascendente: $ordenadosAscendente")`, `println("Ordenados descendente: $ordenadosDescendente")` imprimen los resultados.