```kotlin
// **Función Principal**
fun main(args: Array<String>) {
    // **Lista de Números**
    val numeros = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // **Función para Filtrar la Lista**
    val numerosFiltrados = numeros.filter { it % 2 == 0 }

    // **Función para Mapear la Lista Filtrada**
    val numerosElevadosAlCuadrado = numerosFiltrados.map { it * it }

    // **Función para Reducir la Lista Mapeada**
    val sumaDeNumerosElevadosAlCuadrado = numerosElevadosAlCuadrado.reduce { acc, i -> acc + i }

    // **Función para Imprimir el Resultado**
    println(sumaDeNumerosElevadosAlCuadrado)
}
```

**Explicación:**

* La función principal `main` es el punto de entrada del programa.

* La lista `numeros` contiene una lista de números del 1 al 10.

* La función `filter` filtra la lista de números para obtener sólo los números pares.

* La función `map` mapea la lista de números pares para elevar cada número al cuadrado.

* La función `reduce` reduce la lista mapeada a un solo valor, que es la suma de todos los números elevados al cuadrado.

* La función `println` imprime el resultado final en la consola.